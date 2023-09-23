library(tidyverse)
library(rvest)

season_nums <- as.character(seq(1,18))

# initialize dataframes 
week_names_df <- data.frame(matrix(ncol = 3, nrow = 0))
week_desc_df <- data.frame(matrix(ncol = 3, nrow = 0))

rating_df <- data.frame(matrix(ncol = 17, nrow = 0))
colnames(rating_df) <- c("airdate", "show_number", "episode", "18_49_rating_share", "rating_share","rating_share_households", "share", "dvr_18_49","dvr_viewers_millions", "total_18_49", "viewers_in_millions", "total_viewers_millions","season", "weekrank", "nightlyrank","timeslot_et","ref") 

audtion_df <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(audtion_df) <- c("Episode Air Date", "Episodes", "Audition Date", "Audition City" , "Audition Venue", "Callback Audition Date", "Callback Venue", "Filming Dates", "Filming Venue", "Guest Judge", "Tickets to Hollywood", "Season") 
audtion_df <- janitor::clean_names(audtion_df)

season_df <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(season_df) <- c("season", "winner", "runner_up", "original_release" , "original_network", "no_of_episodes", "hosted_by", "judges", "mentor", "finals_venue", "country_of_origin") 

finalist_df <- data.frame(matrix(ncol = 6, nrow = 0))

elimination_df <-  data.frame(matrix(ncol = 46, nrow = 0))
elimination_df_columns <- c("season", "place", "gender","contestant","top_36","top_36_2","top_36_3","top_36_4","top_32","top_32_2","top_32_3","top_32_4", "top_30","top_30_2","top_30_3" ,"top_25","top_25_2","top_25_3", "top_24","top_24_2", "top_24_3", "top_20","top_20_2", "top_16", "top_14" ,  "top_13", "top_12", "top_11","top_11_2", "wildcard", "comeback", "top_10" , "top_9","top_9_2","top_8","top_8_2", "top_7","top_7_2", "top_6" ,"top_6_2" ,"top_5" ,"top_5_2", "top_4", "top_4_2", "top_3", "finale" )
colnames(elimination_df) <- elimination_df_columns

# bring in cleaner functions
fs::dir_walk("R", source)

# loop through seasons and scrape data
for (season in season_nums) {
  print(season)
  
  page <- read_html(paste0("https://en.wikipedia.org/wiki/American_Idol_(season_", season, ")"))
  
  ###### Week Numbers and Descriptions ######
  # extract header text
  headers <- page %>%
    html_nodes("div.mw-parser-output") %>%
    html_nodes("h3, h2") %>%
    html_nodes(".mw-headline") %>%
    html_text() %>%
    .[grep("ollywood", x=., value=F):grep("Finale|Top 2$|Top 2 |Show", x=., value=F)]
  
  # extract header ids
  header_ids <- page %>%
    html_nodes("div.mw-parser-output") %>%
    html_nodes("h3, h2") %>%
    html_nodes(".mw-headline") %>%
    html_attr("id") %>%
    .[grep("ollywood", x=., value=F):grep("Finale|Top_*2$|Top_*2_", x=., value=F)]
  
  # get paragraphs for each header (episode week)
  # (except headers with 'Group', those don't have descriptions)
  if ( length(header_ids[-grep("Group", header_ids)])==0 ) {
    week_name <- header_ids
  } else {
    week_name <- header_ids[-grep("Group", header_ids)]
  }
  
  week_name <- week_name %>%
    modify(~gsub('\\([^()]*\\)', "", .x))
  
  # get descriptions of each episode week
  week_desc <- map(header_ids,
                   function(x) 
                     page %>% 
                     html_nodes(xpath=paste0('//*[@id="',x,'"]/../following-sibling::p[1]')) %>%
                     html_text()
  ) %>%
    unlist() %>%
    modify(~gsub('\\n|\\\\|\\[[0-9]+?\\]', "", .x))
  ### EDIT: can't be the same one multiple weeks in a row
  
  # put into dataframes
  week_names_int_df <- data.frame(week_name, week_order = seq(1,length(week_name)), season = as.integer(season))
  week_desc_int_df <- data.frame(week_desc, week_order = seq(1,length(week_desc)), season = as.integer(season))
  
  week_names_df <- rbind(week_names_df, week_names_int_df)
  tryCatch(
    expr = {
      week_desc_df <- rbind(week_desc_df, week_desc_int_df)
    },
    error = function(e){ 
      print("")
    }
  )
  # week_desc_df <- rbind(week_desc_df, week_desc_int_df)
  
  
  ###### Episode Ratings ######
  ratings_id <- page %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/*[self::h2 or self::h3]') %>%
    .[grep("ating", x=., value=F)] %>%
    html_nodes(".mw-headline") %>%
    html_attr("id")
  
  tryCatch(
    expr = {
      ratings_table_df <- page %>%
        html_nodes(xpath=paste0('//*[@id="',ratings_id,'"]/../following-sibling::table[1]')) %>%
        html_nodes("table") %>%
        html_table() %>%
        .[[1]] 
    },
    error = function(e){ 
      ratings_table_df <<- page %>%
        html_nodes(xpath=paste0('//*[@id="',ratings_id,'"]/../following-sibling::table[1 or 2]')) %>%
        html_table() %>%
        .[[1]]
    }
  )
  
  colnames(ratings_table_df)[2] <- "Episode"
  ratings_table_df$Episode <- str_replace_all(ratings_table_df$Episode, pattern = '\\n|\\\\|\\[[0-9]+?\\]|"', "")
  ratings_table_df$Season <- as.integer(season)
  ratings_table_df <- ratings_table_df %>%
    janitor::clean_names() %>%
    clean_names_rating(.)
  
  if (as.integer(season != 14)) { rating_df <- merge(rating_df, ratings_table_df, all.x = TRUE, all.y= TRUE) }
  
  
  ###### Audition locations ######
  audition_id <- page %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/*[self::h2 or self::h3]') %>%
    .[grep("[Aa]udition", x=., value=F)] %>%
    html_nodes(".mw-headline") %>%
    html_attr("id")
  
  audtions_table <- page %>%
    html_nodes(xpath=paste0('//*[@id="',audition_id,'"]/../following-sibling::table')) %>%
    .[[1]] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    mutate(Season = as.integer(season)) %>%
    janitor::clean_names(case = "title") %>%
    clean_names_audition(.) %>%
    filter(!str_detect(audition_date, 'Total Tickets to Hollywood|Total number of tickets to Hollywood'))
  
  audtion_df <- merge(audtion_df, audtions_table, all.x = TRUE, all.y= TRUE)
  
  
  ###### Season Info ######
  season_info <- page %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    filter(American.Idol != American.Idol.1) %>%
    rename(Col = American.Idol, Info = American.Idol.1) %>%
    pivot_wider(names_from = Col, values_from = Info) 
  
  if (startsWith(season_info$Judges, '.')) {
    season_info$Judges <-
      page %>%
      # html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[1]/tbody/tr[5]/td/div') %>% 
      html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[1]') %>% html_node("div.plainlist") %>%
      html_text()
  }
  
  season_info <- 
    season_info %>%
    mutate(Season = as.integer(season),
           `Hosted by` =  gsub("([a-z])([A-Z])", "\\1; \\2", `Hosted by`),
           Judges = gsub("([a-z])([A-Z])", "\\1; \\2", Judges),
           Judges = gsub("\n", "; ", trimws(Judges))) %>%
    janitor::clean_names()
  
  season_df <- merge(season_df, season_info, all.x = TRUE, all.y= TRUE)
  
  
  ###### Finalists ######
  # this section was developed through trial and error, and with more time, could be better and more readable! 
  if (as.integer(season) < 18) {
    finalist_id <- NULL
    finalist_id <- page %>%
      html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/*[self::h2 or self::h3]') %>%
      .[grep("Finalist|Semifinals|Top_10_[Cc]ontestants", x=., value=F)] %>%
      html_nodes(".mw-headline") %>%
      html_attr("id") %>%
      sort()
    
    if (season %in% c("6","7")) {
      finalist_desc <- page %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[12]') %>% html_table() %>% .[[1]] %>% pull()
    } else if (season == "14") {
      finalist_desc <- page %>%
        html_nodes(xpath=paste0('//*[@id="',finalist_id,'"]/../following-sibling::ul')) %>%
        html_nodes("li") %>% html_text()
    } else if (as.integer(season) >= 8 & as.integer(season) <= 10) {
      finalist_desc <- list()
      for (num in seq(from = 1, to = 10)) {
        finalist_desc <- append(finalist_desc, page %>%
                                  html_nodes(xpath=paste0('//*[@id="',finalist_id,'"]/../following-sibling::p[',num,']')) %>% html_text() )
      }
    }  else if (length(page %>%
                       html_nodes(xpath=paste0('//*[@id="',finalist_id,'"]/../following-sibling::ul')) %>%
                       .[1] %>%
                       html_nodes("li")) == 1) {
      finalist_desc <- page %>%
        html_nodes(xpath=paste0('//*[@id="',finalist_id,'"]/../following-sibling::ul')) %>%
        html_nodes("li") %>% html_text()
    } else {
      finalist_desc <- page %>%
        html_nodes(xpath=paste0('//*[@id="',finalist_id,'"]/../following-sibling::ul')) %>%
        .[1] %>%
        html_nodes("li") %>% html_text()
    }
    
    name <- map(finalist_desc, function(x) str_split(string = x, pattern = ' \\(born |\\(|\n')[[1]][1] %>%
                  str_trim(.))
    
    birthday <- map(finalist_desc, function(x) str_split(string = x, pattern = ' \\(born |\\(')[[1]][2] %>%
                      str_extract(pattern = '^(.*?)[[:digit:]]{4}', .))
    birthplace <- map(finalist_desc, function(x) str_match(string = x, 
                                                           pattern = '(?<= from|in)\\s*[A-Za-z]*\\s+[A-Za-z]*, [A-Za-z]*\\s*[A-Z]{1}[a-z]*') %>%
                        str_remove(., pattern = '^in ')
    )
    
    hometown <- map(finalist_desc, function(x) str_match(string = x, pattern = '(?<=is from)[A-Za-z]*\\s+[A-Za-z]*, [A-Za-z]*\\s*[A-Z]{1}[a-z]*') %>%
                      str_trim(.))
    contestant_desc <- map(finalist_desc, function(x) 
      paste(
        str_split(string = x, pattern = '\\.')[[1]][2:length(str_split(string = x, pattern = '\\.')[[1]])] %>%
          map(., function(x) ifelse(is.na(x), "", x)) %>%
          str_replace_all(., pattern = '\\\" ', ""),
        collapse = ".") %>%
        str_trim(.)
    ) %>%
      unlist()
    
    contestant_table <- data.frame(unlist(name), unlist(birthday), unlist(birthplace), unlist(hometown), contestant_desc)
    colnames(contestant_table) <- c("Contestant", "Birthday", "Birthplace", "Hometown", "Description") 
    contestant_table <- contestant_table %>% 
      mutate(Season = as.integer(season)) 
    
    finalist_df <- rbind(finalist_df, contestant_table)
  }
  # a better way to do this maybe would have been to manually download the data and just do the cleaning here, since
  # the page formats vary/not stable
  
  ###### Names and Gender of Top 30 ######
  elimination_id <- page %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/*[self::h2 or self::h3]') %>%
    .[grep("[Ee]limination_*[Cc]hart", x=., value=F)] %>%
    html_nodes(".mw-headline") %>%
    html_attr("id") 
  
  elimination_table <- data.frame()
  elimination_table <- page %>%
    html_nodes(xpath=paste0('//*[@id="',elimination_id,'"]/../following-sibling::table[1]')) %>%
    html_table() %>%
    .[[1]] %>%
    .[-c(1),-2] %>%
    janitor::clean_names() %>%
    select(!starts_with("x")) %>%
    clean_names_elimination(.) 
  
  # seasons 13 - 15 are within 'center'
  if (colnames(elimination_table)[1] != "place") {
    elimination_table <- page %>%
      html_nodes(xpath=paste0('//*[@id="',elimination_id,'"]/../following-sibling::center[1]')) %>%
      html_table() %>%
      .[[1]] %>%
      .[-c(1,2),-2] %>%
      janitor::clean_names() %>%
      select(!starts_with("x")) %>%
      clean_names_elimination(.) %>%
      .[-1,]
  }
  
  if (season == 19) {
    elimination_table <- elimination_table[,-2]
  }
  
  # gender of finalists
  gender <- page %>%
    html_nodes(xpath=paste0('//*[@id="',elimination_id,'"]/../following-sibling::table[1]')) %>%
    html_nodes("tr") %>%
    html_nodes("td") %>%
    html_attr("style") %>%
    str_match(., "pink|cyan") %>%
    .[!is.na(.)] 
  
  ## names of finalists
  indx_name <- page %>%
    html_nodes(xpath=paste0('//*[@id="',elimination_id,'"]/../following-sibling::table[1]')) %>%
    html_nodes("tr") %>%
    html_nodes("td") %>%
    html_attr("style") %>%
    str_match(., "pink|cyan")
  
  title_names  <- page %>%
    html_nodes(xpath=paste0('//*[@id="',elimination_id,'"]/../following-sibling::table[1]')) %>%
    html_nodes("tr") %>%
    html_nodes("td")
  
  finalists_names <- title_names[2:length(title_names)][indx_name %in% c("pink", "cyan")] %>%
    html_text2()
  
  # table of name and gender of top 30 finalists
  finalists <- data.frame(finalists_names, gender) %>%
    mutate(gender = ifelse(gender=="pink", "Female", "Male"))
  
  elimination_table <- elimination_table %>%
    left_join(finalists, by = c("contestant" = "finalists_names")) %>%
    mutate(season = as.integer(season))
  
  elimination_df <- merge(elimination_df, elimination_table, all.x = TRUE, all.y= TRUE)
  
  
  ###### Songs ###### 
  ### Create list of song tables
  
  song_headers <- header_ids[-grep("Semi-finals|Finals|Finalists", header_ids)]
  songs_list_all <- map(song_headers,
                        function(x)
                          page %>%
                          html_nodes(xpath=paste0('//*[@id="',x,'"]/../following-sibling::table[1]')) %>%
                          .[1] %>%
                          html_table(fill = TRUE)
  )
  
  ### Formatting changes slightly for some seasons
  if (length(songs_list_all)==0 | season %in% c( "9", "10", "11", "12")) {
    
    song_headers <- header_ids
    songs_list_all <- 
      map(song_headers, function(x) page %>% 
            html_nodes(xpath=paste0('//*[@id="',x,'"]/../following-sibling::table[1]')) %>%
            .[1] %>%
            html_table(fill = TRUE)
      ) 
    
  }
  
  songs_list <- list()
  increase_ind <- 0
  for (ind in seq_along(songs_list_all)) {
    
    i <- as.data.frame(songs_list_all[[ind]]) %>%
      mutate(across(everything(), as.character))
    
    if (as.integer(season) %in% c(18, 19) & ind == 3) {
      i = subset(i, select = -1 )
    }
    
    # only look at tables that are actually Song tables
    if (grepl("[Cc]ontestant", i[1,1]) | (ncol(i)>1 && !is.na(i[1,1]))) {
      
      ### need to grab next table in cases where it grabs the legend
      if (grepl("[Cc]ontestant", i[1,1]) & !(ncol(i)>1 && !is.na(i[1,1]))) {
        
        not_all_na <- function(x) any(!is.na(x))
        i <- page %>%
          html_nodes(xpath=paste0('//*[@id="',header_ids[ind],'"]/../following-sibling::table[2]')) %>%
          .[1] %>%
          html_table(fill = TRUE) %>%
          data.frame() %>%
          select(where(not_all_na)) %>%
          mutate(across(everything(), as.character))
        
      }
      
      if (colnames(i)[1]==i[1,1]) {
        
        i <- i %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
          rename(Contestant = contestant)
        
      }
      
      # this should be two tables, but is brought in as one
      if ("na" %in% colnames(i)) {
        i1 <- i %>%
          select(1:"na") %>%
          select(!"na")
        
        i2 <- i %>%
          select("na":ncol(.)) %>%
          select(!"na")
        
        i_list <- list(i1, i2)
      } else if (("result" %in% colnames(i) & "result_2" %in% colnames(i))) {
        i1 <- i %>%
          select(1:"contestant_2") %>%
          select(!"contestant_2")
        
        i2 <- i %>%
          select("contestant_2":ncol(.)) 
        
        i_list <- list(i1, i2)
      } else {
        i_list <- list(i)
      }
      
      for (ind_loop in seq_along(i_list)) {
        
        i <- i_list[[ind_loop]]
        if (ind_loop > 1 ) {
          increase_ind <- 1 + increase_ind
        }
        ### if multiple Song and Order columns, transform to standardize
        ### note: the following logic could definitely be cleaned up more. I developed it 
        ### through trial and error, and with more time I would love to clean it up more to be
        ### more readable and concise!
        firstsong_colname <- i %>% select(starts_with('First.Song')) %>% colnames(.)
        songchoice_colname <- i %>% select(contains('choice')) %>% colnames(.)
        song_cols <- i %>% .[ , grepl( "[Ss]ong" , names( . ) ) ] %>% colnames(.)
        order_cols <- i %>% .[ , grepl( "[Oo]rder" , names( . ) ) ] %>% colnames(.)
        num_song_cols <- i %>%
          select(contains("Song")|contains("Contestant")) %>%
          colnames() %>%
          length()
        
        if (length(song_cols) %in% c(1,2) & length(songchoice_colname)==0) {
          
          i <- i %>%
            pivot_longer(contains("Order"), #-c(Contestant, contains("Song"), Result),
                         values_to = "Order") %>%
            select(-name) %>%
            pivot_longer(contains("Song"), values_to = "Song") %>%
            group_by(Contestant) %>%
            mutate(n = row_number(), 
                   First.Song = ifelse(n==1,!! song_cols[1], NA),
                   Second.Song = ifelse(n==4,!! song_cols[2], NA)
            ) 
          
          if (!all(is.na(i$Second.Song))) {
            i <- i %>%
              filter(!(is.na(First.Song) & is.na(Second.Song)))
          }
          
          i <- i %>%
            select(- c(contains("."),"n")) %>%
            ungroup() %>%
            rename(Song_Theme = name)
          
          if (length(colnames(i))==5) {
            i <- i %>%
              select("Order", "Contestant", "Song", "Song_Theme","Result") %>%
              arrange(Order)
          }
          
          if (length(colnames(i))==4) {
            i <- i %>%
              select("Order", "Contestant", "Song", "Result") %>%
              arrange(Order)
          }
          
        } else if (length(order_cols)>2) {
          
          uni_song_names <- colnames(i)[grep(pattern = order_cols, x = colnames(i)) + 1]
          
          i <- i %>%
            pivot_longer(contains("Order"), 
                         values_to = "Order") %>%
            select(-name) %>%
            pivot_longer(!! uni_song_names, values_to = "Song") %>%
            group_by(Contestant) %>%
            mutate(n = row_number(), 
                   First.Song = ifelse(n==1,!! uni_song_names[1], NA),
                   Second.Song = ifelse(n==5,!! uni_song_names[2], NA),
                   Third.Song = ifelse(n==9,!! uni_song_names[3], NA),
            ) %>%
            filter(!(is.na(First.Song) & is.na(Second.Song) & is.na(Third.Song))) %>%
            select(- c(contains("."),"n")) %>%
            ungroup() %>%
            rename(Song_Theme = name)
          
          if (length(colnames(i))==5) {
            i <- i %>%
              select("Order", "Contestant", "Song", "Song_Theme","Result") %>%
              arrange(Order)
          }
        } else if (num_song_cols>1 & length(firstsong_colname)!=0) {
          
          i <- i %>%
            pivot_longer(contains("Order"), #-c(Contestant, contains("Song"), Result),
                         values_to = "Order") %>%
            select(-name) %>%
            group_by(Contestant) %>%
            mutate(n = 1:n(), 
                   First.Song = ifelse(n==1,!! firstsong_colname, NA),
                   Song = coalesce(!!! syms(colnames(.)[str_detect(colnames(.), "Song")] ))
            ) %>% 
            select(- c(contains("."),"n")) %>%
            ungroup()
          
          i <- i %>%
            select("Order", "Contestant", "Song", "Result") %>%
            arrange(Order)
          
        } else if (num_song_cols>1 & length(songchoice_colname)!=0) {
          
          i <- i %>%
            pivot_longer(contains("Order"), #-c(Contestant, contains("Song"), Result),
                         values_to = "Order") %>%
            select(-name) %>%
            pivot_longer(contains("choice"), names_to = "Song_Theme", values_to = "Song") %>%
            group_by(Contestant) %>%
            mutate(n = 1:n()) %>% filter(n==min(n)|n==max(n)) %>%
            select(-n)
          
          i <- i %>%
            select("Order", "Contestant", "Song", "Song_Theme", "Result") %>%
            arrange(Order)
          
        }
        
        i <- i %>%
          janitor::clean_names() %>%
          clean_names_songs()
        
        if ((ncol(i) < 10 & ncol(i) > 1) && (!("audition_city" %in% colnames(i)) | as.integer(season) %in% c(18, 19))) {
          songs_list[[ind + increase_ind]] <- i 
        }
        
        firstsong_colname <- NA
        songchoice_colname <- NA
        song_cols <- NA
        order_cols <- NA
      }
    }
  }
  # remove NULL from songs_list
  songs_list <- songs_list[!(sapply(songs_list,is.null))]
  # remove duplicate dataframes
  track_ind <- c()
  for (ind in seq_along(songs_list)) {
    for (x in seq_along(songs_list)) {
      if ((ind != x) && isTRUE(all_equal(songs_list[[ind]], songs_list[[x]])) ) {
        if ((! ind %in% track_ind) & (! x %in% track_ind)) {
          track_ind <- c(track_ind, x)
        }
      }
    }
  }
  if (!is.null(track_ind)) {
    songs_list <- songs_list[-track_ind]
  }
  
  
  ############################################################
  
  # This section is to filter to episode week names/descriptions for episodes that include songs sung for competition by top contenders
  # (i.e filtering out week names/descriptions for audtion, result, special episodes etc.)
  episodes_with_competition_songs <- 
    ratings_table_df %>%
    filter(!(str_detect(episode, '[Rr]esult|[Aa]udition|[Hh]ollywood|Special|Best of the Rest|American Dream|Idol Gives Back|Las Vegas Round|Top 12 Revealed'))) %>%
    head(-1) %>% # remove Finale results show 
    mutate(extract_top_num = as.integer(str_replace_all(str_extract(episode, pattern = 'Top \\d+'), pattern = " |Top ", "")),
           lag_num = as.integer(lag(extract_top_num)),
           lead_num = as.integer(lead(extract_top_num)),
           top_num = case_when(
             extract_top_num == lag_num & lag_num > 7 ~ as.integer(extract_top_num + lag(extract_top_num, default = 0)), 
             extract_top_num == lead_num & lead_num > 7 ~ as.integer(extract_top_num + lead(extract_top_num, default = 0)), 
             is.na(lag_num) ~ extract_top_num,
             T ~ extract_top_num)
    ) %>%
    select(!c(lag_num, lead_num)) 
  
  if (as.integer(season) == 13) {
    episodes_with_competition_songs <- 
      episodes_with_competition_songs %>%
      tail(length(songs_list))
    
    episodes_with_competition_songs$airdate <- glue::glue('{episodes_with_competition_songs$airdate}, 2014')
  }
  
  week_desc_df <-
    week_desc_df %>%
    mutate(
      lag_week_desc = lag(week_desc),
      week_desc = ifelse(week_desc == lag_week_desc | str_detect(week_desc, "olor key"), NA, week_desc)
    )
  
  week_names_descriptions <- 
    week_names_df %>% 
    left_join(., week_desc_df, by = c("season", "week_order")) %>%
    mutate(week_name_desc = str_replace_all(str_split(week_name, pattern = '_[–-]_',  simplify = TRUE)[ , 2], pattern = "_", " "),
           top_num = as.integer(str_replace_all(str_extract(week_name, pattern = 'Top_\\d+_'), pattern = "_|Top_", ""))
    ) %>%
    filter(!is.na(week_name_desc) & !is.na(top_num), season == as.integer(season)) %>%
    select(top_num, week_name_desc, week_desc) %>%
    group_by(top_num) %>%
    mutate(week_name_desc = paste0(week_name_desc, collapse = " / ")) %>% distinct()
  
  episodes_with_competition_songs <- 
    episodes_with_competition_songs %>%
    left_join(., week_names_descriptions, by = "top_num") %>%
    mutate(week_desc = coalesce(as.character(week_desc), as.character(episode)),
           is_saved = NA) 
  
  ############################################################
  
  ######################## Save Songs ########################
  
  #### Save songs ####
  # where possible, I include week description as the file name
  # if (as.integer(season) == 4) {
  #   for (ind in seq_along(songs_list)) {
  #     dir_path <- glue::glue('./Songs/Season_{str_pad(season, 2, pad = "0")}')
  #     if(!dir.exists(dir_path)){
  #       dir.create(dir_path)
  #     }
  # 
  #     top_nrow <- length(unique(songs_list[[ind]]$contestant))
  #     name <-
  #       episodes_with_competition_songs %>%
  #       arrange(show_number) %>%
  #       filter(as.integer(extract_top_num) == top_nrow & is.na(is_saved)) %>%
  #       slice(1) %>%
  #       select(airdate, top_num, episode)
  #     episodes_with_competition_songs[episodes_with_competition_songs$top_num == name$top_num & episodes_with_competition_songs$episode == name$episode,]$is_saved <- T
  #     write.csv(sapply(songs_list[[ind]], remove_wiki_citation_num),
  #               glue::glue('{dir_path}/{str_replace_all(zoo::as.Date(name$airdate, "%b %d, %Y"), "-", "")}_top_{name$top_num}_{janitor::make_clean_names(name$episode)}.csv'),
  #               row.names = F)
  #     }
  # } else if (as.integer(season) < 14 & !(as.integer(season) %in% c(10, 9))
  #          ) {
  #   for (ind in seq_along(songs_list)) {
  #     dir_path <- glue::glue('./Songs/Season_{str_pad(season, 2, pad = "0")}')
  #     if(!dir.exists(dir_path)){
  #       dir.create(dir_path)
  #     }
  # 
  # name <-
  #   episodes_with_competition_songs %>%
  #   arrange(show_number) %>%
  #   filter(is.na(is_saved)) %>%
  #   slice(1) %>%
  #   select(airdate, episode)
  #     episodes_with_competition_songs[episodes_with_competition_songs$episode == name$episode,]$is_saved <- T
  # 
  #     write.csv(sapply(songs_list[[ind]], remove_wiki_citation_num) %>% as.data.frame(),
  #               glue::glue('{dir_path}/{str_replace_all(zoo::as.Date(name$airdate, "%b %d, %Y"), "-", "")}_{janitor::make_clean_names(name$episode)}.csv'),
  #               row.names = F)
  #   }
  # } else {
  #   for (ind in seq_along(songs_list)) {
  #     dir_path <- glue::glue('./Songs/Season_{str_pad(season, 2, pad = "0")}')
  #     if(!dir.exists(dir_path)){
  #       dir.create(dir_path)
  #     }
  #     write.csv(sapply(songs_list[[ind]], remove_wiki_citation_num),
  #               glue::glue('{dir_path}/songs_{str_pad(ind, 2, pad = "0")}.csv'),
  #               row.names = F)
  #     }
  # }
  
  
  }
  
  ######################## Save Data ########################
  
# Episode ratings
rating_df <- 
  sapply(rating_df, remove_wiki_citation_num) %>% 
  as.data.frame() %>%
  mutate(show_number = as.integer(show_number)) %>%
  select(season, show_number, episode, everything()) %>%
  arrange(season, show_number)
# write.csv(rating_df, "./metadata/ratings.csv", row.names = F)
    
# Audition information
audtion_df <- 
  sapply(audtion_df, remove_wiki_citation_num) %>%
  mutate(season = as.integer(season)) %>%
  data.frame() %>%
  arrange(season)
# write.csv(audtion_df, "./metadata/auditions.csv", row.names = F)

# Season overview/winner
season_df <-
  season_df %>%
  mutate(judges = ifelse(str_detect(judges, "^\\."), NA, judges))
# write.csv(season_df, "./metadata/seasons.csv", row.names = F)

# Information/bio on season finalists
finalist_df <-
  finalist_df %>%
  filter(!(is.na(Birthday) & is.na(Birthplace) & is.na(Hometown)) ) %>%
  sapply(., remove_wiki_citation_num) %>%
  janitor::clean_names()
# write.csv(finalist_df, "./metadata/finalists.csv", row.names = F)

# elimination_df
elimination_df <- elimination_df %>%
  arrange(season, as.integer(str_split_fixed(place, "–|-", 2)[,1])) %>%
  select(!! elimination_df_columns)
# write.csv(elimination_df, "./metadata/elimination_chart.csv", row.names = F)