# cleaning functions
clean_names_rating <- function(df) {
  
  colnames(df) <- case_when(colnames(df) == "air_date" ~ "airdate", 
                            colnames(df) == "no" ~ "show_number", 
                            colnames(df) == "u_s_viewers_millions" ~ "viewers_in_millions", 
                            colnames(df) == "viewers_millions" ~ "viewers_in_millions",
                            colnames(df) == "weeklyrank" ~ "weekrank", 
                            colnames(df) == "weeklyrank" ~ "weekrank", 
                            colnames(df) == "rating" ~ "rating_share", 
                            colnames(df) == "rating_share_18_49_2" ~ "share", 
                            colnames(df) == "rating_share18_49" ~ "18_49_rating_share", 
                            colnames(df) == "ratings" ~ "rating_share", 
                            colnames(df) == "rating_share_18_49" ~ "18_49_rating_share", 
                            colnames(df) == "x18_49_rating" ~ "18_49_rating_share",
                            colnames(df) == "x18_49_rating_share" ~ "18_49_rating_share", 
                            colnames(df) == "x18_49rating" ~ "18_49_rating_share", 
                            colnames(df) == "x18_49rating_share" ~ "18_49_rating_share", 
                            colnames(df) == "ratings_sharehouseholds" ~ "rating_share_households", 
                            colnames(df) == "timeslot" ~ "timeslot_et", 
                            colnames(df) == "show" ~ "show_number", 
                            colnames(df) == "note" ~ "ref", 
                            T ~ colnames(df)) 
  
  df 
}

clean_names_audition <- function(df) {
  
  colnames(df) <- case_when(colnames(df) == "Audition Venue 5" ~ "Audition Venue", 
                            colnames(df) == "Audition Venues" ~ "Audition Venue", 
                            colnames(df) == "Callback Date" ~ "Callback Audition Date", 
                            colnames(df) == "Date of Airing" ~ "Episode Air Date", 
                            colnames(df) == "Episode Airdate" ~ "Episode Air Date", 
                            colnames(df) == "Episodeair Date" ~ "Episode Air Date", 
                            colnames(df) == "Episode s" ~ "Episodes", 
                            colnames(df) == "First Audition Date" ~ "Audition Date", 
                            colnames(df) == "Preliminary Date" ~ "Audition Date", 
                            colnames(df) == "Date" ~ "Audition Date", 
                            colnames(df) == "First Audition Venue" ~ "Audition Venue", 
                            colnames(df) == "Venue" ~ "Audition Venue", 
                            colnames(df) == "Preliminary Venue" ~ "Audition Venue", 
                            colnames(df) == "Location" ~ "Audition City", 
                            colnames(df) == "City" ~ "Audition City", 
                            colnames(df) == "Filming Date s" ~ "Filming Dates", 
                            colnames(df) == "Golden Tickets" ~ "Tickets to Hollywood", 
                            T ~ colnames(df)) 
  
  df %>%
    janitor::clean_names()
}

clean_names_elimination <- function(df) {
  
  colnames(df) <- case_when(colnames(df) == "contestant_2" ~ "contestant", 
                            colnames(df) == "top_4_b" ~ "top_4", 
                            colnames(df) == "top_4_b_2" ~ "top_4_2", 
                            colnames(df) == "top_11_a" ~ "top_11",
                            colnames(df) == "top_11_a_2" ~ "top_11_2", 
                            colnames(df) == "top_13_a" ~ "top_13", 
                            colnames(df) == "top_8_a" ~ "top_8", 
                            colnames(df) == "top_8_a_2" ~ "top_8_2", 
                            colnames(df) == "top_9_b" ~ "top_9", 
                            colnames(df) == "semi_finals" ~ "top_24",
                            colnames(df) == "semi_finals_2" ~ "top_24_2", 
                            colnames(df) == "semi_finals_3" ~ "top_24_3", 
                            colnames(df) == "semi_finals_top_24" ~ "top_24", 
                            colnames(df) == "semi_finals_top_24_2" ~ "top_24_2", 
                            colnames(df) == "semi_finals_top_24_3" ~ "top_24_3", 
                            colnames(df) == "top_24_gr_1" ~ "top_24",
                            colnames(df) == "top_24_gr_2" ~ "top_24_2", 
                            colnames(df) == "top_36_gr_1" ~ "top_36", 
                            colnames(df) == "top_36_gr_2" ~ "top_36_2",
                            colnames(df) == "top_36_gr_3" ~ "top_36_3", 
                            colnames(df) == "top_5_b" ~ "top_5_2", 
                            colnames(df) == "top_6_b" ~ "top_6", 
                            colnames(df) == "top_7_b" ~ "top_7", 
                            colnames(df) == "top_7_b_2" ~ "top_7_2", 
                            colnames(df) == "top_7_d" ~ "top_7", 
                            colnames(df) == "top_7_d_2" ~ "top_7_2", 
                            colnames(df) == "top_9_a" ~ "top_9", 
                            colnames(df) == "top_9_a_2" ~ "top_9_2", 
                            colnames(df) == "wild_card" ~ "wildcard", 
                            T ~ colnames(df)) 
  
  df 
}

clean_names_songs <- function(df) {
  
  colnames(df) <- case_when(colnames(df) == "date_of_airing_28_29" ~ "date_of_airing", 
                            colnames(df) == "date_of_airing_32_33" ~ "date_of_airing", 
                            colnames(df) == "duet_partner_12" ~ "duet_partner", 
                            colnames(df) == "duet_partner_31" ~ "duet_partner",
                            colnames(df) == "duet_partner_35" ~ "duet_partner", 
                            colnames(df) == "contestant_s" ~ "contestant", 
                            colnames(df) == "featured_film" ~ "song", 
                            colnames(df) == "featured_movie" ~ "song", 
                            colnames(df) == "elton_john_song" ~ "song", 
                            colnames(df) == "movie_duet" ~ "duet_song", 
                            colnames(df) == "music_of_detroit" ~ "song",
                            colnames(df) == "queen_song_20" ~ "song", 
                            # colnames(df) == "result_2" ~ "result", 
                            colnames(df) == "result_13" ~ "result",
                            colnames(df) == "song_15" ~ "song", 
                            colnames(df) == "song_18" ~ "song", 
                            colnames(df) == "song_19" ~ "song", 
                            colnames(df) == "song_22" ~ "song",
                            colnames(df) == "song_24" ~ "song", 
                            colnames(df) == "song_26" ~ "song", 
                            colnames(df) == "song_12" ~ "song", 
                            colnames(df) == "song_30_31" ~ "song",
                            colnames(df) == "song_34_35" ~ "song", 
                            colnames(df) == "song_36" ~ "song", 
                            colnames(df) == "song_38" ~ "song", 
                            colnames(df) == "song_9" ~ "song", 
                            colnames(df) == "song_original_artist" ~ "song", 
                            colnames(df) == "song_original_artist_when_applicable" ~ "song", 
                            colnames(df) == "soul_song" ~ "song", 
                            colnames(df) == "winner" ~ "result",
                            T ~ colnames(df)) 
  
  df 
}


remove_wiki_citation_num <- function(x) {
  gsub("\\[[^[]*\\]", "", x)
}


remove_citation_num <- function(x) {
  stringr::str_remove(x, '[0-9]{2}\\s*$')
}


standardize_cols <- function(filename) {
  
  c <- colnames(read_csv(filename))
  
  fix_cols <- c("song...3", "song...4", "result...4", "result...5", "song...7", "contestant_21_1", "judges_hometown", "personal_idol", "production_play", "song_original_artist")
  if (any(unlist(fix_cols) %in% c)) {
    return(read_csv(filename, col_names =   
                      dplyr::case_match(c, 
                                        c("song...3") ~ "song",
                                        c("song_original_artist") ~ "song",
                                        c("song...4") ~ "song_theme",
                                        c("song...7") ~ "song_2",
                                        c("result...5") ~ "result",
                                        c("result...4") ~ "song_theme",
                                        c("contestant_21_1") ~ "contestant",
                                        c("judges_hometown") ~ "song_2",
                                        c("personal_idol") ~ "song_theme",
                                        c("production_play") ~ "song_theme",
                                        .default = c
                      )) %>% .[-1,]
    ) 
    
  }
  
  read_csv(filename) %>% mutate_all(as.character)
  
}


get_artist <- function(song) {
  
  artist <-
    ifelse(stringr::str_detect(song, "\\)$"),
           stringr::str_extract(string = song,
                                pattern = "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
           NA
    )
  
  ifelse(stringr::str_detect(song, "\\)$") && length(strsplit(artist,' ')[[1]])>4 && !grepl("and|&|featuring", artist),
         "FLAG",
         artist)
  
}


clean_song_title <- function(song, artist_name) {
  
  ifelse(is.na(artist_name), song, gsub(paste0(" \\(", artist_name, "\\)"), '', song))
  
}

library(spotifyr)
gen_artist_from_song <- function(song_name) {
  
  search_r <- search_spotify(song_name)
  tracks <- data.frame(Reduce(rbind, search_r$tracks)) 
  
  # some logic to try and get the original artist
  if (length(str_split(song_name, ' ')[[1]]) == 1) {
    tracks <- tracks %>% filter(name == song_name)
  } else {
    tracks <- tracks[c(2:3),] %>% arrange(popularity) %>% arrange(album.release_date) %>% slice(1)
  }

  artist <- tracks$artists[[1]]$name 
  
  return(stringr::str_c(cbind(artist), collapse = ","))
  
}
