library(tidyverse)
# bring in custom cleaner functions
fs::dir_walk("R", source)

songs <- 
  # read in downloaded songs
  fs::dir_ls("./Songs/", recurse = T, glob = "*.csv") %>% 
  map_df(~standardize_cols(.x) %>% mutate(file = .x, season = str_split_1(.x, '/')[3], week = tools::file_path_sans_ext(str_split_1(.x, '/')[4]))) %>%
  filter(is.na(x), !is.na(contestant)) %>%
  # initial cleaning
  mutate(song = str_squish(str_remove_all(song, '"')),
         song_theme = coalesce(song_theme, song_theme_1),
         order = as.integer(order)
         ) %>%
  # add order to episodes that didn't specify
  group_by(season, week) %>% mutate(order = ifelse(is.na(order), 1:n(), order)) %>% ungroup() %>%
  # more cleaning
  mutate(
    contestant = ifelse(!is.na(duet_partner), glue::glue('{contestant}, with {duet_partner}'), contestant),
    song_theme = gsub("Original Artist", '', str_to_title(ifelse(stringr::str_detect(song_theme, "^[Ss]ong"), NA, str_squish(remove_citation_num(str_replace_all(song_theme, '\\.s\\.|\\.|^X', ' ')))))),
    artist = map_chr(song, ~get_artist(.x)),
    song = map2_chr(song, artist, ~clean_song_title(.x, .y)),
    # artist = ifelse(is.na(artist), map_chr(song, ~gen_artist_from_song(.x)), artist)
  ) %>%
  select(season, week, order, contestant, song, artist, song_theme, result) 




songs %>%
  ### for some reason it is getting stuck here....
  mutate(artist = ifelse(is.na(artist), map_chr(song, ~gen_artist_from_song(.x)), artist)) 

o <- songs %>%
  filter(is.na(artist)) %>% .[38,]

