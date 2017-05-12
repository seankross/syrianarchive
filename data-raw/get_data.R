library(rvest)
library(purrr)
library(dplyr)
library(stringr)
library(xml2)
library(tidyr)
library(readr)
library(lubridate)
library(devtools)

# turn degrees minutes seconds coordinates into decimal degrees
coord <- function(x){
  if(is.na(x)){
    return(NA)
  }
  x <- gsub("N|E|W|S|\\s", "", x)
  x <- gsub("[^0-9]$", "", x)
  degrees <- str_match(x, "[0-9\\.]+") %>% as.character()
  x <- sub(degrees, "", x)
  x <- sub("^[^0-9]", "", x)
  minutes_ <- str_match(x, "[0-9\\.]+") %>% as.character()
  x <- sub(minutes_, "", x)
  x <- sub("^[^0-9]", "", x)
  seconds_ <- str_match(x, "[0-9\\.]+") %>% as.numeric()
  degrees <- degrees %>% as.numeric()
  minutes_ <- minutes_ %>% as.numeric()
  sum(c(sum(c(degrees, minutes_ / 60), na.rm = TRUE), seconds_ / 3600), na.rm = TRUE)
}

# There are 77 pages as of 2017-03-29
base_url <- "https://syrianarchive.org"

page_urls <- paste0(base_url, "/database/?page=", 1:77)

get_db_ids <- . %>%
  read_html() %>%
  html_nodes("td") %>%
  html_nodes("a") %>%
  html_attr("href")

# This takes ~5 minutes
# There are 3842 entires as of 2017-03-29
db_urls <- map(page_urls, get_db_ids) %>%
  unlist()
saveRDS(db_urls, file="data-raw/db_urls.rds")
# db_urls <- readRDS("data-raw/db_urls.rds")

db_urls <- db_urls %>%
  paste0(base_url, .)

# This takes a while
db_html <- map(db_urls, read_html)

syrian_archive <- map_df(db_html, function(db_page){
  db_p <- db_page %>%
    html_nodes("p") %>%
    as_list()

  data_frame(description = tryCatch(db_p[[1]][[1]], error = function(e){NA}),
             location = db_p[[2]][[2]],
             latitude = db_p[[3]][[2]],
             longitude = db_p[[4]][[2]],
             reference_code = db_p[[6]][[2]],
             violation_type = db_p[[7]][[2]],
             recording_date = db_p[[8]][[2]],
             source = db_p[[9]][[2]],
             keywords = db_p[[10]][[2]],
             youtube_id = tryCatch(db_p[[11]]$a[[1]], error = function(e){NA}),
             landmarks = db_p[[12]][[2]],
             weather_in_media = db_p[[13]][[2]],
             languages = db_p[[14]][[2]],
             clothes_and_uniforms = db_p[[15]][[2]],
             sex = db_p[[16]][[2]],
             weapons_used = db_p[[17]][[2]],
             edited = db_p[[18]][[2]],
             file_size = db_p[[19]][[2]],
             device_used_to_record = db_p[[20]][[2]],
             media_content_type = db_p[[21]][[2]],
             chain_of_custody_notes = db_p[[22]][[2]],
             finding_aids = db_p[[23]][[2]],
             international_instrument = db_p[[24]][[2]],
             urls_and_news = db_p[[25]][[2]],
             related_incidents = db_p[[26]][[2]],
             # database_id = str_match(db_url, "[0-9]+") %>%
             #                 as.character(),
             page_title = db_page %>%
                            html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "pagetitle", " " ))]') %>%
                            html_text(),
             video_url = db_page %>%
                            html_nodes("video") %>%
                            html_attr("src")
             )
})

write_csv(syrian_archive, "data-raw/syrian_archive_raw.csv")
# syrian_archive <- read_csv("data-raw/syrian_archive_raw.csv")

syrian_archive$database_id <- map_chr(db_urls, function(x){
  str_match(x, "[0-9]+") %>%
    as.character()
})

violations <- syrian_archive %>%
  mutate(database_id = map_chr(db_urls, function(x){
    str_match(x, "[0-9]+") %>%
      as.character()
  })) %>%
  map_df(str_trim) %>%
  map_df(~ map_chr(.x, ~ ifelse(.x == "" || .x == "None", NA, .x))) %>%
  separate(location, c("city", "neighborhood"), sep = ":") %>%
  mutate(recording_date = sub(",", "", recording_date)) %>%
  separate(recording_date, c("recording_date", "recording_time"), sep = ",") %>%
  map_df(str_trim) %>%
  mutate(recording_date = sub("Sept", "Sep", recording_date)) %>%
  mutate(recording_date = mdy(recording_date)) %>%
  mutate(recording_time = map_chr(recording_time, ~
    if(.x == "noon"){
      "12:00 p.m."
    } else if(.x == "midnight"){
      "0:00 a.m."
    } else if(!grepl(":", .x)){ # A string like: "2 p.m."
      number <- as.character(str_extract(.x, "[0-9]{1,2}"))
      sub(number, paste0(number, ":00"), .x)
    } else {
      .x
    })) %>%
  separate(recording_time, c("recording_hour", "recording_minute", "recording_am_pm"), sep = "\\s|:") %>%
  mutate(recording_hour = as.numeric(recording_hour)) %>%
  mutate(recording_minute = as.numeric(recording_minute)) %>%
  mutate(recording_hour = map2_dbl(recording_hour, recording_am_pm, ~
    if(.y == "p.m." && .x != 12){
      (.x + 12) %% 24
    } else {
      .x
    })) %>%
  select(-recording_am_pm) %>%
  mutate(latitude = map_dbl(latitude, coord)) %>%
  mutate(longitude = map_dbl(longitude, coord))

# badll <- violations %>%
#   filter(grepl("[^0-9\\.]", latitude)) %>%
#   bind_rows(violations %>%
#               filter(grepl("[^0-9\\.]", longitude))) %>%
#   distinct() %>%
#   mutate(latitude = map_dbl(latitude, coord)) %>%
#   mutate(longitude = map_dbl(longitude, coord))

###############
# Single Page #
###############

base_url <- "https://syrianarchive.org"

page_urls <- paste0(base_url, "/database/?page=", 1:5)

get_db_ids <- . %>%
  read_html() %>%
  html_nodes("td") %>%
  html_nodes("a") %>%
  html_attr("href")

db_urls <- map(page_urls, get_db_ids) %>%
  unlist()

db_urls <- db_urls %>%
  paste0(base_url, .)

db_html <- map(db_urls, read_html)

new_page <- map_df(db_html, function(db_page){
  db_p <- db_page %>%
    html_nodes("p") %>%
    as_list()

  data_frame(description = tryCatch(db_p[[1]][[1]], error = function(e){NA}),
             location = db_p[[2]][[2]],
             latitude = db_p[[3]][[2]],
             longitude = db_p[[4]][[2]],
             reference_code = db_p[[6]][[2]],
             violation_type = db_p[[7]][[2]],
             recording_date = db_p[[8]][[2]],
             source = db_p[[9]][[2]],
             keywords = db_p[[10]][[2]],
             youtube_id = tryCatch(db_p[[11]]$a[[1]], error = function(e){NA}),
             landmarks = db_p[[12]][[2]],
             weather_in_media = db_p[[13]][[2]],
             languages = db_p[[14]][[2]],
             clothes_and_uniforms = db_p[[15]][[2]],
             sex = db_p[[16]][[2]],
             weapons_used = db_p[[17]][[2]],
             edited = db_p[[18]][[2]],
             file_size = db_p[[19]][[2]],
             device_used_to_record = db_p[[20]][[2]],
             media_content_type = db_p[[21]][[2]],
             chain_of_custody_notes = db_p[[22]][[2]],
             finding_aids = db_p[[23]][[2]],
             international_instrument = db_p[[24]][[2]],
             urls_and_news = db_p[[25]][[2]],
             related_incidents = db_p[[26]][[2]],
             # database_id = str_match(db_url, "[0-9]+") %>%
             #                 as.character(),
             page_title = db_page %>%
               html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "pagetitle", " " ))]') %>%
               html_text(),
             video_url = db_page %>%
               html_nodes("video") %>%
               html_attr("src")
  )
})

new_page$database_id <- map_chr(db_urls, function(x){
  str_match(x, "[0-9]+") %>%
    as.character()
})

new_page_violations <- new_page %>%
  mutate(database_id = map_chr(db_urls, function(x){
    str_match(x, "[0-9]+") %>%
      as.character()
  })) %>%
  map_df(str_trim) %>%
  map_df(~ map_chr(.x, ~ ifelse(.x == "" || .x == "None", NA, .x))) %>%
  separate(location, c("city", "neighborhood"), sep = ":") %>%
  mutate(recording_date = sub(",", "", recording_date)) %>%
  separate(recording_date, c("recording_date", "recording_time"), sep = ",") %>%
  map_df(str_trim) %>%
  mutate(recording_date = sub("Sept", "Sep", recording_date)) %>%
  mutate(recording_date = mdy(recording_date)) %>%
  mutate(recording_time = map_chr(recording_time, ~
                                    if(.x == "noon"){
                                      "12:00 p.m."
                                    } else if(.x == "midnight"){
                                      "0:00 a.m."
                                    } else if(!grepl(":", .x)){ # A string like: "2 p.m."
                                      number <- as.character(str_extract(.x, "[0-9]{1,2}"))
                                      sub(number, paste0(number, ":00"), .x)
                                    } else {
                                      .x
                                    })) %>%
  separate(recording_time, c("recording_hour", "recording_minute", "recording_am_pm"), sep = "\\s|:") %>%
  mutate(recording_hour = as.numeric(recording_hour)) %>%
  mutate(recording_minute = as.numeric(recording_minute)) %>%
  mutate(recording_hour = map2_dbl(recording_hour, recording_am_pm, ~
                                     if(.y == "p.m." && .x != 12){
                                       (.x + 12) %% 24
                                     } else {
                                       .x
                                     })) %>%
  select(-recording_am_pm) %>%
  mutate(latitude = map_dbl(latitude, coord)) %>%
  mutate(longitude = map_dbl(longitude, coord))

load("data/violations.rda")

violations <- new_page_violations %>%
  bind_rows(violations) %>%
  select_if(function(col) sum(!is.na(col)) > 50) %>%
  distinct()

colnames(violations) <- colnames(violations) %>%
  str_replace_all("_", " ") %>%
  str_to_title() %>%
  str_replace_all("Id", "ID") %>%
  str_replace_all("Url", "URL") %>%
  str_replace_all(" ", "_")

###############
###############

write_csv(violations, "data-raw/violations.csv")
use_data(violations, overwrite = TRUE)
