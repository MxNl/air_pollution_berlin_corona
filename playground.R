library(rvest)
library(lubridate)
library(tidyverse)

airquality_website <- read_html("https://luftdaten.berlin.de/lqi")

stations <- airquality_website %>%
  html_nodes(".lmn-table__cell--contentAsResponsiveHeadline") %>%
  html_text() %>%
  # tibble(text = .) %>%
  str_remove_all(" ") %>%
  # str_squish() %>%
  str_replace_all("\\n+", "_") %>%
  str_sub(2) %>%
  str_sub(end = -2) %>%
  tibble(station = .) %>%
  separate(col = "station", into = c("station", "type", "description"), sep = "_") %>%
  mutate(station_id = str_sub(station, 1, 3)) %>%
  mutate(station = str_sub(station, 4)) %>%
  select(station_id, everything())

station_websites <- stations %>%
  mutate(site_url = str_c("https://luftdaten.berlin.de/station/mc", station_id)) %>%
  select(site_url)


first_dates <- c("2010-01-01", "2010-12-31")

cus_fun_date_format_to_german <- function(x){
  paste(str_pad(day(x), 2, side = "left", pad = "0"), 
        str_pad(month(x), 2, side = "left", pad = "0"), 
        year(x), 
        sep = ".")
}

period_starting_dates <- tibble(
  from = seq.Date(as.Date(first_dates[1]),
    by = "year",
    length.out = 3
  )
) %>% 
  mutate_all(cus_fun_date_format_to_german)

period_ending_dates <- tibble(
  to = seq.Date(as.Date(first_dates[2]),
    by = "year",
    length.out = 3
  )
) %>% 
  mutate_all(cus_fun_date_format_to_german)

url_template <- "https://luftdaten.berlin.de/station/mc010.csv?group=pollution&period=1h&timespan=custom&start%5Bdate%5D=__________&start%5Bhour%5D=00&end%5Bdate%5D=----------&end%5Bhour%5D=00"


cus_fun_fill_dates_in_url <- function(url, start, end){
  url <- url %>% 
    str_replace("__________", start) %>% 
    str_replace("----------", end)
  
  return(url)
}

urls_to_fetch <- map2_dfr(period_starting_dates,
                          period_ending_dates,
                          .f = ~cus_fun_fill_dates_in_url(url_template, .x, .y))

column_headers <- urls_to_fetch %>% 
  slice(1) %>% 
  pull(from) %>% 
  read_csv2(locale = locale(decimal_mark = ".", grouping_mark = ",")) %>% 
  slice(1) %>%
  pivot_longer(cols = everything()) %>%
  pull(value)


cus_fun_read_csv_from_url <- function(url){
  ####################### TEst
  # url <- urls_to_fetch %>% 
  #   slice(3) %>% 
  #   pull(from)
  #############################
  url %>% 
    read_csv2(locale = locale(decimal_mark = ".", grouping_mark = ","),
              skip = 3) %>% 
    return()
}

data <- urls_to_fetch %>% 
  pull(from) %>% 
  map_dfr(cus_fun_read_csv_from_url)

data <-   

