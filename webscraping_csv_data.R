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



first_dates <- c("2000-01-01", "2000-12-31")



number_of_years <- 21

period_starting_dates <- tibble(
  from = seq.Date(as.Date(first_dates[1]),
                  by = "year",
                  length.out = number_of_years
  )
) %>% 
  mutate_all(cus_fun_date_format_to_german)



period_ending_dates <- tibble(
  to = seq.Date(as.Date(first_dates[2]),
                by = "year",
                length.out = number_of_years
  )
) %>% 
  mutate_all(cus_fun_date_format_to_german)

url_template <- "https://luftdaten.berlin.de/station/mc+++.csv?group=pollution&period=1h&timespan=custom&start%5Bdate%5D=__________&start%5Bhour%5D=00&end%5Bdate%5D=----------&end%5Bhour%5D=00"




urls_to_fetch <- map2_dfr(period_starting_dates,
                          period_ending_dates,
                          .f = ~cus_fun_fill_dates_in_url(url_template, .x, .y))


download <- urls_to_fetch %>% 
  pull(from) %>% 
  map(cus_fun_read_csv_from_url)

station_data <- download %>% 
  map(~slice(., -c(2:3))) %>% 
  map(~set_names(., as_vector(slice(., 1)))) %>% 
  map(~slice(., -1))

columns_to_keep <- station_data %>% 
  map(~names(.)) %>% 
  Reduce(intersect, .)

station_data <- station_data %>% 
  map_dfr(~select(., one_of(columns_to_keep))) %>% 
  rename("Messzeit" = 1) %>% 
  janitor::clean_names()

station_data <- station_data %>% 
  mutate(date = dmy_hm(date))

station_data <- station_data %>%
  distinct(date, .keep_all = TRUE)
  


station_data %>% tail(10)
