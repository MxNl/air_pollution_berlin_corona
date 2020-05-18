cus_fun_fill_dates_in_url <- function(url, start, end){
  url <- url %>% 
    str_replace("__________", start) %>% 
    str_replace("----------", end)
  
  return(url)
}
