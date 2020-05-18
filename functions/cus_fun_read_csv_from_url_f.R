cus_fun_read_csv_from_url <- function(url){
  ####################### TEst
  # url <- urls_to_fetch %>% 
  #   slice(3) %>% 
  #   pull(from)
  #############################
  url %>% 
    read_csv2(locale = locale(decimal_mark = ".", grouping_mark = ",")) %>% 
    return()
}
