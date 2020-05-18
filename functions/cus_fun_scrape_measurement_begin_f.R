cus_fun_scrape_measurement_begin <- function(url) {
  url %>% 
    read_html() %>% 
    html_node(xpath = "/html/body/div[3]/div/div/div/div[3]/div/div[2]/div[2]/div[2]/div/div[2]/dl/dd[3]") %>% 
    html_text()
}

