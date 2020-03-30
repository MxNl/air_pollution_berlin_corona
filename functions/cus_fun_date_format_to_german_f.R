cus_fun_date_format_to_german <- function(x){
  paste(str_pad(day(x), 2, side = "left", pad = "0"), 
        str_pad(month(x), 2, side = "left", pad = "0"), 
        year(x), 
        sep = ".")
}