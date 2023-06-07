library(tidyverse)
library(lubridate)

download_silo <- function(date, year, var, path){
  download.file(url = paste0("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/daily/",
                             var, "/",
                             year, "/",
                             date, ".",
                             var, ".tif"),
                method = "curl",
                destfile = paste0(path, date, ".", var, ".tif")) 
}

times <- seq(ymd("2022-01-01"), ymd("2022-12-31"), by = "day")

for(i in 1:length(times)){
  for(k in c("max_temp", "min_temp", "daily_rain")){
    date = stringr::str_remove_all(sapply(strsplit(as.character(times[[i]]), " "), "[", 1), "-")
    year = year(times[[i]])
    var = k

    download_silo(date, year, var, "Data/silo/")
  }
}
