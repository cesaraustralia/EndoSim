#' Download SILO data
#'
#' Download weather station data from SILO
#'
#' @param date date to download data for in YYYY-MM-DD format
#' @param year year of date
#' @param var name of variable to download (e.g. max_temp, min_temp, daily_rain)
#' @param path pathway to output directory
#' @return raster of selected weather data variable for selected date
#' @seealso [make_conds()]
#' 
#' @export download_silo

download_silo <- function(date, year, var, path){
  utils::download.file(url = paste0("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/daily/",
                                    var, "/",
                                    year, "/",
                                    date, ".",
                                    var, ".tif"),
                       method = "curl",
                       destfile = paste0(path, date, ".", var, ".tif"),
                       quiet = T) 
}