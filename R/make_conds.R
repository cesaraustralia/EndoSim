#' Create sim_conds
#'
#' Create sim_conds object with conditions for simulation
#'
#' @param start_date start date of simulation in YYYY-MM-DD format
#' @param end_date end date of simulation in YYYY-MM-DD format
#' @param lat latitude of site in decimal degrees
#' @param long longitude of site in decimal degrees
#' @param path pathway to SILO weather data. If null (default) data for the selected dates are downloaded
#' @return object of class \code{sim_conds} with the following arguments:
#' \itemize{
#'   \item env: dataframe containing mean temperature and rainfall for each day of the simulation
#'   \item sim_length: length of simulation in days
#'   \item start_date: start date of simulation in YYYY-MM-DD format
#' }
#' @seealso [download_silo()]
#' 
#' @export make_conds

make_conds <- function(start_date, end_date, lat, long, path = NULL){
  
  main.dir <- getwd()
  
  if(is.null(path)){
    silo_path <- paste0(file.path(main.dir, "temp"), "/")
  } else {
    silo_path <- paste0(file.path(main.dir, path), "/")
  }
  
  times <- seq(lubridate::ymd(start_date), lubridate::ymd(end_date), by = "day")
  
  sim_length <- length(times)
  
  if(is.null(path)){
    warning("No pathway selected; downloading")
    
    dir.create(file.path(main.dir, "temp"))
    
    for(i in 1:length(times)){
      for(k in c("max_temp", "min_temp", "daily_rain")){
        date = stringr::str_remove_all(sapply(strsplit(as.character(times[[i]]), " "), "[", 1), "-")
        year = lubridate::year(times[[i]])
        var = k
        
        EndosymbiontModel::download_silo(date, year, var, path = silo_path)
      }
    }
  }
  
  # sample environmental conditions
  dat <- dir(silo_path)
  
  dat_rain <- dat[which(stringr::str_detect(dat, "rain"))]
  dat_max_temp <- dat[which(stringr::str_detect(dat, "max_temp"))]
  dat_min_temp <- dat[which(stringr::str_detect(dat, "min_temp"))]
  
  env <- data.frame(t = 1:sim_length,
                    Temperature =
                      (unlist(sapply(1:sim_length,
                                     function(x)
                                       terra::extract(
                                         x = terra::rast(paste0(silo_path, dat_max_temp[x])),
                                         y = data.frame(x = long, y = lat),
                                         ID = FALSE
                                       ))) +
                         unlist(sapply(1:sim_length,
                                       function(x)
                                         terra::extract(
                                           x = terra::rast(paste0(silo_path, dat_min_temp[x])),
                                           y = data.frame(x = long, y = lat),
                                           ID = FALSE
                                         )))
                      )/2,
                    Precipitation =
                      unlist(sapply(1:sim_length,
                                    function(x)
                                      terra::extract(
                                        x = terra::rast(paste0(silo_path, dat_rain[x])),
                                        y = data.frame(x = long, y = lat),
                                        ID = FALSE
                                      )))
  )
  
  if(is.null(path))
    unlink(silo_path, recursive = TRUE)
  
  conds <- list(env = env,
                sim_length = sim_length,
                start_date = start_date)
  class(conds) <- "sim_conds"
  
  return(conds)
}