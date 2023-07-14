#' An S4 class to store simulation conditions for the endosymbiont model
#' 
#' @exportClass sim_conds
#'
#' @slot env dataframe containing temperature and precipitation per day of simulation
#' @slot sim_length length of simulation in days
#' @slot start_date first day of simulation in YYYY-MM-DD format
#' 

methods::setClass("sim_conds",
                  slots = c(env = "data.frame",
                            sim_length = "numeric",
                            start_date = "character")
)