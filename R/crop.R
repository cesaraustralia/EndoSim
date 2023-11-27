#' An S4 class to store a crop's life history for the endosymbiont model
#' 
#' @exportClass crop
#'
#' @slot name The name of the crop
#' @slot heal_time Half-time in days for plant recovery from endosymbiont infection
#' @slot sowing_date Date crop is sown in YYYY-MM-DD format
#' @slot harvest_date Date crop is harvested in YYYY-MM-DD format
#' 
#' @examples 
#' canola <-
#' new("crop",
#' name = "Canola",
#' heal_time = 2,
#' sowing_date = "2022-04-01",
#' harvest_date = "2022-11-01")
#' 
#' canola

methods::setClass("crop",
                  slots = c(name = "character",
                            heal_time = "numeric",
                            sowing_date = "character",
                            harvest_date = "character")
)
