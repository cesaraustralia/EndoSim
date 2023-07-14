#' An S4 class to store a crop's life history for the endosymbiont model
#' 
#' @exportClass crop
#'
#' @slot name The name of the crop
#' @slot heal_time Time in days endosymbiont remains in plant
#' @slot sowing_date Date crop is sown in YYYY-MM-DD format
#' @slot harvest_date Date crop is harvested in YYYY-MM-DD format
#' 
#' @examples 
#' canola <-
#' new("crop",
#' name = "Canola",
#' heal_time = 3)
#' 
#' canola

methods::setClass("crop",
                  slots = c(name = "character",
                            heal_time = "numeric",
                            sowing_date = "character",
                            harvest_date = "character")
)
