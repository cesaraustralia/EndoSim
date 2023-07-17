#' An S4 class to store a crop's life history for the endosymbiont model
#' 
#' @exportClass crop
#'
#' @slot name The name of the crop
#' @slot heal_time Time in days endosymbiont remains in plant
#' @slot fun_reinf Function describing probability of plant reinfection as function of R+ pest density
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
                            fun_reinf = "function",
                            sowing_date = "character",
                            harvest_date = "character")
)
