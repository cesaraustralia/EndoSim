#' An S4 class to store a crop's life history for the endosymbiont model
#' 
#' @exportClass crop
#'
#' @slot name The name of the crop
#' @slot heal_time Half-time in days for plant recovery from endosymbiont infection
#' @slot sowing_date Date crop is sown in YYYY-MM-DD format
#' @slot emergence_date Date crop emerges in YYYY-MM-DD format
#' @slot harvest_date Date crop is harvested in YYYY-MM-DD format
#' @slot carrying_capacity Function defining proportion of pest carrying capacity based on days after emerging
#' @slot density Crop density (m2)
#' 
#' @examples 
#' canola <-
#' new("crop",
#' name = "Canola",
#' heal_time = 2,
#' sowing_date = "2022-04-05",
#' emergence_date = "2022-04-10",
#' harvest_date = "2022-11-01",
#' carrying_capacity = EndoSim:::fit_sigmoid(1, 0.1, 80),
#' density = 35)
#' 
#' canola

methods::setClass("crop",
                  slots = c(name = "character",
                            heal_time = "numeric",
                            sowing_date = "character",
                            emergence_date = "character",
                            harvest_date = "character",
                            carrying_capacity = "function",
                            density = "numeric")
)
