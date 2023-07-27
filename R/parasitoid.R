#' An S4 class to store a parasitoid's life history for the endosymbiont model
#' 
#' @exportClass parasitoid
#'
#' @slot species The name of the pest
#' @slot fun_dev_para Function describing temperature-dependent development of parasitoids
#' @slot fun_para_scal Function to modify development rate
#' @slot fun_attack Function describing attack rate as function of temperature
#' @slot fun_handling Function describing handling rate as function of temperature
#' 
#' @examples 
#' Wasp <- new("parasitoid",
#'             name = "Diaeretiella rapae",
#'             fun_dev_para = EndosymbiontModel:::fit_lactin(0.13048, 7.64313, -0.01308, 35.47),
#'             fun_para_scal = EndosymbiontModel:::fit_null(1),
#'             fun_attack = EndosymbiontModel:::fit_thirdpoly(0.00001084, -0.0021, -0.0272, -0.0145),
#'             fun_handling = EndosymbiontModel:::fit_thirdpoly(0.298, 0.0035, 0.000256, 1.395, inv = T))
#' 
#' Wasp

methods::setClass("parasitoid",
                  slots = c(species = "character",
                            fun_dev_para = "function",
                            fun_para_scal = "function",
                            fun_attack = "function",
                            fun_handling = "function")
)
