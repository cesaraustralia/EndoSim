#' An S4 class to store a parasitoid's life history for the endosymbiont model
#' 
#' @exportClass parasitoid
#'
#' @slot species The name of the pest
#' @slot fun_dev_para Function describing temperature-dependent development of parasitoids
#' @slot fun_para_scal Function to modify development rate
#' @slot fun_attack Function describing attack rate as function of temperature
#' @slot fun_handling Function describing handling rate as function of temperature
#' @slot susc_stage Numeric vector defining the susceptible lifestages of the pest to parasitoids
#' @slot introduction_date Date parasitoids are introduced in YYYY-MM-DD format
#' @slot introduction_n Number of adult parasitoids introduced (assumed to all be female)
#' 
#' @examples 
#' Wasp <- new("parasitoid",
#'             species = "Diaeretiella rapae",
#'             fun_dev_para = EndoSim:::fit_lactin(0.13048, 7.64313, -0.01308, 35.47),
#'             fun_para_scal = EndoSim:::fit_null(1),
#'             fun_attack = EndoSim:::fit_thirdpoly(0.00001084, -0.0021, -0.0272, -0.0145),
#'             fun_handling = EndoSim:::fit_thirdpoly(0.298, 0.0035, 0.000256, 1.395, inv = T),
#'             susc_stage = c(2, 3),
#'             introduction_date = "2024-04-17",
#'             introduction_n = 5)
#' 
#' Wasp

methods::setClass("parasitoid",
                  slots = c(species = "character",
                            fun_dev_para = "function",
                            fun_para_scal = "function",
                            fun_attack = "function",
                            fun_handling = "function",
                            susc_stage = "numeric",
                            introduction_date = "character",
                            introduction_n = "numeric")
)
