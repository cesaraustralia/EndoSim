#' An S4 class to store a endosymbiont's life history for the endosymbiont model
#' 
#' @exportClass endosym
#'
#' @slot name The name of the endosymbiont
#' @slot fitness_cost Relative fitness cost of R+ phenotype (0-1)
#' @slot fun_trans_eff Function describing temperature-dependent efficiency of endosymbiont transmission to plants
#' @slot fun_susc Function describing temperature-dependent susceptibility of R- pests to infection from plants
#' 
#' @examples 
#' Rickettsiella <-
#'   new(
#'     "endosym",
#'     name = "Rickettsiella",
#'     fitness_cost = 0.5,
#'     fun_trans_eff = EndoSim:::fit_custom(seq(0, 36, 4),
#'       c(0, 0.6, 0.7, 0.75, 0.8, 0.8, 0.75, 0.6, 0.3, 0),
#'       k = 4),
#'     fun_susc = EndoSim:::fit_custom(seq(0, 40, 4),
#'       c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.75, 0.6, 0.3, 0, 0),
#'       k = 7)
#'     )
#' 
#' Rickettsiella

methods::setClass("endosym",
                  slots = c(name = "character",
                            fitness_cost = "numeric",
                            fun_trans_eff = "function",
                            fun_susc = "function")
)
