#' An S4 class to store a species' life history and functions for the endosymbiont model
#'
#' @slot name The name of the species
#' @slot reproduction The form of reproduction the species has ("sexual", "asexual")
#' @slot development The form of development of the species ("holometabolous", "hemimetabolous)
#' @slot f_growth Function for growth rate ~ temperature
#' @slot f_loss Function for mortality rate ~ rainfall
#' @slot f_suscept Function for infection susceptability ~ temperature
#' @slot f_inoc Function for inoculation efficiency ~ temperature
#' @slot f_alate Function for production of alates ~ proportion of carrying capacity (density)
#' 
#' @examples 
#' GPA <-
#' new("species",
#'   name = "Myzus persicae",
#'   reproduction = "asexual",
#'   development = "hemimetabolous",
#'   f_growth = EndosymbiontModel:::fit_tpc("custom",
#'     c(4, 8.87, 11.82, 14.22, 17.13, 19.92, 22.61, 25.77, 27.80, 30),
#'     c(0, 0.06, 0.09, 0.11, 0.13, 0.15, 0.16, 0.14, 0.04, 0)),
#'   f_loss = EndosymbiontModel:::fit_sigmoid(0.55, 1.5, 3.5),
#'   f_suscept = EndosymbiontModel:::fit_tpc("custom",
#'     seq(0, 40, 4)[1:10],
#'     c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.75, 0.6, 0.3, 0)),
#'  f_inoc = EndosymbiontModel:::fit_tpc("quadratic", 0.8, 22, 0, 40),
#'  f_alate = EndosymbiontModel:::fit_tpc("custom",
#'   seq(0, 1, 0.1),
#'   c(0.01, 0.22, 0.35, 0.45, 0.53, 0.6, 0.66, 0.72, 0.76, 0.78, 0.79))
#'  )
#' 
#' GPA

species <- methods::setClass("species",
                                  representation(name = "character",
                                                 reproduction = "character",
                                                 development = "character",
                                                 f_growth = "function",
                                                 f_loss = "function",
                                                 f_suscept = "function",
                                                 f_inoc = "function",
                                                 f_alate = "function"),
                             validity = EndosymbiontModel:::check_species
)
