#' An S4 class to store a collection of simulations with the Endosymbiont model
#' 
#' @exportClass endosym_col
#'
#' @slot scenarios Dataframe defining which modules are active or deactivated in each simulated scenario
#' @slot sims List of objects of class [endosym_mod-class], each corresponding to a row of \code{scenarios}


methods::setClass("endosym_col",
                  slots = c(scenarios = "data.frame",
                            sims = "list")
)
