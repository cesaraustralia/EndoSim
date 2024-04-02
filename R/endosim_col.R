#' An S4 class to store a collection of simulations with the endosymbiont model
#' 
#' @exportClass endosim_col
#'
#' @slot scenarios Dataframe defining which modules are active or deactivated in each simulated scenario
#' @slot sims List of objects of class [endosim_mod-class], each corresponding to a row of \code{scenarios}


methods::setClass("endosim_col",
                  slots = c(scenarios = "data.frame",
                            sims = "list")
)
