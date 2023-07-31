#' Fit Thackray model for emigration
#'
#' Fits a function that returns number of emigrating adults
#'
#' @param x population of newly emerged adults
#' @return number of emigrating adults
#' @details
#' A base emigration rate of 20% of newly emerged adults
#' @keywords internal
#' @examples 
#' my_func <- EndoSim:::fit_emi_thack()
#' my_func(runif(10, 1, 100))
#' @references
#' Thackray, D.J., Diggle, A.J., Berlandier, F.A., Jones, R.A.C., Forecasting aphid outbreaks and epidemics of Cucumber mosaic virus in lupin crops in a Mediterranean-type environment. Virus Research, 100, 67-82 (2004)
#' @seealso [fit_null()]

fit_emi_thack <- function(){
  
  func <- function(x) {
    output <- round(0.2 * x, 0)
    return(output)
    }
  
  return(func)
}
