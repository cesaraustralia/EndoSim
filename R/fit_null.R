#' Fit null
#'
#' Fits a function that returns 0 for any given input value
#'
#' @param x vector of any class
#' @param output which value to return (default is 0)
#' @return function that accepts x and returns the value of \code{output}
#' @details
#' A function to pass on to the Endosymbiont model which can be used to nullify any process. The default output is 0, and therefore can be used to stand-in for any process that is modelled as a function accepting an input vector. However, the output can be changed to a different value (e.g., 1) if required, as long as the null process is supposed to return a constant value.
#' @keywords internal
#' @examples 
#' my_func <- EndoSim:::fit_null(0)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' 
#' my_func <- EndoSim:::fit_null(1)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' 
#' my_func <- EndoSim:::fit_null(0.5)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @seealso [fit_custom()]

fit_null <- function(output = 0){
  
  func <- function(x){
    return(rep(output, length(x)))
  }
  
  return(func)
}
