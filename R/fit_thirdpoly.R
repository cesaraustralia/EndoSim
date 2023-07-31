#' Fit third-order polynomial regression
#'
#' Fits a function that returns a third-order polynomial regression
#'
#' @param a coefficient of the cubic term (x^3)
#' @param b coefficient of the quadratic term (x^2)
#' @param c coefficient of the linear term (x)
#' @param d constant term
#' @param inv if \code{TRUE} return inverse
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a third-order polynomial regression:
#' \deqn{
#' a \cdot x^3 + b \cdot x^2 + c \cdot x + d
#' }
#' @keywords internal
#' @examples 
#' my_func <- EndoSim:::fit_thirdpoly(0.00001084, -0.0021, -0.0272, -0.0145)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @seealso [fit_null()]

fit_thirdpoly <- function(a, b, c, d, inv = F){
  
  func <- function(x){
    output <- a * x^3 + b * x^2 + c * x + d
    if(inv) output <- 1 / output
    return(output)
  }
  
  return(func)
}
