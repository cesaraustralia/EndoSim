#' Fit gaussian curve
#'
#' Fits a function that returns value based on daily temperature
#'
#' @param rmax maxmimum rate at optimum temperature
#' @param topt optimum reproductive temperature
#' @param a width parameter
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a Gaussian curve to produce a temperature scalar ranging from 0 to rmax:
#' \deqn{
#'   r_{max} \cdot e(-0.5 \cdot (\frac{|x - t_{opt}|}{a}))^2)
#' }
#' @keywords internal
#' @examples
#' my_func <- EndoSim:::fit_gaussian(1, 25, 3)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @seealso [fit_custom()], [fit_null()], [fit_quadratic()], [fit_rezende()]

fit_gaussian <- function(rmax, topt, a){
  
  func <- function(x){
    output <- rmax * exp(-0.5 * (abs(x - topt)/a)^2)
    output <- ifelse(output < 0, 0, output)
    return(output)
  }
  
  return(func)
}

