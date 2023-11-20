#' Fit quadratic curve for fecundity
#'
#' Fits a function that returns scaled daily fecundity based on daily temperature
#'
#' @param a coefficient 1
#' @param b coefficient 2
#' @param topt optimum reproductive temperature
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a scaled quadratic curve to produce a temperature scalar ranging from 0 to 1:
#' \deqn{
#'   \frac{a \cdot x^2 + b \cdot x}{a \cdot T_{opt}^2 + b \cdot T_{opt}}
#' }
#' @keywords internal
#' @examples
#' my_func <- EndoSim:::fit_quadratic(-0.7611, 31.9847, 25)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @seealso [fit_bannerman()], [fit_null()], [fit_weibull()]

fit_quadratic <- function(a, b, topt){
  
  func <- function(x){
    output <- (a*x^2 + b*x)/(a*topt^2 + b*topt)
    output <- ifelse(output < 0, 0, output)
    return(output)
  }
  
  # Scale to range 0-1
  max.y <- max(func(seq(-50, 50, by = 0.1)))
  min.y <- min(func(seq(-50, 50, by = 0.1)))
  
  scale_values <- function(x) {
    (x-min.y) / (max.y-min.y)
  }
  
  func <- function(x){
    output <- (a*x^2 + b*x)/(a*topt^2 + b*topt)
    output <- ifelse(output < 0, 0, output)
    return(scale_values(output))
  }
  
  return(func)
}

