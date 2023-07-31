#' Fit Bannerman & Roitberg function for fecundity
#'
#' Fits a function that returns scaled daily fecundity based on population density
#'
#' @param D carrying capacity (inflection point)
#' @param p rate of decline in fecundity
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a curve based on Bannerman & Roitberg (2014) to describe how daily fecundity changes with density:
#' \deqn{
#' 1 - \frac{1}{1 + e^{-p \cdot (\text{density} - D)}}
#' }
#' @keywords internal
#' @examples 
#' my_func <- EndoSim:::fit_bannerman(10000, 0.0008)
#' plot(seq(0, 15000, by = 1), my_func(seq(0, 15000, by = 1)), type = "l")
#' @references
#' Bannerman, J,A., Roitberg, B.D., Impact of extreme and fluctuating temperatures on aphid-parasitoid dynamics. Oikos, 123, 89-98 (2014)
#' @seealso [fit_null()], [fit_quadratic()], [fit_weibull()]

fit_bannerman <- function(D, p){
  
  func <- function(x){
    output <- 1 - (1 / (1 + exp(-1 * p * (x - D))))
    return(output)
  }
  
  return(func)
}
