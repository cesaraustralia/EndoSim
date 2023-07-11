#' Fit Gompertz survival function
#'
#' Fits a function that returns mortality rate based on an inverse Gompertz survival function
#'
#' @param delta shape of mortality curve
#' @param beta rate of decline
#' @return function that accepts x and returns y based on fitted model
#' @details
#' A mortality rate function to pass on to the Endosymbiont model.
#' Fits an a Gomeprtz survival function to calculate mortality rate:
#' \deqn{
#' 1 - e^{\left(\frac{\delta}{\beta}\left(1 - e^{\beta \cdot \text{{age}}}\right)\right)}
#' }
#' 
#' @keywords internal
#' @examples 
#' my_func <- EndosymbiontModel:::fit_gompertz(0.00188, 0.13)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @references
#' Gompertz, B., On the nature of the function expressive of the law of human mortality and on a new mode of determining the value of life contingencies. Philosophical Transactions of the Royal Society of London, 115, 513-583 (1825)
#' @seealso [fit_null()], [fit_rainfall_thack()]

fit_gompertz <- function(delta, beta){
  
  fun <- function(x){
    # output <- 1 - exp((delta/beta)*(1 - exp(beta * x)))
    output <- (delta * exp(beta * x)) * exp(-1 * (delta/beta)*(exp(beta * x) - 1))
    output <- ifelse(output > 1, 1, output)
    return(output)
  }
  
  return(fun)
}

