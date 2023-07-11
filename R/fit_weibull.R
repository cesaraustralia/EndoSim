#' Fit Weibull curve for fecundity
#'
#' Fits a function that returns daily fecundity based on cohort adult age
#'
#' @param K fecundity rate scalar
#' @param iota shape of fecundity curve
#' @param gamma scale of fecundity curve
#' @param eta locations of fecundity curve
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a curve using the Weibull distribution to describe how daily fecundity changes with adult age:
#' \deqn{
#' K \cdot \frac{\iota}{\gamma} \cdot  \left( \frac{\text{age} - \eta}{\gamma} \right)^{\iota - 1} \cdot e^{-\left( \frac{\text{age} - \eta}{\gamma} \right)^\iota}
#' }
#' @keywords internal
#' @examples 
#' my_func <- EndosymbiontModel:::fit_weibull(58, 1.885, 5.953, 0)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @references
#' Weibull, W., A statistical distribution function of wide applicability. Journal of Applied Mechanics, 18, 293-297 (1951)
#' @seealso [fit_bannerman()], [fit_null()], [fit_quadratic()]

fit_weibull <- function(K, iota, gamma, eta){
  
  func <- function(x){
    output <- K * (iota / gamma) * (((x - eta)/gamma)^(iota - 1)) * exp(-1 * (((x - eta)/gamma))^iota)
    output <- ifelse(is.na(output), 0, output)
    return(output)
  }
  
  return(func)
}
