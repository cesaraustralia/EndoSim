#' Fit Lactin model for development
#'
#' Fits a function that returns parasitoid wasp development rate based on daily temperature
#'
#' @param epsilon development rate scalar
#' @param tau development rate scalar
#' @param lambda development rate scalar
#' @param Tupper upper development threshold
#' @return function that accepts x and returns y based on fitted model
#' @details
#' A development rate function to pass on to the Parasitoid model.
#' Fits a curve using the Lactin model (Lactin et al. 1995):
#' \deqn{
#' e^{\epsilon \cdot T} - e^{\epsilon \cdot T_{\text{opt}} - \left(\frac{T_{\text{opt}} - T}{\tau}\right)} + \lambda
#' }
#' @keywords internal
#' @examples 
#' my_func <- EndoSim:::fit_lactin(0.13048, 7.64313, -0.01308, 35.47)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @references
#' Lactin, D.J., Holliday, N.J., Johnson, D.L., Craigen, R., Improved rate model of temperature-dependent development by arthropods. Environmental Entomology, 24, 68-75 (1995)
#' @seealso [fit_null()], [fit_wang()]

fit_lactin <- function(epsilon, tau, lambda, Tupper){
  
  func <- function(x){
    output <- exp(epsilon * x) - exp(epsilon * Tupper - ((Tupper - x) / tau)) + lambda
    output <- ifelse(output < 0, 0, output)
    return(output)
  }
  
  return(func)
}

