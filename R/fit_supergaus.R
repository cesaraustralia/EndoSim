#' Fit super-gaussian function for mortality
#'
#' Fits a function that returns aphid mortality rate based on divergence of maximum and minimum temperatures from development thresholds
#'
#' @param Tupper upper development threshold
#' @param Tlower lower development threshold
#' @param Piupper mortality rate beyond upper threshold
#' @param Pilower mortality rate beyond lower threshold
#' @param Psi survival breadth parameter
#' @param Chi survival breadth parameter
#' @return function that accepts Tmax and Tmin and returns y based on fitted model
#' @details
#' A temperature-dependent mortality rate function to pass on to the Endosymbiont model.
#' Fits a super-gaussian function:
#' \deqn{
#'   1 - e^ ( -\Pi_{\text{upper}} \cdot (\chi \cdot (\frac{T_{\text{max}} - T_{\text{upper}}}{\Psi})^2) -\Pi_{\text{lower}} \cdot (\chi \cdot (\frac{T_{\text{lower}} - T_{\text{min}}}{\Psi})^2) )
#' }
#' @keywords internal
#' @examples 
#' my_func <- EndoSim:::fit_supergaus(32.79, 2.84, 0.00083, 0.00017, 7.66, 17.66)
#' plot(seq(-10, 50, by = 0.1), sapply(seq(-10, 50, by = 0.1), function(x) my_func(x, x)), type = "l")
#' @seealso [fit_null()]

fit_supergaus <- function(Tupper, Tlower, Piupper, Pilower, Psi, Chi){
  
  func <- function(Tmax, Tmin){
    if(Tmax > Tupper)
      max_comp <- -Piupper * (Chi*(Tmax - Tupper)/Psi)^3
    else
      max_comp <- 0
    
    
    if(Tmin < Tlower)
      min_comp <- -Pilower * (Chi*(Tlower - Tmin)/Psi)^3
    else
      min_comp <- 0
    
    output <- 1 - exp(max_comp + min_comp)
    
    return(output)
  }
  
  return(func)
}

