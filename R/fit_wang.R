#' Fit Wang model for development
#'
#' Fits a function that returns aphid nymph development rate based on daily temperature
#'
#' @param H development rate scalar
#' @param r development rate scalar
#' @param Topt optimum development temperature
#' @param Tupper upper development threshold
#' @param Tlower lower development threshold
#' @param bound boundary width of thresholds
#' @return function that accepts x and returns y based on fitted model
#' @details
#' A development rate function to pass on to the Endosymbiont model.
#' Fits a curve using the Wang model (Wang et al. 1982):
#' \deqn{
#' \left(\frac{H}{{1 + e^{-r \cdot (T - T_{\text{opt}})}}}\right) \cdot \left(1 - e^{- \frac{T - T_{\text{lower}}}{\text{bound}}}\right) \cdot \left(1 - e^{- \frac{T_{\text{upper}} - T}{\text{bound}}}\right)
#' }
#' @keywords internal
#' @examples 
#' my_func <- EndosymbiontModel:::fit_wang(24.1, 0.152, 18.09, 32.79, 2.84, 1.8977)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @references
#' Wang, R.S., Lan, Z.X., Ting, Y.C., Studies on mathematical models of the relationships between insect development and temperature. Acta Ecologica Sinica, 2, 47-57 (1982)
#' @seealso [fit_null()]

fit_wang <- function(H, r, Topt, Tupper, Tlower, bound){
  
  func <- function(x){
    output <- (H / (1 + exp(-1 * r * (x - Topt)))) * (1 - exp(-1*(x - Tlower)/bound)) * (1 - exp(-1*(Tupper - x)/bound)) / 100
    output <- ifelse(output < 0, 0, output)
    return(output)
  }
  
  return(func)
}
