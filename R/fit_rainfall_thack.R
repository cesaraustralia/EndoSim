#' Fit Thackray model for rainfall-induced mortality
#'
#' Fits a function that returns mortality rate based on daily rainfall
#'
#' @param x daily rainfall in mm
#' @return mortality rate
#' @details
#' A mortality rate function to pass on to the Endosymbiont model.
#' Fits a curve using the Thackray definition of rainfall-induced mortality in aphids (Thackray et al. 2004, modified for green peach aphid by Maling et al. 2010):
#' \deqn{
#' \text{{Rainfall Mortality}} = 0.026 \cdot \text{{Rainfall}} - 0.33
#' }
#' 
#' Where mortality increases linearly as daily rainfall increases from 15 mm and is capped at a maximum of 0.5.
#' 
#' @keywords internal
#' @examples 
#' my_func <- EndosymbiontModel:::fit_rainfall_thack()
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @references
#' Thackray, D.J., Diggle, A.J., Berlandier, F.A., Jones, R.A.C., Forecasting aphid outbreaks and epidemics of Cucumber mosaic virus in lupin crops in a Mediterranean-type environment. Virus Research, 100, 67-82 (2004)
#' 
#' Maling, T., Diggle, A.J., Thackray, D.J., Siddique, K.H.M., Jones, R.A.C. An epidemiological model for externally acquired vector-borne viruses applied to \emph{Beet western yellows virus} in \emph{Brassica napus} crops in a Mediterranean-type environment. Crop and Pasture Science, 61, 132-144 (2010)
#' @seealso [fit_gompertz()], [fit_null()]

fit_rainfall_thack <- function(){
  
  func <- function(x){
    output <- (0.026 * x) - 0.33
    output <- ifelse(output < 0, 0, ifelse(output > 0.5, 0.5, output))
    
    return(output)
  }
  
  return(func)
}
