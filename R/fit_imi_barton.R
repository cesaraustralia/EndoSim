#' Fit model for immigration
#'
#' Fits a function that returns number of immigrating adults
#'
#' @param t time step
#' @return number of immigrating adults
#' @details
#' A base immigration rate of 5 adult alates from a background population
#' @keywords internal
#' @examples 
#' my_func <- EndosymbiontModel:::fit_imi_barton()
#' my_func(runif(10, 1, 100))
#' @references
#' Barton, M., Parry, H., Ward, S., Hoffmann, A.A., Umina, P.A., van Helden, M., Macfadyen, S. Forecasting impacts of biological control under future climates: mechanistic modelling of an aphid pest and a parasitic wasp. Ecological Modelling, 457, 109679 (2021)
#' @seealso [fit_null()]

fit_imi_barton <- function(){
  
  func <- function(x) {
    return(5)
    }
  
  return(func)
}
