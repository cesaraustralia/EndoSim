#' Fit alate production function
#'
#' Fits a function that returns proportion of offspring destined to develop into alates
#'
#' @param omega inflection point of logistic regression
#' @param z logistic regression gradient parameter
#' @param W logistic regression gradient parameter
#' @param V logistic regression gradient parameter
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a logistic regression curve to determine the proportion of offspring destined to become alate adults, based on a function defined for cherry-oat aphids (\emph{Rhopalosiphum padi}; Morgan 2000; Parry et al. 2006)
#' \deqn{
#' \frac{z}{1 + e^{-W \cdot \left( \frac{\text{density}}{V} - \omega \right)}}
#' }
#' @keywords internal
#' @examples 
#' my_func <- EndosymbiontModel:::fit_alate(67.418, 0.993, 0.076, 300)
#' plot(seq(0, 15000, by = 1), my_func(seq(0, 15000, by = 1)), type = "l")
#' @references
#' Morgan, D., Population dynamics of the bird cherry-oat aphid, \emph{Rhopalosiphum padi} (L.), during the autumn and winter: a modelling approach. Agricultural and Forest Entomology, 2, 297-304 (2000)
#' Parry, H.R., Evans, A.J., Morgan, D., Aphid population response to agricultural landscape change: a spatially explicit, individual-based model. Ecological Modelling, 199, 451-463 (2006)
#' @seealso [fit_null()]

fit_alate <- function(omega, z, W, V){
  
  func <- function(x){
    output <- z / (1 + exp(-1 * W * ((x / V) - omega)))
    output <- ifelse(output > 0.5, 0.5, output)
    return(output)
  }
  
  return(func)
}
