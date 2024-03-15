#' Fit sigmoidal curve
#'
#' Fits a sigmoidal growth function that returns value based on day after emergence
#'
#' @param K upper asymptote
#' @param B growth rate
#' @param M time of maximum growth
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a sigmoidal growth function to produce a value ranging from 0 to K:
#' \deqn{
#'   \frac{K}{1 + e^{-B \cdot \left(x - M \right)}}
#' }
#' @keywords internal
#' @examples
#' my_func <- EndoSim:::fit_sigmoid(1, 0.1, 10)
#' plot(seq(0, 200), my_func(seq(0, 200)), type = "l")
#' @seealso [fit_custom()]

fit_sigmoid <- function(K, B, M){
  
  func <- function(x){
    output <- K / (1 + exp(-B * (x - M)))
    output <- ifelse(output < 0, 0, output)
    return(output)
  }
  
  return(func)
}