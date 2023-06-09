#' Fit custom curve
#'
#' Fits a curve using a generalised additive model
#'
#' @param x vector of numeric predictor variable
#' @param y vector of numeric response variable, of same length as x
#' @param k passed onto smooth term of x. Default is length(x) - 1
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a generalised additive model (GAM) using the mgcv package.
#' @keywords internal
#' @examples
#' my_func <- EndosymbiontModel:::fit_custom(
#' c(4, 8.87, 11.82, 14.22, 17.13, 19.92, 22.61, 25.77, 27.80, 30),
#' c(0, 0.06, 0.09, 0.11, 0.13, 0.15, 0.16, 0.14, 0.04, 0)
#' )
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @seealso [fit_briere()], [fit_sigmoid()], [fit_gaussian()], [fit_quadratic()], [fit_rezende()], [fit_weibull()]

fit_custom <- function(x, y, k = NULL){
  if(is.null(k)){
    k <- length(x) - 1
  }
  
  # fit model
  mod <- mgcv::gam(y ~ s(x, k = k))
  
  # Define the model equation
  model <- function(x) {
    new <- data.frame(x = x)
    y <- as.numeric(unlist(mgcv::predict.gam(mod, newdata = new)))
    
    y <- ifelse(y < 0, 0, y)
    return(y)
  }
}
