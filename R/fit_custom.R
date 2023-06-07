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

fit_custom <- function(x, y, k = NULL){
  if(is.null(k)){
    k <- length(x) - 1
  }
  
  # fit model
  mod <- gam(y ~ s(x, k = k))
  
  # Define the model equation
  model <- function(x) {
    new <- data.frame(x = x)
    y <- (unlist(predict(mod, newdata = new)))
    
    y <- ifelse(y < 0, 0, y)
    return(y)
  }
}
