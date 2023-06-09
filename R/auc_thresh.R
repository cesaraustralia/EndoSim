#' Area between thresholds
#'
#' Calculate the area of a curve between thresholds of y values
#'
#' @param x numeric vector of x values
#' @param y numeric vector of y values
#' @param min_val lower threshold of y
#' @param max_val upper threshold of y
#' @return a vector consisting of two arguments: the full area under the curve, and the area under the curve between the threshold values
#' @details
#' This function uses smooth splines to fit a curve defined by vectors of x and y values. It then integrates across the curve to find the area under the curve, and between two threshold values of y.
#' @keywords internal
#' @examples
#' temp_auc <- EndosymbiontModel:::auc_thresh(seq(0,50), sin(seq(0,50)), 0, 0.5)
#' temp_auc

auc_thresh <- function(x, y, min_val, max_val) {
  
  min_y <- min(c(y, min_val))
  
  # normalise y and min_val, max_val
  y_norm <- y - min_y
  min_norm <- min_val - min_y
  max_norm <- max_val - min_y
  
  # calculate area under curve for full range
  spline_fit <- stats::smooth.spline(x, y_norm)
  auc_full <- stats::integrate(function(z) predict(spline_fit, z)$y, min(x), max(x))$value
  
  # define function to integrate above min threshold
  f_above_min_thresh <- function(z, spline_fit, thresh) {
    y_pred <- stats::predict(spline_fit, z)$y - thresh
    ifelse(y_pred > 0, y_pred, 0)
  }
  
  # calculate area above min threshold
  auc_min <- stats::integrate(f_above_min_thresh, min(x), max(x), spline_fit = spline_fit, thresh = min_norm)$value
  
  # define function to integrate above max threshold
  f_above_max_thresh <- function(z, spline_fit, thresh) {
    y_pred <- stats::predict(spline_fit, z)$y - thresh
    ifelse(y_pred > 0, y_pred, 0)
  }
  
  # calculate area above max threshold
  auc_max <- stats::integrate(f_above_max_thresh, min(x), max(x), spline_fit = spline_fit, thresh = max_norm)$value
  
  # return auc_full and difference between auc_min and auc_max
  return(c(auc_full, auc_min - auc_max))
}