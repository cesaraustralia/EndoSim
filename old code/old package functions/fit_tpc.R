#' Fit thermal performance curve
#'
#' Fits a thermal performance curve using selected model
#'
#' @param model one of "briere", "custom", "gaussian", "quadratic", "rezende", "weibull"
#' @param ... additional arguments of selected model
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a curve using the selected model.
#' If "custom" requires vectors of x and y values.
#' For all other models requires values of ymax, xopt, xmin, and xmax and optimises parameters of selected model based on input arguments.
#' @keywords internal
#' @examples
#' my_func1 <- EndoSim:::fit_tpc("briere", 0.16, 22.61, 4, 30)
#' plot(seq(0, 50, by = 0.1), my_func1(seq(0, 50, by = 0.1)), type = "l")
#' 
#' my_func2 <- EndoSim:::fit_tpc("custom",
#' c(4, 8.87, 11.82, 14.22, 17.13, 19.92, 22.61, 25.77, 27.80, 30),
#' c(0, 0.06, 0.09, 0.11, 0.13, 0.15, 0.16, 0.14, 0.04, 0))
#' plot(seq(0, 50, by = 0.1), my_func2(seq(0, 50, by = 0.1)), type = "l")
#' @seealso [fit_briere()], [fit_custom()], [fit_gaussian()], [fit_quadratic()], [fit_rezende()], [fit_weibull()]

fit_tpc <- function(model, ...){
  if(is.null(model))
    stop("Model must be selected")
  
  if (model == "briere")
    fit_briere(...)
  else if (model == "custom")
    fit_custom(...)
  else if (model == "gaussian")
    fit_gaussian(...)
  else if (model == "quadratic")
    fit_quadratic(...)
  else if (model == "rezende")
    fit_rezende(...)
  else if (model == "weibull")
    fit_weibull(...)
  else
    stop("Selected model not available")
}
