#' Fit Gaussian curve
#'
#' Fits a curve using the Gaussian model for fitting thermal performance curves
#'
#' @param ymax maximal value of y
#' @param xopt value of x at which y = ymax
#' @param xmin minimal threshold of x (y = 0)
#' @param xmax maximal threshold of x (y = 0)
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a curve using the Gaussian model:
#' \deqn{
#'   y = y_{\text{max}} \cdot e^{-0.5 \cdot \left(\frac{{|x - x_{\text{opt}}|}}{{a}}\right)^2}
#' }
#' Optimal value for \code{a} is estimated based on provided arguments.
#' @keywords internal

fit_gaussian <- function(ymax, xopt, xmin, xmax){
  # Define the equation
  equation <- function(x, params) {
    a <- params[1]
    xopt <- params[2]
    ymax <- params[3]
    
    y <- ymax * exp(-0.5 * (abs(x - xopt)/a)^2)
    return(y)
  }
  
  # Define the objective function
  objective <- function(params, x, y) {
    y_fit <- equation(x, params)
    diff <- y_fit - y
    return(sum(diff^2))  # Sum of squared differences for optimization
  }
  
  # Create the x vector for optimization
  x <- c(xmin, xmax, xopt)
  
  # Create the y vector for optimization
  y <- c(0, 0, ymax)
  
  # Set the initial values for a and b
  initial_params <- c(10, xopt, ymax)  # Initial guess for a
  
  # Use optim to find the optimal value of a
  result <- optim(par = initial_params, fn = objective, x = x, y = y)
  
  # Extract the optimized value of a
  a <- result$par[1]
  
  # Define the model equation
  model <- function(x) {
    y <- ymax * exp(-0.5 * (abs(x - xopt)/a)^2)
    y <- ifelse(x < xmin | x > xmax, 0,
                ifelse(y < 0, 0, y))
    return(y)
  }
}
