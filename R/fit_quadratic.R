#' Fit Brière curve
#'
#' Fits a curve using the Brière model for fitting thermal performance curves
#'
#' @param ymax maximal value of y
#' @param xopt value of x at which y = ymax
#' @param xmin minimal threshold of x (y = 0)
#' @param xmax maximal threshold of x (y = 0)
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a curve using the quadratic model:
#' \deqn{
#'   y = a + b \cdot x + c \cdot x^2
#' }
#' Optimal values for \code{a} (parameter defining y when x = 0), \code{b}, and \code{c} are estimated based on provided arguments.
#' @keywords internal

fit_quadratic <- function(ymax, xopt, xmin, xmax){
  # Define the equation
  equation <- function(x, params) {
    a <- params[1]
    b <- params[2]
    c <- params[3]
    
    y <- a + b * x + c * x^2
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
  
  # Set the initial values for a, b, and c
  initial_params <- c(0.23, 0.01, -2)  # Initial guess for a, b, and c
  
  # Use optim to find the optimal values of a, b, and c
  result <- optim(par = initial_params, fn = objective, x = x, y = y)
  
  # Extract the optimized values of a, b, and c
  a <- result$par[1]
  b <- result$par[2]
  c <- result$par[3]
  
  # Define the model equation
  model <- function(x) {
    y <- a + b * x + c * x^2
    y <- ifelse(x < xmin | x > xmax, 0,
                ifelse(y < 0, 0, y))
    return(y)
  }
}
