#' Fit sigmoid curve
#'
#' Fits a sigmoid curve
#'
#' @param ymax maximal value of y
#' @param xmin minimal threshold of x (y = 0)
#' @param xmax maximal threshold of x (y = ymax)
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a sigmoid curve:
#' \deqn{
#'   y = \frac{a}{1 + e^{-b \cdot (x - x_{\text{max}})}}
#' }
#' Optimal values for \code{a} and \code{b} (curve breadth parameter) are estimated based on provided arguments.
#' @keywords internal

fit_sigmoid <- function(ymax, xmin, xmax){
  # Define the equation
  equation <- function(x, params) {
    a <- params[1]
    b <- params[2]
    xmax <- params[3]
    
    y <- a / (1 + exp(-b * (x - xmax)))
    return(y)
  }
  
  # Define the objective function
  objective <- function(params, x, y) {
    y_fit <- equation(x, params)
    diff <- y_fit - y
    return(sum(diff^2))  # Sum of squared differences for optimization
  }
  
  # Create the x vector for optimization
  x <- c(xmin, (xmin + xmax)/2, xmax)
  
  # Create the y vector for optimization
  y <- c(0, ymax/2, ymax)
  
  # Set the initial values for a and b
  initial_params <- c(0.54, 0.37, xmax)  # Initial guess for a and b
  
  # Use optim to find the optimal values of a and b
  result <- optim(par = initial_params, fn = objective, x = x, y = y)
  
  # Extract the optimized values of a and b
  a <- result$par[1]
  b <- result$par[2]
  
  # Define the model equation
  model <- function(x) {
    y <- a / (1 + exp(-b * (x - xmax)))
    y <- ifelse(x < xmin, 0,
                ifelse(y < 0, 0, y))
    return(y)
  }
}