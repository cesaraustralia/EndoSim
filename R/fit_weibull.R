#' Fit Weibull curve
#'
#' Fits a curve using the Weibull model for fitting thermal performance curves
#'
#' @param ymax maximal value of y
#' @param xopt value of x at which y = ymax
#' @param xmin minimal threshold of x (y = 0)
#' @param xmax maximal threshold of x (y = 0)
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a curve using the Weibull model:
#' \deqn{
#'   y = a \cdot \left(\frac{c - 1}{c}\right)^{\frac{1 - c}{c}} \cdot \left(\frac{x - x_{\text{opt}}}{b} + \left(\frac{c - 1}{c}\right)^{\frac{1}{c}}\right)^{c - 1}  \cdot e^{-\left(\left(\frac{x - x_{\text{opt}}}{b} + \frac{c - 1}{c}\right)^{\frac{1}{c}}\right)^c + \frac{c - 1}{c}\right)} 
#' }
#' Optimal values for \code{a} (curve height parameter), \code{b} (curve breadth parameter), and \code{c} (curve shape parameter) are estimated based on provided arguments.
#' @keywords internal

fit_weibull <- function(ymax, xopt, xmin, xmax){
  # Define the equation
  equation <- function(x, params) {
    a <- params[1]
    b <- params[2]
    c <- params[3]
    xopt <- params[4]
    
    y <- (a *
            (((c - 1)/c)^((1 - c)/c)) *
            ((((x - xopt)/b) + (((c - 1)/c)^(1/c)))^(c - 1)) *
            (exp(-((((x - xopt)/b) + (((c - 1)/c)^(1/c)))^c) + ((c - 1)/c))))
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
  initial_params <- c(0.09, 26, 4, xopt)  # Initial guess for a, b, and c
  
  # Use optim to find the optimal values of a, b, and c
  result <- optim(par = initial_params, fn = objective, x = x, y = y)
  
  # Extract the optimized values of a, b, and c
  a <- result$par[1]
  b <- result$par[2]
  c <- result$par[3]
  
  # Define the model equation
  model <- function(x) {
    y <- (a *
            (((c - 1)/c)^((1 - c)/c)) *
            ((((x - xopt)/b) + (((c - 1)/c)^(1/c)))^(c - 1)) *
            (exp(-((((x - xopt)/b) + (((c - 1)/c)^(1/c)))^c) + ((c - 1)/c))))
    y <- ifelse(x < xmin | x > xmax, 0,
                ifelse(y < 0, 0, y))
    return(y)
  }
}