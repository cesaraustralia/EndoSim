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
#' Fits a curve using the Brière model (Brière et al. 1999):
#' \deqn{
#'   y = a \cdot x \cdot (x - x_{\text{min}}) \cdot ((x_{\text{max}} - x)^{\frac{1}{b}})
#' }
#' Optimal values for \code{a} (scale parameter) and \code{b} (shape parameter) are estimated based on provided arguments.
#' @keywords internal
#' @examples 
#' my_func <- EndoSim:::fit_briere(0.16, 22.61, 4, 30)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @references
#' Brière, J.F., Pracros, P., Le Roux, A.Y., Pierre, J.S., A novel rate model of temperature-dependent development for arthropods. Environmental Entomololgy, 28, 22–29 (1999)
#' @seealso [fit_sigmoid()], [fit_custom()], [fit_gaussian()], [fit_quadratic()], [fit_rezende()], [fit_weibull()]

fit_briere <- function(ymax, xopt, xmin, xmax){
  if(xopt < xmin | xopt > xmax)
    warning("xopt is outside threshold values; results may be nonsensical")
  
  # Define the equation
  equation <- function(x, params) {
    a <- params[1]
    b <- params[2]
    
    y <- a * x * (x - xmin) * ((xmax - x)^(1/b))
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
  initial_params <- c(0.001, 1)  # Initial guess for a and b
  
  # Use optim to find the optimal values of a and b
  result <- stats::optim(par = initial_params, fn = objective, x = x, y = y)
  
  # Extract the optimized values of a and b
  a <- result$par[1]
  b <- result$par[2]
  
  # Define the model equation
  model <- function(x) {
    y <- a * x * (x - xmin) * (xmax - x)^(1/b)
    y <- ifelse(x < xmin | x > xmax, 0,
                ifelse(y < 0, 0, y))
    return(y)
  }
}
