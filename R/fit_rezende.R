#' Fit Rezende curve
#'
#' Fits a curve using the Rezende model for fitting thermal performance curves
#'
#' @param ymax maximal value of y
#' @param xopt value of x at which y = ymax
#' @param xmin minimal threshold of x (y = 0)
#' @param xmax maximal threshold of x (y = 0)
#' @return function that accepts x and returns y based on fitted model
#' @details
#' Fits a curve using the Rezende model (Rezende et al. 2019):
#' \deqn{
#'   y = \begin{cases}
#'   a \cdot 10^{\frac{\log_{10}(q_{10})}{\frac{10}{x}}}, & \text{if } x < x_{\text{opt}} \\
#'   a \cdot 10^{\frac{\log_{10}(q_{10})}{\frac{10}{x}}} \cdot (1 - b \cdot (x_{\text{opt}} - x)^2), & \text{otherwise}
#'   \end{cases}
#' }
#' Optimal values for \code{a} (rate shift parameter), \code{b} (decline rate parameter), and q_{10} are estimated based on provided arguments.
#' @keywords internal
#' @examples 
#' my_func <- EndoSim:::fit_rezende(1, 25, 2.84, 32.79)
#' plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
#' @references
#' Rezende, Enrico L., and Francisco Bozinovic. Thermal performance across levels of biological organization. Philosophical Transactions of the Royal Society B 374.1778 (2019): 20180549.
#' @seealso [fit_custom()], [fit_gaussian()], [fit_null()], [fit_quadratic()]

fit_rezende <- function(ymax, xopt, xmin, xmax){
  if(xopt < xmin | xopt > xmax)
    warning("xopt is outside threshold values; results may be nonsensical")
  
  # Define the equation
  equation <- function(x, params) {
    a <- params[1]
    b <- params[2]
    q10 <- params[3]
    
    y <- {
      ifelse(x < xopt, (a * 10^(log10(q10)/(10/x))), (a * 10^(log10(q10)/(10/x))) * (1 - b * (xopt - x)^2))
    }
    return(y)
  }
  
  # Define the objective function
  objective <- function(params, x, y) {
    y_fit <- equation(x, params)
    diff <- y_fit - y
    return(sum(diff^2))  # Sum of squared differences for optimization
  }
  
  # Create the x vector for optimization
  x <- c(xmin, xopt, xmax)
  
  # Create the y vector for optimization
  y <- c(0, ymax, 0)
  
  # Set the initial values for a, b, and q10
  initial_params <- c(0.1, 0.1, 2.77)  # Initial guess for a, b, and q10
  
  # Use optim to find the optimal values of a, b, and q10
  result <- stats::optim(par = initial_params, fn = objective, x = x, y = y)
  
  # Extract the optimized values of a, b, and q10
  a <- result$par[1]
  b <- result$par[2]
  q10 <- result$par[3]
  
  # Define the model equation
  func <- function(x) {
    output <- {
      ifelse(x < xopt, (a * 10^(log10(q10)/(10/x))), (a * 10^(log10(q10)/(10/x))) * (1 - b * (xopt - x)^2))
    }
    output <- ifelse(output < 0, 0, output)
    return(output)
  }
  
  # Scale to range 0-1
  max.y <- max(func(seq(-50, 50, by = 0.1)))
  min.y <- min(func(seq(-50, 50, by = 0.1)))
  
  scale_values <- function(x) {
    (x-min.y) / (max.y-min.y)
  }
  
  func <- function(x) {
    output <- {
      ifelse(x < xopt, (a * 10^(log10(q10)/(10/x))), (a * 10^(log10(q10)/(10/x))) * (1 - b * (xopt - x)^2))
    }
    output <- ifelse(output < 0, 0, output)
    return(scale_values(output))
  }
  
  return(func)
}
