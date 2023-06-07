fit_briere <- function(ymax, xopt, xmin, xmax){
  # Define the equation
  equation <- function(x, params) {
    a <- params[1]
    b <- params[2]
    xmin <- params[3]
    xmax <- params[4]
    
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
  initial_params <- c(0.001, 1, xmin, xmax)  # Initial guess for a and b
  
  # Use optim to find the optimal values of a and b
  result <- optim(par = initial_params, fn = objective, x = x, y = y)
  
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
  
  # Set the initial values for a and b
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

fit_rezende <- function(ymax, xopt, xmin, xmax){
  # Define the equation
  equation <- function(x, params) {
    a <- params[1]
    b <- params[2]
    q10 <- params[3]
    xopt <- params[4]
    
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
  x <- c(xmin, xmax, xopt)
  
  # Create the y vector for optimization
  y <- c(0, 0, ymax)
  
  # Set the initial values for a, b, and q10
  initial_params <- c(0.06, 0.003, 2.77, xopt)  # Initial guess for a, b, and q10
  
  # Use optim to find the optimal values of a, b, and q10
  result <- optim(par = initial_params, fn = objective, x = x, y = y)
  
  # Extract the optimized values of a, b, and q10
  a <- result$par[1]
  b <- result$par[2]
  q10 <- result$par[3]
  
  # Define the model equation
  model <- function(x) {
    y <- {
      ifelse(x < xopt, (a * 10^(log10(q10)/(10/x))), (a * 10^(log10(q10)/(10/x))) * (1 - b * (xopt - x)^2))
    }
    y <- ifelse(x < xmin | x > xmax, 0,
                ifelse(y < 0, 0, y))
    return(y)
  }
}

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

