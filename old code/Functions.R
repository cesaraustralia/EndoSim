# ###########################################################
#               Endosymbiont functions
#                   Alex Slavenko
#                    24/10/2022
# ###########################################################

## DEPENDENT FUNCTIONS
# 1. Function to calculate aphid increase rate
aphid_growth <- function(temperature) {
  # Ta1 <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)
  # At <- c(0, 0.18, 0.5, 0.77, 0.99, 1, 0.9, 0.8, 0)
  # mod <- lm(At ~ Ta1 + I(Ta1 ^ 2))
  
  # Data from Whalon & Smilowitz 1979 
  
  Ta1 <- c(4, 8.87, 11.82, 14.22, 17.13, 19.92, 22.61, 25.77, 27.80, 30)
  At <- c(0, 0.06, 0.09, 0.11, 0.13, 0.15, 0.16, 0.14, 0.04, 0)
  mod <- mgcv::gam(At ~ s(Ta1, k = 7))
  new <- data.frame(Ta1 = temperature)
  current_growth <- (unlist(predict(mod, newdata = new)))
  return(max(0, current_growth))
}

# 2. Function to calculate aphid loss due to rainfall
aphid_loss <- function(rainfall) {
  Wr <- c(1.5, 2, 2.5, 3, 3.5)
  Wl <- c(0, 0.25, 0.35, 0.45, 0.55)
  mod <-
    nls(Wl ~ SSlogis(Wr, Asym, xmid, scal), data = data.frame(Wr, Wl))
  new <- data.frame(Wr = rainfall)
  loss <- (unlist(predict(mod, newdata = new)))
  return(max(loss, 0))
}

# 3. Function to calculate aphid tendency to become infective
aphid_suscept <- function(temperature) {
  Ta1 <- seq(0, 40, 4)
  As <- c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.75, 0.6, 0.3, 0, 0)
  mod <- mgcv::gam(As ~ s(Ta1, k = 7))
  new <- data.frame(Ta1 = temperature)
  aphid_sus <- max(0, (unlist(predict(mod, newdata = new))))
  aphid_sus <- max(aphid_sus, 0)
  return(aphid_sus)
}

# 4. Function to calculate transmission efficiency
transmission_efficiency <- function(temperature) {
  Ta1 <- seq(0, 40, 4)[1:10]
  An <- c(0, 0.6, 0.7, 0.75, 0.8, 0.8, 0.75, 0.6, 0.3, 0, 0)[1:10]
  mod <- lm(An ~ Ta1 + I(Ta1 ^ 2))
  new <- data.frame(Ta1 = temperature)
  virus_transmit <- max(0, (unlist(predict(mod, newdata = new))))
  return(virus_transmit)
}

# 5. Function to calculate production of alate aphids
alate_production <- function(CP_fraction_aphid_capacity) {
  if (CP_fraction_aphid_capacity <= 0) {
    prop_winged <- 0
  } else{
    Af <- seq(0, 0.9, 0.1)
    App <-
      c(0.01, 0.22, 0.35, 0.45, 0.53, 0.6, 0.66, 0.72, 0.76, 0.78)
    mod <- lm(App ~ Af + I(Af ^ 2))
    new <- data.frame(Af = CP_fraction_aphid_capacity)
    prop_winged <- max(0, (unlist(predict(mod, newdata = new))))
  }
  return(prop_winged)
}


##INTERMEDIATE CALCULATIONS
# 1. Function to calculate infection of plants per time step
plant_infect <-
  function(Ph,
           Pi,
           Awt,
           Ai,
           Awtp,
           Aip,
           Ac,
           temperature,
           Aa,
           Aw) {
    # a. calculate current fraction of carrying capacity
    Af = (Awt + Ai) / Ac
    # b. calculate production of aphids
    Awp = (Awtp + Aip) * alate_production(Af)
    # c. calculate total aphid moves
    Amt = ((Awt + Ai) * Aa) + (Awp * Aw)
    # d. calculate transmission efficiency
    An = transmission_efficiency(temperature)
    # e. calculate successful plant innoculations
    Pn = (Amt * Ai) / ((Awt + Ai) * An)
    # f. calculate new number of infected plants
    Pi = Ph * (1 - exp(-Pn / (Ph + Pi)))
    
    return(max(0, Pi))
  }

# 2. Function to calculate infection of WT aphids per time step
WT_infect <- function(temperature, Awt, Ph, Pi) {
  # a. calculate aphid susceptibility
  As = aphid_suscept(temperature)
  # b. calculate proportion of infected plants
  Pf = Pi / (Ph + Pi)
  # c. calculate infection of WT aphids
  Awti = As * Pf * Awt
  
  return(max(0, Awti))
}

# 3. Function to calculate production of new WT aphids per time step
pop_produce <- function(temperature, Ax, Ac, A, Apop, prop_day) {
  # a. calculate aphid increase rate
  At = aphid_growth(temperature)
  # b. calculate current fraction of carrying capacity
  Af = A / Ac
  # c.calculate aphid reproductive rate
  Ar = At * Ax * (1 - Af)
  # d. calculate production of new WT aphids
  A_produce = Ar * Apop * prop_day
  
  return(max(0, A_produce))
}

# 4. Function to calculate loss of aphids per time step
pop_loss <- function(rainfall, Al, Apop) {
  # a. calculate loss of aphids due to rainfall
  Wl = aphid_loss(rainfall)
  # b. calculate loss of WT aphids
  A_loss = Apop * (1 - (1 - Al) * (1 - Wl))
  
  return(max(0, A_loss))
}

##HELPER FUNCTIONS
auc_thresh <- function(x, y, min_val, max_val) {
  library(splines)
  
  min_y <- min(c(y, min_val))
  
  # normalise y and min_val, max_val
  y_norm <- y - min_y
  min_norm <- min_val - min_y
  max_norm <- max_val - min_y
  
  # calculate area under curve for full range
  spline_fit <- smooth.spline(x, y_norm)
  auc_full <- integrate(function(z) predict(spline_fit, z)$y, min(x), max(x))$value
  
  # define function to integrate above min threshold
  f_above_min_thresh <- function(z, spline_fit, thresh) {
    y_pred <- predict(spline_fit, z)$y - thresh
    ifelse(y_pred > 0, y_pred, 0)
  }
  
  # calculate area above min threshold
  auc_min <- integrate(f_above_min_thresh, min(x), max(x), spline_fit = spline_fit, thresh = min_norm)$value
  
  # define function to integrate above max threshold
  f_above_max_thresh <- function(z, spline_fit, thresh) {
    y_pred <- predict(spline_fit, z)$y - thresh
    ifelse(y_pred > 0, y_pred, 0)
  }
  
  # calculate area above max threshold
  auc_max <- integrate(f_above_max_thresh, min(x), max(x), spline_fit = spline_fit, thresh = max_norm)$value
  
  # return auc_full and difference between auc_min and auc_max
  return(c(auc_full, auc_min - auc_max))
}