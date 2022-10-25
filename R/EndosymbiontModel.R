# ###########################################################
#                 Endosymbiont model
#                   Alex Slavenko
#                    24/10/2022
# ###########################################################

source("R/Functions.R")

endosym_model <- function(t, Y, params, temperature, precipitation){
  ## INITIAL CONDITIONS
  Time <<- t
  Tot_aphids <- Y['Tot_aphids']
  I_aphids <- Y['I_aphids']
  WT_aphids <- Y['WT_aphids']
  HealthyCP <- Y['HealthyCP']
  InfectedCP <- Y['InfectedCP']
  WT_production <- Y['WT_production']
  I_production <- Y['I_production']
  
  if(!(round(I_aphids + WT_aphids) == round(Tot_aphids)))
    stop("Number of infected and WT aphids do not sum to total")
  
  if(Tot_aphids == 0)
    return(c(Tot_aphids,
             I_aphids,
             WT_aphids,
             HealthyCP,
             InfectedCP,
             WT_production,
             I_production))
  
  ## APHID MODEL PARAMETERS
  params <- unlist(params)
  max_net_reproductive_rate <- params["max_net_reproductive_rate"]
  CP_aphid_capacity <- params["CP_aphid_capacity"]
  aphid_natural_losses <- params["aphid_natural_losses"]
  fitness_cost <- params["fitness_cost"]
  aphid_walks <- params["aphid_walks"]
  aphid_flights <- params["aphid_flights"]
  decay_constant <- params["decay_constant"]
  
  ## Calculate plant infection parameters
  # Calculate number of newly infected plants
  new_inf = plant_infect(Ph = HealthyCP,
                         Pi = InfectedCP,
                         Awt = WT_aphids,
                         Ai = I_aphids,
                         Awtp = WT_production,
                         Aip = I_production,
                         Ac = CP_aphid_capacity,
                         temperature = temperature,
                         Aa = aphid_walks,
                         Aw = aphid_flights)
  
  # Calculate total number of plants
  CP_total = InfectedCP + HealthyCP
  
  # Calculate decay constant
  decay_constant = CP_total / I_aphids
  
  # Calculate plants "recovered" (new proportion of infected plants)
  Plant_heal = (InfectedCP / CP_total) * exp(-decay_constant)
  
  # Calculate change in density of infected plants
  InfectedCP = max(0, CP_total*Plant_heal + new_inf)
  
  # Calculate change in density of healthy plants
  HealthyCP = CP_total - InfectedCP
  
  ## Calculate population change parameters
  # calculate production of newly infected aphids
  WT_innoculated = WT_infect(temperature = temperature,
                             Awt = WT_aphids,
                             Ph = HealthyCP,
                             Pi = InfectedCP)
  
  # calculate production of new WT aphids
  WT_production = pop_produce(temperature = temperature,
                              Ax = max_net_reproductive_rate,
                              Ac = CP_aphid_capacity,
                              A = Tot_aphids,
                              Apop = WT_aphids - WT_innoculated)
  
  # calculate loss of WT aphids
  WT_decrease = pop_loss(rainfall = precipitation,
                         Al = aphid_natural_losses,
                         Apop = WT_aphids - WT_innoculated)
  
  # calculate production of new infected aphids
  I_production = pop_produce(temperature = temperature,
                             Ax = max_net_reproductive_rate,
                             Ac = CP_aphid_capacity,
                             A = Tot_aphids,
                             Apop = I_aphids + WT_innoculated) * (1 - fitness_cost)
  
  # calculate loss of infected aphids
  I_decrease = pop_loss(rainfall = precipitation,
                        Al = aphid_natural_losses,
                        Apop = I_aphids + WT_innoculated)
  
  
  ## Calculate aphid population changes
  # Calculate change in density of WT aphids
  dWT_aphids = WT_production - WT_innoculated - WT_decrease
  WT_aphids = WT_aphids + dWT_aphids
  
  # Calculate change in density of infective aphids
  dI_aphids = I_production + WT_innoculated - I_decrease
  I_aphids = I_aphids + dI_aphids
  
  # Calculate total density of aphids
  Tot_aphids = Tot_aphids + dWT_aphids + dI_aphids
  
  # prevent aphid/crop population densities dropping below 0
  Tot_aphids <- max(Tot_aphids, 0)
  I_aphids <- max(I_aphids, 0)
  WT_aphids <- max(WT_aphids, 0)
  HealthyCP <- max(HealthyCP, 0)
  InfectedCP <- max(InfectedCP, 0)
  WT_production <- max(WT_production, 0)
  I_production <- max(I_production, 0)
  
  return(c(Tot_aphids,
           I_aphids,
           WT_aphids,
           HealthyCP,
           InfectedCP,
           WT_production,
           I_production))
}

model_run <- function(time, Y_init, params, plot = F, constant = F,...){
  if(constant)
    print("Modelling with constant temperature and rainfall through time")
  
  require(tidyverse)
  
  setwd("Data/")
  source("st_r_functions.R")
  setwd("..")
  
  ## INITIAL CONDITIONS
  Y_init <- unlist(Y_init)
  
  Y_time <- tibble(time = 0,
                   Tot_aphids = Y_init["Tot_aphids"],
                   I_aphids = Y_init["I_aphids"],
                   WT_aphids = Y_init["WT_aphids"],
                   HealthyCP = Y_init["HealthyCP"],
                   InfectedCP = Y_init["InfectedCP"],
                   WT_production = 0,
                   I_production = 0)
  
  Y <- c(Y_init, 0, 0)
  names(Y) <- c(names(Y_init), "WT_production", "I_production")
  
  print("Running model:")
  
  progress_bar = txtProgressBar(min=0, max=length(time), style = 1, char="=")
  
  for(t in time){
    if(!constant){
      precipitation <- inputData(t, 'Precipitation')
      temperature <- inputData(t, 'Temperature') 
    }

    Y <- set_names(endosym_model(t, Y, params, temperature, precipitation),
                   c("Tot_aphids",
                     "I_aphids",
                     "WT_aphids",
                     "HealthyCP",
                     "InfectedCP",
                     "WT_production",
                     "I_production"))
    
    Y_time <- bind_rows(Y_time,
                        set_names(c(t, Y),
                                  c("time", names(Y))))
    
    setTxtProgressBar(progress_bar, value = t)
  }
  
  close(progress_bar)
  
  if(plot) {
    print(Y_time %>%
            gather(SV, value, -c(time, WT_production, I_production)) %>%
            mutate(SV = factor(SV, levels = c("Tot_aphids", "I_aphids", "WT_aphids", "HealthyCP", "InfectedCP"))) %>%
            ggplot(aes(x = time, y = value)) +
            geom_line() +
            theme_bw() +
            facet_wrap(vars(SV), nrow = 2, scales = "free"))
  }
  
  return(Y_time)
}