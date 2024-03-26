#' EndoSim
#'
#' Run simulation using the endosymbiont model
#'
#' @param Pest object of class \code{pest} defining the pest
#' @param Endosymbiont object of class \code{endosym} defining the endosymbiont
#' @param Crop object of class \code{crop} defining the crop
#' @param Parasitoid object of class \code{parasitoid} defining the parasitoid
#' @param init object of class \code{initial} defining the starting conditions for the simulation
#' @param conds object of class \code{sim_conds} defining the simulation conditions
#' @param plot if \code{TRUE} (default) generate plots
#' @param progress if \code{TRUE} (default) print progress bar
#' @param vert_trans set to \code{TRUE} to turn on vertical transmission
#' @param hori_trans set to \code{TRUE} to turn on horizontal transmission
#' @param imi set to \code{TRUE} to turn on immigration into paddock
#' @param emi set to \code{TRUE} to turn on emigration from paddock
#' @param para set to \code{TRUE} to turn on parasitoids
#' @return object of class \code{endosym_mod}; see [endosym_mod-class].
#' @details
#' Constructs the endosymbiont model using the provided [pest-class], [endosym-class], [crop-class],[parasitoid-class], [initial-class], and [sim_conds-class] objects.
#' 
#' The \code{Pest} species feeds on the provided \code{Crop}.
#' 
#' Vertical and horizontal transmission of endosymbiont are defined using functions in the \code{Endosymbiont}.
#' 
#' Parasitism rates are defined usined functions in the \code{Parasitoid}.
#' 
#' @export endosim


endosim <- function(Pest,
                    Endosymbiont,
                    Crop,
                    Parasitoid = NULL,
                    init,
                    conds,
                    plot = TRUE,
                    progress = TRUE,
                    vert_trans = FALSE,
                    hori_trans = FALSE,
                    imi = FALSE,
                    emi = FALSE,
                    para = FALSE
) {
  if (!inherits(Pest, "pest"))
    stop("No Pest of class pest provided!")
  
  if (!inherits(Crop, "crop"))
    stop("No Crop of class crop provided!")
  
  if (!inherits(init, "initial"))
    stop("No init of class initial provided!")
  
  if (!inherits(conds, "sim_conds"))
    stop("No conds of class sim_conds provided!")
  
  if (!inherits(Parasitoid, "parasitoid"))
    stop("No Parasitoid of class parasitoid provided!")
  
  if(!vert_trans){
    Endosymbiont@'fitness_cost' <- 0
    warning("Vertical transmission cancelled!")
  }
  
  if(!hori_trans){
    Endosymbiont@'fun_trans_eff' <- fit_null(0)
    Endosymbiont@'fun_susc' <- fit_null(0)
    warning("Horizontal transmission cancelled!")
  }
  
  if(!imi){
    warning("Immigration cancelled!")
  }
  
  if(!emi){
    Pest@'fun_emi' <- fit_null(0)
    warning("Emigration cancelled!")
  }
  
  if(!para){
    Parasitoid@'fun_para_scal' <- fit_null(0)
    Parasitoid@'introduction_n' <- 0
    warning("Parasitoids cancelled!")
  }
  
  # define simulation parameters
  sim_length = conds@'sim_length'
  env = conds@'env'
  start_date = conds@'start_date'
  
  bg_loss <- Pest@'bg_loss'
  alate_penalty <- Pest@'alate_penalty'
  apterae_walk <- Pest@'apterae_walk'
  alate_flight <- Pest@'alate_flight'
  
  fitness_cost <- Endosymbiont@'fitness_cost'
  
  heal_time <- Crop@'heal_time'
  sowing_date <- Crop@'sowing_date'
  emergence_date <- Crop@'emergence_date'
  harvest_date <- Crop@'harvest_date'
  pest_intro_date <- Endosymbiont@'introduction_date'
  para_intro_date <- Parasitoid@'introduction_date'
  
  area <- init@'Crop'[[3]]/Crop@'density'
  
  introduction_n <- Endosymbiont@'introduction_n'
  
  # define functions
  fun_dev_apt <- Pest@'fun_dev_apt'
  fun_dev_ala <- Pest@'fun_dev_ala'
  fun_imi_neg <- Pest@'fun_imi_neg'
  fun_imi_pos <- Pest@'fun_imi_pos'
  fun_emi <- Pest@'fun_emi'
  fun_temp_loss <- Pest@'fun_temp_loss'
  fun_rainfall_loss <- Pest@'fun_rainfall_loss'
  fun_sen_loss <- Pest@'fun_sen_loss'
  fun_dens_fecund <- Pest@'fun_dens_fecund'
  fun_temp_fecund <- Pest@'fun_temp_fecund'
  fun_age_fecund <- Pest@'fun_age_fecund'
  fun_alate_prod <- Pest@'fun_alate_prod'
  
  fun_trans_eff <- Endosymbiont@'fun_trans_eff'
  fun_susc <- Endosymbiont@'fun_susc'
  
  fun_dev_para <- Parasitoid@'fun_dev_para'
  fun_para_scal <- Parasitoid@'fun_para_scal'
  fun_attack <- Parasitoid@'fun_attack'
  fun_handling <- Parasitoid@'fun_handling'
  
  fun_cc <- Crop@'carrying_capacity'
  
  # define tracked populations
  cohorts <- init@'Pest'
  
  # create initial vector of active cohorts
  active_cohorts <- which(cohorts[, 1, ] > 0)
  
  # create initial dataframe of pest population
  pos_pops <- rbind(t(cohorts[1, , ]), t(cohorts[3, , ]))
  neg_pops <- rbind(t(cohorts[2, , ]), t(cohorts[4, , ]))
  
  pest_pop <- data.frame(t = 0,
                         pos_n1 = sum(pos_pops[which(pos_pops[, 2] == 1), 1], na.rm = T),
                         neg_n1 = sum(neg_pops[which(neg_pops[, 2] == 1), 1], na.rm = T),
                         pos_n2 = sum(pos_pops[which(pos_pops[, 2] == 2), 1], na.rm = T),
                         neg_n2 = sum(neg_pops[which(neg_pops[, 2] == 2), 1], na.rm = T),
                         pos_n3 = sum(pos_pops[which(pos_pops[, 2] == 3), 1], na.rm = T),
                         neg_n3 = sum(neg_pops[which(neg_pops[, 2] == 3), 1], na.rm = T),
                         pos_n4 = sum(pos_pops[which(pos_pops[, 2] == 4), 1], na.rm = T),
                         neg_n4 = sum(neg_pops[which(neg_pops[, 2] == 4), 1], na.rm = T),
                         pos_adult = sum(pos_pops[which(pos_pops[, 2] == 5), 1], na.rm = T),
                         neg_adult = sum(neg_pops[which(neg_pops[, 2] == 5), 1], na.rm = T)
  )
  
  # create initial matrix of cohort development
  dev_cohorts <- matrix(rep(0, 20), ncol = 4)
  
  # create initial matrix of adult ages
  adult_ages <- matrix(c(rep(0, 20)), nrow = 4)
  
  # create initial matrix of cohort ages
  cohort_ages <- matrix(c(rep(1, 20)), nrow = 4)
  
  # create initial vector of crop population
  crop_pop <- init@'Crop'
  
  # create initial vector of parasitoid population and array of parasitised cohorts
  para_pop <- c(0, 0)
  names(para_pop) <- c("mummies", "females")
  
  para_cohorts <- array(0,
                        dim = c(4, 2, 2),
                        dimnames = list("pest_type" = c("pos_apt", "neg_apt", "pos_ala", "neg_ala"),
                                        "variable" = c("N", "lifestage"),
                                        "cohort_num" = c()
                        )
  )
  
  active_para <- which(para_cohorts[, 1, ] > 0)
  
  dev_para_cohorts <- matrix(rep(0, 8), ncol = 4)
  
  para_ages <- matrix(rep(0, 8), nrow = 4)
  
  para_df <- data.frame(t = 0,
                        mummies = 0,
                        females = para_pop[['females']])
  
  if(progress){
    print("Running simulation:")
    
    progress_bar = utils::txtProgressBar(
      min = 0,
      max = sim_length,
      style = 1,
      char = "="
    )
  }
  
  for (t in 1:sim_length){
    current_date <- lubridate::ymd(start_date) + lubridate::days(t)
    growth_season <- current_date %in% seq(lubridate::ymd(emergence_date), lubridate::ymd(harvest_date), "day")
    
    # adjust carrying capacity if outside growth season
    dae <- as.numeric(current_date - lubridate::ymd(emergence_date))
    cc_p <- fun_cc(dae)
    
    if(growth_season){
      fun_dens_fecund <- fit_bannerman(10000 * area * cc_p, 0.0008)
    }
    else
      fun_dens_fecund <- fit_null(0)
    
    min_temp = env[t, 2]
    max_temp = env[t, 3]
    temperature = env[t, 4]
    rainfall = env[t, 5]
    
    ## nymph development and adult aging
    # calculate development units gained in timestep
    dev_apt <- fun_dev_apt(temperature)
    dev_ala <- fun_dev_ala(temperature)
    dev_para <- fun_dev_para(temperature)
    
    # add development units
    dev_cohorts[, c(1, 2)] <- dev_cohorts[, c(1, 2)] + dev_apt
    dev_cohorts[, c(3, 4)] <- dev_cohorts[, c(3, 4)] + dev_ala
    
    dev_para_cohorts <- dev_para_cohorts + dev_para
    
    ## life-stage transitions
    # identify which pest types in the cohort have reached completion of development
    completed_dev <- which(t(dev_cohorts >= 1))
    completed_dev <- completed_dev[which(completed_dev %in% active_cohorts)]
    
    dev_cohorts[which(dev_cohorts >= 1)] <- dev_cohorts[which(dev_cohorts >= 1)] - 1
    
    # identify which are becoming adults
    new_adult <- which(cohorts[, 2, ] == 4)[which(cohorts[, 2, ] == 4) %in% completed_dev]
    
    # transfer to next life stage
    cohorts[, 2, ][completed_dev] <- cohorts[ , 2, ][completed_dev] + 1
    cohorts[, 2, ] <- sapply(cohorts[, 2, ], min, 5) # make sure lifestage isn't over 5 (adult)
    
    # update adult ages
    adult_ages[new_adult] <- t
    
    # identify which parasitised pests have completed development
    new_para <- which(t(dev_para_cohorts >= 1))
    new_para <- new_para[which(new_para %in% active_para)]
    
    para_pop[['females']] <- para_pop[['females']] + round(sum(para_cohorts[, 1, ][new_para])/4, 0)
    
    para_cohorts[, 1, ][new_para] <- 0
    
    active_para <- which(para_cohorts[, 1, ] > 0)
    
    # identify which parasitised pests have become mummies
    mummies <- which(dev_para_cohorts >= 0.6)
    mummies <- mummies[which(mummies %in% active_para)]
    
    para_pop[['mummies']] <- sum(para_cohorts[, 1, ][mummies])
    
    ## dispersal
    # calculate how many adults emigrate
    emi_adult <- fun_emi(cohorts[, 1, ][new_adult])
    
    cohorts[, 1, ][new_adult] <- cohorts[, 1, ][new_adult] - emi_adult
    cohorts[, 1, ] <- sapply(cohorts[, 1, ], max, 0) # make sure pop size doesn't drop to negative
    
    # calculate how many new adults immigrate into population
    # introduction of R+
    if(current_date == lubridate::ymd(pest_intro_date)){
      # create new cohort
      new_cohort <- matrix(c(0, 0, introduction_n, 0,
                             rep(5, 4)),
                           ncol = 2)
      
      cohorts <- abind::abind(cohorts,
                              new_cohort,
                              along = 3)
      dev_cohorts <- rbind(dev_cohorts,
                           matrix(rep(0, 4), nrow = 1))
      adult_ages <- cbind(adult_ages,
                          matrix(c(0, 0, t - 1, 0), ncol = 1))
      cohort_ages <- cbind(cohort_ages,
                           matrix(c(0, 0, t, 0), ncol = 1))
    }
    
    
    # daily immigration
    imi_adult_neg <- fun_imi_neg(t) * area
    imi_adult_pos <- fun_imi_pos(t) * area
    
    if(!imi){
      imi_adult_neg <- round(imi_adult_neg * (1-cc_p), 0)
      imi_adult_pos <- round(imi_adult_pos * (1-cc_p), 0)
    }
    
    if(sum(imi_adult_neg, imi_adult_pos) > 0){
      # create new cohort
      new_cohort <- matrix(c(0, 0, imi_adult_pos, imi_adult_neg,
                             rep(5, 4)),
                           ncol = 2)
      
      cohorts <- abind::abind(cohorts,
                              new_cohort,
                              along = 3)
      dev_cohorts <- rbind(dev_cohorts,
                           matrix(rep(0, 4), nrow = 1))
      adult_ages <- cbind(adult_ages,
                          matrix(c(0, 0, t - 1, t - 1), ncol = 1))
      cohort_ages <- cbind(cohort_ages,
                           matrix(c(0, 0, t, t), ncol = 1))
    }
    
    # update active cohorts vector
    active_cohorts <- which(cohorts[, 1, ] > 0)
    
    # update life stages
    cohorts[, 2, ][-active_cohorts] <- 0
    
    ## virus horizontal transmission
    # identify adult alates
    adult_ala <- which(cohorts[, 2, ] == 5 & stringr::str_detect(rownames(cohorts[, 2, ]), "ala"))
    
    # calculate total pest moves
    tot_mov = (sum(cohorts[, 1, ][-adult_ala], para_cohorts[, 1, ]) * apterae_walk) +
      (sum(cohorts[, 1, ][adult_ala]) * alate_flight)
    
    # calculate proportion of inoculated plants
    new_inoc =
      min(tot_mov/crop_pop[3], 1) * # proportion of plants encountered by pest
      sum(cohorts[c(1, 3), 1, ], para_cohorts[c(1, 3), 1, ]) / sum(cohorts[, 1, ], para_cohorts[, 1, ]) * # probability pest is R+
      crop_pop[1] * # probability encountered plant is R-
      fun_trans_eff(temperature) # probability plant is infected
    
    if(is.na(new_inoc))
      new_inoc <- 0 # capture case when population dies out
    
    # proportion of plants recovering
    heal_ind <- crop_pop[2] * exp(-1/heal_time)
    
    # update crop dataframe
    crop_pop[1] <- max(min(crop_pop[1] - new_inoc + (crop_pop[2] - heal_ind), 1), 0)
    crop_pop[2] <- max(min(heal_ind + new_inoc, 1), 0)
    
    # identify susceptible pests
    sus_ala <- which(cohorts[, 2, ] == 5 & stringr::str_detect(rownames(cohorts[, 2, ]), "neg_ala"))
    sus_apt <- which(cohorts[, 2, ] %in% c(1, 2, 3, 4) & stringr::str_detect(rownames(cohorts[, 2, ]), "neg") |
                       cohorts[, 2, ] == 5 & stringr::str_detect(rownames(cohorts[, 2, ]), "neg_apt"))
    
    sus_para <- sum(para_cohorts[c(2, 4), 1, ])
    
    # calculate infection rate
    inf_rate =
      sum(sum(cohorts[, 1, ][c(sus_apt, sus_ala)]), sus_para) * # Number of susceptible R- pests
      crop_pop[2] * # proportion of R+ plants
      fun_susc(temperature) # probability pest is infected
    
    inf_para <- inf_rate * sus_para / sum(sum(cohorts[, 1, ][c(sus_apt, sus_ala)]), sus_para)
    inf_rate <- inf_rate - inf_para
    
    inf_apt <- inf_rate * sum(cohorts[, 1, ][c(sus_apt)]) / sum(cohorts[, 1, ][c(sus_apt, sus_ala)])
    inf_ala <- inf_rate * sum(cohorts[, 1, ][c(sus_ala)]) / sum(cohorts[, 1, ][c(sus_apt, sus_ala)])
    
    new_inf_apt <- round((cohorts[, 1, ][sus_apt] / sum(cohorts[, 1, ][sus_apt])) * inf_apt, 0)
    new_inf_ala <- round((cohorts[, 1, ][sus_ala] / sum(cohorts[, 1, ][sus_ala])) * inf_ala, 0)
    
    # update cohorts
    cohorts[, 1, ][sus_apt - 1] <- cohorts[, 1, ][sus_apt - 1] + new_inf_apt
    cohorts[, 1, ][sus_apt] <- cohorts[, 1, ][sus_apt] - new_inf_apt
    cohorts[, 1, ][sus_ala - 1] <- cohorts[, 1, ][sus_ala - 1] + new_inf_ala
    cohorts[, 1, ][sus_ala] <- cohorts[, 1, ][sus_ala] - new_inf_ala
    
    ## parasitism
    
    # introduction of parasitoids
    if(current_date == lubridate::ymd(para_intro_date)){
      para_pop[[2]] <- Parasitoid@'introduction_n'
    }
    
    # identify susceptible pests (instars 2-3)
    all_susc <- sum(cohorts[, 1, ][which(cohorts[, 2, ] %in% c(2, 3))])
    
    all_parasite <- para_pop[['females']]
    
    attack_rate <- fun_attack(temperature)
    
    handling_time <- fun_handling(temperature)
    
    para_rate <- all_susc * (1 - exp((-abs(attack_rate)*all_parasite)/(1 + abs(attack_rate)*all_susc*handling_time))) * fun_para_scal(t)
    
    if(is.na(para_rate))
      para_rate <- 0
    
    # calculate new parasitised cohort
    if(para_rate > 0){
      
      new_para <- round(cohorts[, 1, ][which(cohorts[, 2, ] %in% c(2, 3))] * para_rate / all_susc, 0)
      
      new_para_cohort_n2 <- matrix(c(round(sum(new_para) * sum(cohorts[1, 1, ][which(cohorts[1, 2, ] == 2)]) / all_susc, 0),
                                     round(sum(new_para) * sum(cohorts[2, 1, ][which(cohorts[2, 2, ] == 2)]) / all_susc, 0),
                                     round(sum(new_para) * sum(cohorts[3, 1, ][which(cohorts[3, 2, ] == 2)]) / all_susc, 0),
                                     round(sum(new_para) * sum(cohorts[4, 1, ][which(cohorts[4, 2, ] == 2)]) / all_susc, 0),
                                     rep(2, 4)), ncol = 2)
      
      new_para_cohort_n3 <- matrix(c(round(sum(new_para) * sum(cohorts[1, 1, ][which(cohorts[1, 2, ] == 3)]) / all_susc, 0),
                                     round(sum(new_para) * sum(cohorts[2, 1, ][which(cohorts[2, 2, ] == 3)]) / all_susc, 0),
                                     round(sum(new_para) * sum(cohorts[3, 1, ][which(cohorts[3, 2, ] == 3)]) / all_susc, 0),
                                     round(sum(new_para) * sum(cohorts[4, 1, ][which(cohorts[4, 2, ] == 3)]) / all_susc, 0),
                                     rep(3, 4)), ncol = 2)
      
      para_cohorts <- abind::abind(para_cohorts,
                                   new_para_cohort_n2,
                                   new_para_cohort_n3,
                                   along = 3)
      
      dev_para_cohorts <- rbind(dev_para_cohorts,
                                matrix(rep(0, 8), ncol = 4))
      
      para_ages <- cbind(para_ages,
                         matrix(rep(min(cohort_ages[which(cohorts[, 2, ] %in% c(2, 3))]), 8), nrow = 4))
      
      active_para <- which(para_cohorts[, 1, ] > 0)
      
      cohorts[, 1, ][which(cohorts[, 2, ] %in% c(2, 3))] <- cohorts[, 1, ][which(cohorts[, 2, ] %in% c(2, 3))] - new_para
    }
    
    ## mortality
    # calculate mortality due to temperature and rainfall
    temp_loss <-
      fun_temp_loss(max_temp, min_temp)
    rainfall_loss <-
      fun_rainfall_loss(rainfall)
    
    # calculate mortality due to senesence
    ages <- t - cohort_ages
    
    sen_loss <- fun_sen_loss(ages)
    sen_loss[is.na(sen_loss)] <- 0
    
    # calculate mortality rate for each cohort
    daily_loss <-
      rep(temp_loss, length(sen_loss)) +
      rep(rainfall_loss, length(sen_loss)) +
      sen_loss +
      rep(bg_loss, length(sen_loss))
    
    daily_loss <- sapply(daily_loss, min, 1) # make sure mortality rate doesn't exceed 1
    
    daily_loss <- ceiling(cohorts[, 1, ] * daily_loss)
    
    # update cohort sizes
    cohorts[, 1, ] <- sapply(1:length(ages), function(x) max(cohorts[, 1, ][x] - daily_loss[x], 0))
    
    # update active cohorts vector
    active_cohorts <- which(cohorts[, 1, ] > 0)
    
    # update lifestages
    cohorts[, 2, ][-active_cohorts] <- 0
    
    ## parasitoid mortality
    ages <- t - para_ages
    
    sen_loss <- fun_sen_loss(ages)
    sen_loss[is.na(sen_loss)] <- 0
    
    # calculate mortality rate for each cohort
    daily_loss <-
      rep(temp_loss, length(sen_loss)) +
      rep(rainfall_loss, length(sen_loss)) +
      sen_loss +
      rep(bg_loss, length(sen_loss))
    
    daily_loss <- sapply(daily_loss, min, 1) # make sure mortality rate doesn't exceed 1
    
    daily_loss <- ceiling(para_cohorts[, 1, ] * daily_loss)
    
    # update cohort sizes
    para_cohorts[, 1, ] <- sapply(1:length(ages), function(x) max(para_cohorts[, 1, ][x] - daily_loss[x], 0))
    
    # update active cohorts vector
    active_para <- which(para_cohorts[, 1, ] > 0)
    
    ## offspring production (create new cohort)
    # identify adult cohorts
    all_adult <- which(cohorts[, 2, ] == 5)
    
    # calculate daily fecundity due to age
    ages <- t - adult_ages
    ages[-all_adult] <- 0 # make only active adult cohorts are counted
    
    age_prod <- fun_age_fecund(ages)
    
    # calculate temperature scalar
    temp_prod <- fun_temp_fecund(temperature)
    
    # calculate density scalar
    dens_prod <- fun_dens_fecund(sum(cohorts[, 1, ]))
    
    # calculate realised fecundity
    res_fecund <- age_prod * temp_prod * dens_prod
    
    # calculate total newly produced pests with adjustments for type
    all_prod <- as.numeric(cohorts[, 1, ])
    
    all_prod <- round(all_prod * as.numeric(res_fecund), 0)
    
    if(is.matrix(cohorts[, 1, ]))
    {names(all_prod) <- rep(rownames(cohorts[,1,]), length.out = length(all_prod))} else
    {names(all_prod) <- rep(names(cohorts[,1,]), length.out = length(all_prod))}
    
    if(length(all_prod) > 0){
      all_prod[which(names(all_prod) == "pos_apt")] <- all_prod[which(names(all_prod) == "pos_apt")] * (1 - fitness_cost)
      all_prod[which(names(all_prod) == "pos_ala")] <- all_prod[which(names(all_prod) == "pos_ala")] * (1 - fitness_cost) * (1 - alate_penalty)
      all_prod[which(names(all_prod) == "neg_ala")] <- all_prod[which(names(all_prod) == "neg_ala")] * (1 - alate_penalty)
      
      pos_prod <- round(sum(all_prod[which(names(all_prod) %in% c("pos_apt", "pos_ala"))]), 0)
      neg_prod <- round(sum(all_prod[which(names(all_prod) %in% c("neg_apt", "neg_ala"))]), 0)
    } else {
      pos_prod <- 0
      neg_prod <- 0
    }
    
    if(!vert_trans){
      neg_prod <- sum(pos_prod, neg_prod)
      pos_prod <- 0
    }
    
    # calculate proportion of alate-destined nymphs produced
    alate_prod <- fun_alate_prod(sum(cohorts[, 1, ]))
    
    # calculate newly produced pests
    pos_apt_new <- round(pos_prod * (1 - alate_prod), 0)
    neg_apt_new <- round(neg_prod * (1 - alate_prod), 0)
    pos_ala_new <- round(pos_prod * alate_prod, 0)
    neg_ala_new <- round(neg_prod * alate_prod, 0)
    
    # update cohort sizes
    if(sum(pos_apt_new, neg_apt_new, pos_ala_new, neg_ala_new) > 0){
      
      new_cohort <- matrix(c(pos_apt_new, neg_apt_new, pos_ala_new, neg_ala_new,
                             rep(1, 4)),
                           ncol = 2)
      cohorts <- abind::abind(cohorts,
                              new_cohort,
                              along = 3)
      dev_cohorts <- rbind(dev_cohorts,
                           matrix(rep(0, 4), nrow = 1))
      adult_ages <- cbind(adult_ages,
                          matrix(rep(0, 4), ncol = 1))
      cohort_ages <- cbind(cohort_ages,
                           matrix(rep(t, 4), ncol = 1))
    }
    
    # update active cohorts vector
    active_cohorts <- which(cohorts[, 1, ] > 0)
    
    # update life stages
    cohorts[, 2, ][-active_cohorts] <- 0
    
    ## update pest_pop dataframe
    pos_pops <- rbind(t(cohorts[1, , ]), t(cohorts[3, , ]))
    neg_pops <- rbind(t(cohorts[2, , ]), t(cohorts[4, , ]))
    
    pos_para <- rbind(t(para_cohorts[1, , ]), t(para_cohorts[3, , ]))
    neg_para <- rbind(t(para_cohorts[2, , ]), t(para_cohorts[4, , ]))
    
    pest_pop <- rbind(pest_pop,
                      c(t,
                        sum(pos_pops[which(pos_pops[, 2] == 1), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 1), 1], na.rm = T),
                        sum(pos_pops[which(pos_pops[, 2] == 2), 1], na.rm = T) + sum(pos_para[which(pos_para[, 2] == 2), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 2), 1], na.rm = T) + sum(neg_para[which(neg_para[, 2] == 2), 1], na.rm = T),
                        sum(pos_pops[which(pos_pops[, 2] == 3), 1], na.rm = T) + sum(pos_para[which(pos_para[, 2] == 3), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 3), 1], na.rm = T) + sum(neg_para[which(neg_para[, 2] == 3), 1], na.rm = T),
                        sum(pos_pops[which(pos_pops[, 2] == 4), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 4), 1], na.rm = T),
                        sum(pos_pops[which(pos_pops[, 2] == 5), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 5), 1], na.rm = T)))
    
    para_df <- rbind(para_df,
                     c(t = t,
                       mummies = para_pop[['mummies']],
                       females = para_pop[['females']]))
    
    para_pop[['females']] <- 0
    
    if(progress){
      utils::setTxtProgressBar(progress_bar, value = t)
    }
    
    if(sum(cohorts[, 1, ]) == 0){
      print(paste0("Population died out at time ", t, "; simulation ended"))
      break
    }
  }
  
  if(progress){
    close(progress_bar)
  }
  
  output <- new("endosym_mod",
                pest = Pest@species,
                crop = Crop@name,
                endosymbiont = Endosymbiont@name,
                parasitoid = Parasitoid@species,
                start_date = start_date,
                sim_length = sim_length,
                vert_trans = vert_trans,
                hori_trans = hori_trans,
                imi = imi,
                emi = emi,
                para = para,
                pest_df = pest_pop,
                pest_cohorts = cohorts,
                para_df = para_df,
                area = area)
  
  if (plot){
    mod_plot <- plot(output, type = "pop_size")
    
    print(mod_plot)
  }
  
  return(output)
}