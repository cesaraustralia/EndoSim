#' Endosymbiont model
#'
#' Run simulation using the endosymbiont model
#'
#' @param Pest object of class \code{pest} defining the pest
#' @param Endosymbiont object of class \code{endosym} defining the endosymbiont
#' @param Crop object of class \code{crop} defining the crop
#' @param Beneficials list of objects of class \code{beneficial} describing the beneficials in the system
#' @param init object of class \code{initial} defining the starting conditions for the simulation
#' @param conds object of class \code{sim_conds} defining the simulation conditions
#' @param plot if \code{TRUE} (default) generate plots
#' @param vert_trans set to \code{FALSE} to turn off vertical transmission
#' @param hori_trans set to \code{FALSE} to turn off horizontal transmission
#' @param imi set to \code{FALSE} to turn off immigration into paddock
#' @param emi set to \code{FALSE} to turn off emigration from paddock
#' @return object of class \code{endosym_mod}; see [endosym_mod].
#' @details
#' Constructs the endosymbiont model using the provided [pest], [endosym], [crop], [beneficials], [initial], and [sim_conds] objects.
#' 
#' The \code{Pest} species feeds on the provided \code{Crop}.
#' 
#' Vertical and horizontal transmission of endosymbiont are defined using functions in the \code{Endosymbiont}.
#' 
#' Each element of the \code{Beneficials} list should be a \code{beneficial} object with functions defining the interactions with the pest (e.g. predation, parasitism) and their impacts on mortality and endosymbiont transmission.
#' 
#' @export endosym_model


endosym_model <- function(Pest,
                          Endosymbiont,
                          Crop,
                          Beneficials = NULL,
                          init,
                          conds,
                          plot = TRUE,
                          vert_trans = TRUE,
                          hori_trans = TRUE,
                          imi = TRUE,
                          emi = TRUE
                          ) {
  if (!inherits(Pest, "pest"))
    stop("No Pest of class pest provided!")
  
  if (!inherits(Crop, "crop"))
    stop("No Crop of class crop provided!")
  
  if (!inherits(init, "initial"))
    stop("No init of class initial provided!")
  
  if (!inherits(conds, "sim_conds"))
    stop("No conds of class sim_conds provided!")
  
  if (is.null(Beneficials))
    warning("No Beneficials provided; parameterising model without beneficial species")
  
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
    Pest@'fun_imi' <- fit_null(0)
    warning("Immigration cancelled!")
  }
  
  if(!emi){
    Pest@'fun_emi' <- fit_null(0)
    warning("Emigration cancelled!")
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
  harvest_date <- Crop@'harvest_date'
  
  # define functions
  fun_dev_apt <- Pest@'fun_dev_apt'
  fun_dev_ala <- Pest@'fun_dev_ala'
  fun_imi <- Pest@'fun_imi'
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
  adult_ages <- matrix(c(rep(0, 16), rep(1, 4)), nrow = 4)
  
  # create initial matrix of cohort ages
  cohort_ages <- matrix(c(rep(0, 16), rep(1, 4)), nrow = 4)
  
  # create initial dataframe of crop population
  crop_pop <- init@'Crop'
  
  print("Running model:")
  
  progress_bar = utils::txtProgressBar(
    min = 0,
    max = sim_length,
    style = 1,
    char = "="
  )
  
  for (t in 1:sim_length){
    # current_date <- lubridate::ymd(start_date) + lubridate::days(t)
    # growth_season <- current_date %in% seq(lubridate::ymd(sowing_date), lubridate::ymd(harvest_date), "day")
    # 
    # # adjust carrying capacity if outside growth season
    # if(growth_season)
    #   fun_dens_fecund <- Pest@'fun_dens_fecund'
    # else
    #   fun_dens_fecund <- fit_bannerman(100, 0.08)
    
    temperature = env[t, 2]
    rainfall = env[t, 3]
    
    ## nymph development and adult aging
    # calculate development units gained in timestep
    dev_apt <- fun_dev_apt(temperature)
    dev_ala <- fun_dev_ala(temperature)
    
    # add development units
    dev_cohorts[, c(1, 2)] <- dev_cohorts[, c(1, 2)] + dev_apt
    dev_cohorts[, c(3, 4)] <- dev_cohorts[, c(3, 4)] + dev_ala
    
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
    
    ## dispersal
    # calculate how many adults emigrate
    emi_adult <- fun_emi(cohorts[, 1, ][new_adult])
    
    cohorts[, 1, ][new_adult] <- cohorts[, 1, ][new_adult] - emi_adult
    cohorts[, 1, ] <- sapply(cohorts[, 1, ], max, 0) # make sure pop size doesn't drop to negative
    
    # calculate how many new adults immigrate into population
    imi_adult <- fun_imi(t)
    
    if(imi_adult > 0){
      # create new cohort
      new_cohort <- matrix(c(0, 0, 0, imi_adult,
                             rep(5, 4)),
                           ncol = 2)
      cohorts <- abind::abind(cohorts,
                              new_cohort,
                              along = 3)
      dev_cohorts <- rbind(dev_cohorts,
                           matrix(rep(0, 4), nrow = 1))
      adult_ages <- cbind(adult_ages,
                          matrix(c(0, 0, 0, t), ncol = 1))
      cohort_ages <- cbind(cohort_ages,
                           matrix(c(0, 0, 0, t), ncol = 1))
    }
    
    # update active cohorts vector
    active_cohorts <- which(cohorts[, 1, ] > 0)
    
    # update life stages
    cohorts[, 2, ][-active_cohorts] <- 0
    
    ## virus horizontal transmission
    # calculate total pest moves
    tot_mov = (sum(cohorts[, 1, ]) * apterae_walk) +
      (sum(c(cohorts[3, 1, ], cohorts[4, 1, ])[which(cbind(cohorts[3, , ], cohorts[4, , ])[2, ] == 5)]) * alate_flight)
    
    # calculate number of inoculated plants
    new_inoc = tot_mov *
      sum(cohorts[c(1, 3), 1, ]) / sum(cohorts[, 1, ]) *
      fun_trans_eff(temperature) *
      sum(crop_pop[, 3]) / sum(crop_pop[, 2:3])
    
    if(is.na(new_inoc))
      new_inoc <- 0 # capture case when population dies out
    
    if(round(new_inoc, 0) > 0){
      neg_crop_ind <- which(crop_pop[, 2] == 0)[1:new_inoc]
      
      crop_pop[neg_crop_ind, 1] <- t
      crop_pop[neg_crop_ind, 2] <- 1
      crop_pop[neg_crop_ind, 3] <- 0
    }
    
    # which plants recover
    heal_ind <- which(t - crop_pop[, 1] >= heal_time)
    
    # update crop dataframe
    crop_pop[heal_ind, 2] <- 0
    crop_pop[heal_ind, 3] <- 1
    
    # calculate infection rate
    inf_rate = fun_susc(temperature) *
      (sum(crop_pop[, 2]) / sum(crop_pop[, 2:3])) *
      (sum(cohorts[c(2, 4), 1, ]) / sum(cohorts[, 1, ]))
    
    if(is.na(inf_rate))
      inf_rate <- 0 # capture case when population dies out
    
    # update cohorts
    new_inf_apt <- round(cohorts[2, 1, ] * inf_rate, 0)
    new_inf_ala <- round(cohorts[4, 1, ] * inf_rate, 0)
    
    cohorts[1, 1, ] <- cohorts[1, 1, ] + new_inf_apt
    cohorts[2, 1, ] <- cohorts[2, 1, ] - new_inf_apt
    cohorts[3, 1, ] <- cohorts[3, 1, ] + new_inf_apt
    cohorts[4, 1, ] <- cohorts[4, 1, ] - new_inf_apt
    
    ## mortality
    # calculate mortality due to temperature and rainfall
    temp_loss <-
      fun_temp_loss(temperature)
    rainfall_loss <-
      fun_rainfall_loss(rainfall)
    
    # calculate mortality due to senesence
    ages <- t - cohort_ages
    ages[which(ages == t)] <- 0
    ages[, 1] <- t # update first cohort age
    
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
    
    ## parasitism
    # TBA
    
    ## offspring production (create new cohort)
    # identify adult cohorts
    all_adult <- which(cohorts[, 2, ] == 5)
    
    # calculate daily fecundity due to age
    ages <- t - adult_ages
    ages[which(ages == t)] <- 0
    ages[, 1] <- t # fix first cohort age
    ages[-active_cohorts] <- 0 # make sure non-active cohorts are not counted
    
    age_prod <- fun_age_fecund(ages)
    
    # calculate temperature scalar
    temp_prod <- fun_temp_fecund(temperature)
    
    # calculate density scalar
    dens_prod <- fun_dens_fecund(sum(cohorts[, 1, ]))
    
    # calculate realised fecundity
    res_fecund <- age_prod * temp_prod * dens_prod
    
    # calculate total newly produced pests with adjustments for type
    all_prod <- as.numeric(cohorts[, 1, ])
    
    all_prod <- round(all_prod[all_adult] * as.numeric(res_fecund), 0)
    
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
    
    pest_pop <- rbind(pest_pop,
                      c(t,
                        sum(pos_pops[which(pos_pops[, 2] == 1), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 1), 1], na.rm = T),
                        sum(pos_pops[which(pos_pops[, 2] == 2), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 2), 1], na.rm = T),
                        sum(pos_pops[which(pos_pops[, 2] == 3), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 3), 1], na.rm = T),
                        sum(pos_pops[which(pos_pops[, 2] == 4), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 4), 1], na.rm = T),
                        sum(pos_pops[which(pos_pops[, 2] == 5), 1], na.rm = T),
                        sum(neg_pops[which(neg_pops[, 2] == 5), 1], na.rm = T)))
    
    utils::setTxtProgressBar(progress_bar, value = t)
  }
  
  close(progress_bar)
  
  if (plot){
    pop_plot <- pest_pop %>%
      tidyr::pivot_longer(cols = 2:11,
                          names_to = "lifestage",
                          values_to = "n") %>%
      dplyr::mutate(endosymbiont = ifelse(stringr::str_detect(lifestage, "pos"), "pos", "neg"),
                    lifestage = sub("_", "", stringr::str_remove(lifestage, "pos|neg|"))) %>%
      dplyr::group_by(t, endosymbiont) %>%
      dplyr::summarise(n = sum(n)) %>%
      dplyr::group_by(t) %>%
      dplyr::mutate(tot = sum(n)) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.Date(t, origin = as.Date(start_date)), y = n)) +
      ggplot2::geom_density(ggplot2::aes(fill = endosymbiont),
                            stat = "identity",
                            colour = NA,
                            position = "stack",
                            data = pest_pop %>%
                              tidyr::pivot_longer(cols = 2:11,
                                                  names_to = "lifestage",
                                                  values_to = "n") %>%
                              dplyr::mutate(endosymbiont = ifelse(stringr::str_detect(lifestage, "pos"), "pos", "neg"),
                                            lifestage = sub("_", "", stringr::str_remove(lifestage, "pos|neg|"))) %>%
                              dplyr::group_by(t, endosymbiont) %>%
                              dplyr::summarise(n = sum(n))) +
      ggplot2::geom_line(data = pest_pop %>%
                           tidyr::pivot_longer(cols = 2:11,
                                               names_to = "lifestage",
                                               values_to = "n") %>%
                           dplyr::group_by(t) %>%
                           dplyr::summarise(n = sum(n))) +
      ggplot2::labs(x = "Date",
                    y = "Total number of pests",
                    title = paste0("Fitness cost = ", fitness_cost)) +
      ggplot2::scale_fill_manual(
        values = c("darkgoldenrod", "darkgreen"),
        name = "Phenotype",
        labels = c("R-", "R+")
      ) +
      ggplot2::theme_bw()
    
    prop_plot <- pest_pop %>%
      tidyr::pivot_longer(cols = 2:11,
                          names_to = "lifestage",
                          values_to = "n") %>%
      dplyr::mutate(endosymbiont = ifelse(stringr::str_detect(lifestage, "pos"), "pos", "neg"),
                    lifestage = sub("_", "", stringr::str_remove(lifestage, "pos|neg|"))) %>%
      dplyr::group_by(t, endosymbiont) %>%
      dplyr::summarise(n = sum(n)) %>%
      dplyr::group_by(t) %>%
      dplyr::summarise(tot = sum(n),
                       prop = n / tot) %>%
      dplyr::slice(2L) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.Date(t, origin = as.Date(start_date)), y = prop)) +
      ggplot2::geom_line(colour = "darkred", size = 1) +
      ggplot2::lims(y = c(0, 1)) +
      ggplot2::labs(x = "Date",
                    y = "Proportion of R+ in population",
                    title = paste0("Fitness cost = ", fitness_cost)) +
      ggplot2::theme_bw()
    
    output <- list(pest_pop,
                   cohorts,
                   pop_plot,
                   prop_plot)
    
    names(output) <- c("pest_df",
                       "pest_cohorts",
                       "Population_plot",
                       "Endosymbiont_plot")
  } else {
    output <- list(pest_pop,
                   cohorts)
    
    names(output) <- c("pest_df",
                       "pest_cohorts")
  }
  
  class(output) <- "endosym_model"
  return(output)
}