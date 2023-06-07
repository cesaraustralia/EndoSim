library(tidyverse)
library(lubridate)
source("R/EndosymbiontModel.R")

# download_silo <- function(date, year, var, path){
#   download.file(url = paste0("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/daily/",
#                              var, "/",
#                              year, "/",
#                              date, ".",
#                              var, ".tif"),
#                 method = "curl",
#                 destfile = paste0(path, date, ".", var, ".tif"))
# }
#
# times <- seq(ymd("2022-01-01"), ymd("2022-12-31"), by = "day")
#
# for(i in 1:length(times)){
#   for(k in c("max_temp", "daily_rain")){
#     date = stringr::str_remove_all(sapply(strsplit(as.character(times[[i]]), " "), "[", 1), "-")
#     year = year(times[[i]])
#     var = k
#
#     download_silo(date, year, var, "Data/silo/")
#   }
# }

time <- seq(1:365)
Y_init <- list(
  Tot_aphids = 1000,
  I_aphids = 100,
  WT_aphids = 900,
  HealthyCP = 100,
  InfectedCP = 0,
  WT_production = 0,
  I_production = 0
)
params <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0.1,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

# test model under ranges of constant temperatures and rainfall
Y_temp <- list()
for (i in 1:6) {
  temperature = c(0, 10, 20, 30, 40, 50)[[i]]
  precipitation = 1
  prop_day = 1
  
  Y_temp[[i]] <-
    model_run(time, Y_init, params, constant = T, temperature, precipitation, prop_day) %>%
    mutate(Temperature = c(0, 10, 20, 30, 40, 50)[[i]])
}

Y_prec <- list()
for (i in 1:6) {
  temperature = 25
  precipitation = seq(0:5)[[i]]
  prop_day = 1
  
  Y_prec[[i]] <-
    model_run(time, Y_init, params, constant = T, temperature, precipitation, prop_day) %>%
    mutate(Precipitation = seq(0:5)[[i]])
}

Y_fit <- list()
for (i in 1:6) {
  temperature = 25
  precipitation = 1
  prop_day = 1
  params <- list(
    max_net_reproductive_rate = 6,
    CP_aphid_capacity = 5000,
    aphid_natural_losses = 0.1,
    fitness_cost = seq(0, 1, by = 0.2)[[i]],
    aphid_walks = 0.025,
    aphid_flights = 0.05
  )
  
  Y_fit[[i]] <-
    model_run(time, Y_init, params, constant = T, temperature, precipitation, prop_day) %>%
    mutate(Fitness = seq(0, 1, by = 0.2)[[i]])
}

params <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0.1,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

Y_start <- list()
for (i in 1:6) {
  temperature = 25
  precipitation = 1
  prop_day = 1
  Y_init <- list(
    Tot_aphids = 1000,
    I_aphids = seq(0, 0.1, by = 0.02)[[i]] * 1000,
    WT_aphids = rev(seq(0.9, 1, by = 0.02))[[i]] * 1000,
    HealthyCP = 100,
    InfectedCP = 0,
    WT_production = 0,
    I_production = 0
  )
  
  Y_start[[i]] <-
    model_run(time, Y_init, params, constant = T, temperature, precipitation, prop_day) %>%
    mutate(Initial = seq(0, 0.1, by = 0.02)[[i]])
}

# plot outputs of simulations
png("Figures/temp_prop.png", width = 600)
p_temp_prop <- bind_rows(Y_temp) %>%
  rowwise() %>%
  mutate(I_prop = I_aphids / Tot_aphids) %>%
  mutate(I_prop = case_when(is.infinite(I_prop) ~ 0,
                            TRUE ~ I_prop)) %>%
  mutate(I_prop = min(I_prop, 1)) %>%
  ggplot(aes(
    x = time,
    y = I_prop,
    colour = as.factor(Temperature)
  )) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Temperature (⁰C)") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Proportion of aphids infected",
       title = "Rainfall = 1 cm; Fitness cost = 0.1")
p_temp_prop
dev.off()

p_dml <- rvg::dml(ggobj = p_temp_prop)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "temp_prop.pptx"))

png("Figures/temp_total.png", width = 600)
p_temp_total <- bind_rows(Y_temp) %>%
  ggplot(aes(
    x = time,
    y = Tot_aphids,
    colour = as.factor(Temperature)
  )) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Temperature (⁰C)") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Aphid population density",
       title = "Rainfall = 1 cm; Fitness cost = 0.1")
p_temp_total
dev.off()

p_dml <- rvg::dml(ggobj = p_temp_total)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "temp_total.pptx"))


png("Figures/prec_prop.png", width = 600)
p_prec_prop <- bind_rows(Y_prec) %>%
  rowwise() %>%
  mutate(I_prop = I_aphids / Tot_aphids) %>%
  mutate(I_prop = case_when(is.infinite(I_prop) ~ 0,
                            TRUE ~ I_prop)) %>%
  mutate(I_prop = min(I_prop, 1)) %>%
  ggplot(aes(
    x = time,
    y = I_prop,
    colour = as.factor(Precipitation)
  )) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Rainfall (cm)") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Proportion of aphids infected",
       title = "Temperature = 25⁰C; Fitness cost = 0.1")
p_prec_prop
dev.off()

p_dml <- rvg::dml(ggobj = p_prec_prop)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "prec_prop.pptx"))

png("Figures/prec_total.png", width = 600)
p_prec_total <- bind_rows(Y_prec) %>%
  ggplot(aes(
    x = time,
    y = Tot_aphids,
    colour = as.factor(Precipitation)
  )) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Rainfall (cm)") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Aphid population density",
       title = "Temperature = 25⁰C; Fitness cost = 0.1")
p_prec_total
dev.off()

p_dml <- rvg::dml(ggobj = p_prec_total)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "prec_total.pptx"))

png("Figures/fit_prop.png", width = 600)
p_fit_prop <- bind_rows(Y_fit) %>%
  rowwise() %>%
  mutate(I_prop = I_aphids / Tot_aphids) %>%
  mutate(I_prop = case_when(is.infinite(I_prop) ~ 0,
                            TRUE ~ I_prop)) %>%
  mutate(I_prop = min(I_prop, 1)) %>%
  ggplot(aes(
    x = time,
    y = I_prop,
    colour = as.factor(Fitness)
  )) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Fitness cost") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Proportion of aphids infected",
       title = "Temperature = 25⁰C; Rainfall = 1 cm")
p_fit_prop
dev.off()

p_dml <- rvg::dml(ggobj = p_fit_prop)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "fit_prop.pptx"))

png("Figures/fit_total.png", width = 600)
p_fit_total <- bind_rows(Y_fit) %>%
  ggplot(aes(
    x = time,
    y = Tot_aphids,
    colour = as.factor(Fitness)
  )) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Fitness cost") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Aphid population density",
       title = "Temperature = 25⁰C; Rainfall = 1 cm")
p_fit_total
dev.off()

p_dml <- rvg::dml(ggobj = p_fit_total)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "fit_total.pptx"))

png("Figures/init_prop.png", width = 600)
p_init_prop <- bind_rows(Y_start) %>%
  rowwise() %>%
  mutate(I_prop = I_aphids / Tot_aphids) %>%
  mutate(I_prop = case_when(is.infinite(I_prop) ~ 0,
                            TRUE ~ I_prop)) %>%
  mutate(I_prop = min(I_prop, 1)) %>%
  ggplot(aes(
    x = time,
    y = I_prop,
    colour = as.factor(Initial)
  )) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Initial Ai/A") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Proportion of aphids infected",
       title = "Temperature = 25⁰C; Rainfall = 1 cm; Fitness cost = 0.1")
p_init_prop
dev.off()

p_dml <- rvg::dml(ggobj = p_init_prop)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "init_prop.pptx"))

png("Figures/init_total.png", width = 600)
p_init_total <- bind_rows(Y_start) %>%
  ggplot(aes(
    x = time,
    y = Tot_aphids,
    colour = as.factor(Initial)
  )) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Initial Ai/A") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Aphid population density",
       title = "Temperature = 25⁰C; Rainfall = 1 cm; Fitness cost = 0.1")
p_init_total
dev.off()

p_dml <- rvg::dml(ggobj = p_init_total)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "init_total.pptx"))

# plot population dynamics through time under different fitness costs
for (i in 1:6) {
  png(paste("Figures/fit_", unique(Y_fit[[i]]$Fitness), "_prop.png", sep = ""), width = 600)
  print(
    Y_fit[[i]] %>%
      gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
      ggplot(aes(
        x = time, y = aphid_pop, fill = status
      )) +
      geom_density(stat = "identity", position = "stack") +
      labs(
        x = "Time from introduction",
        y = "Total number of aphids",
        title = paste(
          "Temperature = 25⁰C; Rainfall = 1 cm; Fitness cost = ",
          unique(Y_fit[[i]]$Fitness),
          sep = ""
        )
      ) +
      scale_fill_manual(
        values = c("dark green", "darkgoldenrod"),
        name = "Status",
        labels = c("Infected", "Healthy")
      ) +
      theme_bw()
  )
  dev.off()
}

read_csv("Data/2021/st_r_Data_Precipitation.csv") %>% write_csv("Data/st_r_Data_Precipitation.csv")
read_csv("Data/2021/st_r_Data_Temperature.csv") %>% write_csv("Data/st_r_Data_Temperature.csv")

# conditions throughout 2021 in Wagga Wagga:
png("Figures/Waga_waga_2021.png", width = 600)
p_cond <- left_join(
  read_csv("Data/st_r_Data_Precipitation.csv"),
  read_csv("Data/st_r_Data_Temperature.csv")
) %>%
  gather(variable, value, -t) %>%
  ggplot(aes(
    x = as.Date(t, origin = as.Date("2021-01-01")),
    y = value,
    colour = variable
  )) +
  geom_line() +
  scale_colour_manual(values = c("dark blue", "dark red"), name = "Variable") +
  theme_bw() +
  facet_wrap(vars(variable), ncol = 1, scales = "free") +
  theme(legend.position = "none") +
  xlab("Time")
p_cond
dev.off()

p_dml <- rvg::dml(ggobj = p_cond)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "Waga_waga_2021.pptx"))


# model aphid population in Wagga Wagga over 2021
time <- seq(1:365)
Y_init <- list(
  Tot_aphids = 1000,
  I_aphids = 100,
  WT_aphids = 900,
  HealthyCP = 100,
  InfectedCP = 0,
  WT_production = 0,
  I_production = 0
)
params <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0.5,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

png("Figures/Wagga_Wagga_2021_model.png", width = 600)
ww_m <- model_run(time,
                  sowing_period = c(4:10),
                  start_date = "2021-01-01", Y_init = Y_init, params = params, plot = T, constant = F)
dev.off()

# model null aphid population in Wagga Wagga (no infected)
params_null <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_null <-
  model_run(time,
            sowing_period = c(4:10),
            start_date = "2021-01-01",
            Y_init,
            params_null,
            plot = T,
            constant = F)

png("Figures/Wagga_wagga_2021_prop.png", width = 600)
p_ww <- ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
  ggplot(aes(x = as.Date(time, origin = as.Date("2021-01-01")))) +
  geom_line(aes(y = Tot_aphids, colour = "red"), data = ww_null) +
  geom_density(aes(y = aphid_pop, fill = status),
               stat = "identity",
               position = "stack") +
  labs(x = "Date",
       y = "Total number of aphids",
       title = "Wagga Wagga 2021; Fitness cost = 0.5") +
  scale_fill_manual(
    values = c("dark green", "darkgoldenrod"),
    name = "Status",
    labels = c("Infected", "Healthy")
  ) +
  scale_colour_manual(values = "red",
                      labels = "Null",
                      name = "") +
  theme_bw()
p_ww
dev.off()

p_dml <- rvg::dml(ggobj = p_ww)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "Waga_waga_2021_prop.pptx"))


# sample environmental conditions in Wagga Wagga in 2022:
dat_22 <- dir("Data/silo")
dat_22_rain <- dat_22[which(str_detect(dat_22, "rain"))]
dat_22_max_temp <- dat_22[which(str_detect(dat_22, "max_temp"))]
dat_22_min_temp <- dat_22[which(str_detect(dat_22, "min_temp"))]

tibble(t = 1:365,
       Precipitation = sapply(1:365,
                              function(x)
                                raster::extract(
                                  x = raster::raster(paste0("Data/silo/", dat_22_rain[x])),
                                  y = data.frame(x = 147.4573, y = -35.1583)
                                ))) %>%
  write_csv("Data/st_r_Data_Precipitation.csv")

tibble(
  t = 1:365,
  Temperature =
    (sapply(1:365,
           function(x)
             raster::extract(
               x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
               y = data.frame(x = 147.4573, y = -35.1583)
             )) +
    sapply(1:365,
           function(x)
             raster::extract(
               x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
               y = data.frame(x = 147.4573, y = -35.1583)
             ))
    )/2
) %>%
  write_csv("Data/st_r_Data_Temperature.csv")

chillR::stack_hourly_temps(tibble(date = as.Date(0:364,
                                                 origin = as.Date("2022-01-01")),
                                  Tmax = sapply(1:365,
                                                function(x)
                                                  raster::extract(
                                                    x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                                                    y = data.frame(x = 147.4573, y = -35.1583)
                                                  )),
                                  Tmin = sapply(1:365,
                                                function(x)
                                                  raster::extract(
                                                    x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                                                    y = data.frame(x = 147.4573, y = -35.1583)
                                                  ))) %>%
                             mutate(Year = year(date),
                                    Month = month(date),
                                    Day = day(date)) %>%
                             dplyr::select(Year, Month, Day, Tmax, Tmin),
                           latitude = -35.1583)$hourtemps %>%
  group_by(JDay) %>%
  summarise(AUC_full = auc_thresh(Hour, Temp, 4, 30)[[1]],
            AUC_thresh = auc_thresh(Hour, Temp, 4, 30)[[2]],
            Prop_day = AUC_thresh/AUC_full) %>%
  dplyr::select(JDay, Prop_day) %>%
  rename(t = JDay) %>%
  write_csv("Data/st_r_Data_Prop_day.csv")

# conditions throughout 2022 in Wagga Wagga:
png("Figures/Waga_waga_2022.png", width = 3000, height = 1500, res = 300)
p_cond <-  left_join(
  read_csv("Data/st_r_Data_Precipitation.csv"),
  read_csv("Data/st_r_Data_Temperature.csv"),
) %>%
  gather(variable, value, -t) %>%
  left_join(tibble(
    t = 1:365,
    Max =
      sapply(1:365,
             function(x)
               raster::extract(
                 x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                 y = data.frame(x = 147.4573, y = -35.1583)
               )),
    Min = sapply(1:365,
                 function(x)
                   raster::extract(
                     x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                     y = data.frame(x = 147.4573, y = -35.1583)
                   ))
    
  ) %>%
    mutate(variable = "Temperature")) %>%
  ggplot(aes(
    x = as.Date(t-1, origin = as.Date("2022-01-01")),
    y = value,
    colour = variable,
  )) +
  geom_ribbon(aes(ymax = Max, ymin = Min), colour = NA, fill = "darkred", alpha = .2) +
  geom_line() +
  geom_hline(aes(yintercept = yintercept), colour = "black", linetype = "dashed", data = tibble(yintercept = c(4, 30, 3), variable = c("Temperature", "Temperature", "Precipitation"))) +
  scale_colour_manual(values = c("darkblue", "darkred"), name = "Variable") +
  theme_bw() +
  facet_wrap(vars(variable), ncol = 1, scales = "free") +
  theme(legend.position = "none") +
  xlab("Date")
p_cond
dev.off()

p_dml <- rvg::dml(ggobj = p_cond)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, officer::ph_location()) %>%
  # export slide -----
base::print(target = here::here("Figures",
                                "Waga_waga_2022.pptx"))

# model aphid population in Wagga Wagga over 2022
time <- seq(1:365)
Y_init <- list(
  Tot_aphids = 100,
  I_aphids = 10,
  WT_aphids = 90,
  HealthyCP = 100,
  InfectedCP = 0,
  WT_production = 0,
  I_production = 0
)
params <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0.5,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

png("Figures/Wagga_Wagga_2022_model.png", width = 600)
ww_m <- model_run(time, Y_init,
                  sowing_period = c(4:10),
                  start_date = "2022-01-01", params, plot = T, constant = F)
dev.off()

# model null aphid population in Wagga Wagga (no infected)
params_null <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_null <-
  model_run(time,
            Y_init,
            sowing_period = c(4:10),
            start_date = "2022-01-01",
            params_null,
            plot = T,
            constant = F)

png("Figures/Wagga_wagga_2022_prop.png", width = 3000, height = 1500, res = 300)
p_ww <- ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
  ggplot(aes(x = as.Date(time, origin = as.Date("2022-01-01")))) +
  geom_line(aes(y = Tot_aphids, colour = "red"), data = ww_null) +
  geom_density(aes(y = aphid_pop, fill = status),
               stat = "identity",
               position = "stack") +
  labs(x = "Date",
       y = "Total number of aphids",
       title = "Wagga Wagga 2022; Fitness cost = 0.5") +
  scale_fill_manual(
    values = c("dark green", "darkgoldenrod"),
    name = "Status",
    labels = c("Infected", "Healthy")
  ) +
  scale_colour_manual(values = "red",
                      labels = "Null (no endosymbionts)",
                      name = "") +
  theme_bw()
p_ww
dev.off()

p_dml <- rvg::dml(ggobj = p_ww)

png("Figures/Wagga_wagga_2022_sowing_period.png", width = 3000, height = 1500, res = 300)
ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>% mutate(model = "Endosymbionts") %>%
  bind_rows(ww_null %>%
              gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>% mutate(status = "Null (no endosymbionts)", aphid_pop = Tot_aphids) %>% mutate(model = "Null (no endosymbionts)")) %>%
  bind_rows(ww_m %>%
              gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>% mutate(status = "Endosymbionts", aphid_pop = Tot_aphids) %>% mutate(model = "Endosymbionts")) %>%
  mutate(time = as.Date(time, origin = as.Date("2022-01-01")),
         month_t = month(time),
         status = factor(status, levels = c("I_aphids", "WT_aphids", "Endosymbionts", "Null (no endosymbionts)"))) %>%
  ggplot(aes(x = time)) +
  geom_point(aes(y = aphid_pop, colour = status), se = F, alpha = 0.1) +
  geom_smooth(aes(y = aphid_pop, colour = status, fill = status, linetype = status), span = 0.1) +
  scale_colour_manual(
    values = c("dark green", "darkgoldenrod", "red", "darkred"),
    name = "Modelled population",
    labels = c("Infected", "Healthy", "Total", "Null (no endosymbionts)")
  ) +
  scale_fill_manual(
    values = c("dark green", "darkgoldenrod", "red", "darkred"),
    name = "Modelled population",
    labels = c("Infected", "Healthy", "Total", "Null (no endosymbionts)")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "solid", "dashed"),
    name = "Modelled population",
    labels = c("Infected", "Healthy", "Total", "Null (no endosymbionts)")
  ) +
  labs(x = "Date",
       y = "Total number of aphids",
       title = "Wagga Wagga 2022; Fitness cost = 0.5") +
  # scale_x_continuous(breaks = c(1, 4, 7, 10, 13),
  #                    labels = c("Jan 2022", "Apr 2022", "Jul 2022", "Oct 22", "Jan 2023")) +
  theme_bw()
dev.off()


# model two consecutive years in Wagga Wagga:
# generate prop_day and mean_temp data for 2021 by extrapolating from prop_day ~ max_temp in 2022:
model_day <- mgcv::gam(Prop_day ~ 
                            s(Temperature),
                       data = tibble(Prop_day = read_csv("Data/st_r_Data_Prop_day.csv")$Prop_day,
                                     Temperature = sapply(1:365,
                                                          function(x)
                                                            raster::extract(
                                                              x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                                                              y = data.frame(x = 147.4573, y = -35.1583)
                                                            ))
                                     ))

model_temp <- mgcv::gam(Temperature ~ 
                         s(Tmax),
                       data = tibble(Temperature = read_csv("Data/st_r_Data_Temperature.csv")$Temperature,
                                     Tmax = sapply(1:365,
                                                          function(x)
                                                            raster::extract(
                                                              x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                                                              y = data.frame(x = 147.4573, y = -35.1583)
                                                            ))
                       ))

bind_rows(read_csv("Data/2021/st_r_Data_Precipitation.csv"),
          read_csv("Data/st_r_Data_Precipitation.csv")) %>%
  mutate(t = row_number()) %>% write_csv("Data/st_r_Data_Precipitation.csv")

bind_rows(tibble(t = 1:365,
       Prop_day = sapply(predict(model_day, tibble(Temperature = read_csv("Data/2021/st_r_Data_Temperature.csv")$Temperature)), function(x) min(1, x))),
       read_csv("Data/st_r_Data_Prop_day.csv")) %>%
  mutate(t = row_number()) %>%
  write_csv("Data/st_r_Data_Prop_day.csv")

bind_rows(tibble(t = 1:365,
                 Temperature = round(predict(model_temp, tibble(Tmax = read_csv("Data/2021/st_r_Data_Temperature.csv")$Temperature)), 1)),
          read_csv("Data/st_r_Data_Temperature.csv")) %>%
  mutate(t = row_number(),
         Temperature = as.numeric(Temperature)) %>%
  write_csv("Data/st_r_Data_Temperature.csv")

left_join(
  read_csv("Data/st_r_Data_Precipitation.csv"),
  read_csv("Data/st_r_Data_Temperature.csv")
) %>%
  gather(variable, value, -t) %>%
  ggplot(aes(
    x = as.Date(t, origin = as.Date("2021-01-01")),
    y = value,
    colour = variable
  )) +
  geom_line() +
  scale_colour_manual(values = c("dark blue", "dark red"), name = "Variable") +
  theme_bw() +
  facet_wrap(vars(variable), ncol = 1, scales = "free") +
  theme(legend.position = "none") +
  xlab("Time")

# model aphid population
time <- seq(1:730)
Y_init <- list(
  Tot_aphids = 100,
  I_aphids = 10,
  WT_aphids = 90,
  HealthyCP = 100,
  InfectedCP = 0,
  WT_production = 0,
  I_production = 0
)
params <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0.5,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_m <- model_run(time, Y_init,
                  sowing_period = c(4:10),
                  start_date = "2021-01-01",
                  params,
                  plot = T,
                  constant = F)

# model null aphid population (no infected)
params_null <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_null <-
  model_run(time,
            Y_init,
            sowing_period = c(4:10),
            start_date = "2021-01-01",
            params_null,
            plot = T,
            constant = F)

ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
  ggplot(aes(x = as.Date(time, origin = as.Date("2021-01-01")))) +
  geom_line(aes(y = Tot_aphids, colour = "red"), data = ww_null) +
  geom_density(aes(y = aphid_pop, fill = status),
               stat = "identity",
               position = "stack") +
  labs(x = "Date",
       y = "Total number of aphids",
       title = "Fitness cost = 0.5") +
  scale_fill_manual(
    values = c("dark green", "darkgoldenrod"),
    name = "Status",
    labels = c("Infected", "Healthy")
  ) +
  scale_colour_manual(values = "red",
                      labels = "Null",
                      name = "") +
  theme_bw()

png("Figures/Wagga_wagga_two_years_sowing_period_per_month.png", width = 600)
ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
  filter(status == "I_aphids") %>%
  mutate(time = as.Date(time, origin = as.Date("2021-01-01")),
         year = year(time),
         month_t = ifelse(year == 2021, month(time), month(time) + 12)) %>%
  ggplot(aes(x = month_t)) +
  geom_point(aes(y = Tot_aphids), colour = "red", data = ww_null %>%
               mutate(time = as.Date(time, origin = as.Date("2021-01-01")),
                      year = year(time),
                      month_t = ifelse(year == 2021, month(time), month(time) + 12)), alpha = 0.1) +
  geom_smooth(aes(y = Tot_aphids), colour = "red", fill = "red", data = ww_null %>%
                mutate(time = as.Date(time, origin = as.Date("2021-01-01")),
                       year = year(time),
                       month_t = ifelse(year == 2021, month(time), month(time) + 12)), span = 0.1) +
  geom_point(aes(y = Tot_aphids), colour = "darkgreen", se = F, alpha = 0.1) +
  geom_smooth(aes(y = aphid_pop), colour = "darkgreen",
              fill = "darkgreen", span = 0.1) +
  labs(x = "Month",
       y = "Total number of aphids",
       title = "Fitness cost = 0.5") +
  scale_x_continuous(breaks = c(1, 4, 7, 10, 13, 16, 19, 22),
                     labels = c("Jan 21", "Apr 21", "Jul 21", "Oct 21", "Jan 22", "Apr 22", "Jul 22", "Oct 22")) +
  theme_bw()
dev.off()

# sample environmental conditions in random spot in 2022 no. 2:
dat_22 <- dir("Data/silo")
dat_22_rain <- dat_22[which(str_detect(dat_22, "rain"))]
dat_22_max_temp <- dat_22[which(str_detect(dat_22, "max_temp"))]
dat_22_min_temp <- dat_22[which(str_detect(dat_22, "min_temp"))]

tibble(t = 1:365,
       Precipitation = sapply(1:365,
                              function(x)
                                raster::extract(
                                  x = raster::raster(paste0("Data/silo/", dat_22_rain[x])),
                                  y = data.frame(x = 146.33, y = -42.4281)
                                ))) %>%
  write_csv("Data/st_r_Data_Precipitation.csv")

tibble(t = 1:365,
       Temperature = (
         sapply(1:365,
                function(x)
                  raster::extract(
                    x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                    y = data.frame(x = 146.33, y = -42.4281)
                  )) +
           sapply(1:365,
                  function(x)
                    raster::extract(
                      x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                      y = data.frame(x = 146.33, y = -42.4281)
                    ))
       ) / 2) %>%
  write_csv("Data/st_r_Data_Temperature.csv")

chillR::stack_hourly_temps(tibble(date = as.Date(0:364,
                                                 origin = as.Date("2022-01-01")),
                                  Tmax = sapply(1:365,
                                                function(x)
                                                  raster::extract(
                                                    x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                                                    y = data.frame(x = 146.33, y = -42.4281)
                                                  )),
                                  Tmin = sapply(1:365,
                                                function(x)
                                                  raster::extract(
                                                    x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                                                    y = data.frame(x = 146.33, y = -42.4281)
                                                  ))) %>%
                             mutate(Year = year(date),
                                    Month = month(date),
                                    Day = day(date)) %>%
                             dplyr::select(Year, Month, Day, Tmax, Tmin),
                           latitude = -42.4281)$hourtemps %>%
  group_by(JDay) %>%
  summarise(AUC_full = auc_thresh(Hour, Temp, 4, 30)[[1]],
            AUC_thresh = auc_thresh(Hour, Temp, 4, 30)[[2]],
            Prop_day = AUC_thresh/AUC_full) %>%
  dplyr::select(JDay, Prop_day) %>%
  rename(t = JDay) %>%
  write_csv("Data/st_r_Data_Prop_day.csv")

left_join(
  read_csv("Data/st_r_Data_Precipitation.csv"),
  read_csv("Data/st_r_Data_Temperature.csv"),
) %>%
  gather(variable, value, -t) %>%
  left_join(tibble(
    t = 1:365,
    Max =
      sapply(1:365,
             function(x)
               raster::extract(
                 x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                 y = data.frame(x = 146.33, y = -42.4281)
               )),
    Min = sapply(1:365,
                 function(x)
                   raster::extract(
                     x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                     y = data.frame(x = 146.33, y = -42.4281)
                   ))
    
  ) %>%
    mutate(variable = "Temperature")) %>%
  ggplot(aes(
    x = as.Date(t-1, origin = as.Date("2022-01-01")),
    y = value,
    colour = variable,
  )) +
  geom_ribbon(aes(ymax = Max, ymin = Min), colour = NA, fill = "darkred", alpha = .2) +
  geom_line() +
  geom_hline(aes(yintercept = yintercept), colour = "black", linetype = "dashed", data = tibble(yintercept = c(4, 30, 3), variable = c("Temperature", "Temperature", "Precipitation"))) +
  scale_colour_manual(values = c("darkblue", "darkred"), name = "Variable") +
  theme_bw() +
  facet_wrap(vars(variable), ncol = 1, scales = "free") +
  theme(legend.position = "none") +
  xlab("Date")

# model aphid population
time <- seq(1:365)
Y_init <- list(
  Tot_aphids = 1000,
  I_aphids = 100,
  WT_aphids = 900,
  HealthyCP = 100,
  InfectedCP = 0,
  WT_production = 0,
  I_production = 0
)
params <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0.5,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_m <- model_run(time, Y_init,
                  sowing_period = c(4:10),
                  start_date = "2022-01-01", params, plot = T, constant = F)

# model null aphid population (no infected)
params_null <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_null <-
  model_run(time,
            Y_init,
            sowing_period = c(4:10),
            start_date = "2022-01-01",
            params_null,
            plot = T,
            constant = F)

ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
  ggplot(aes(x = as.Date(time, origin = as.Date("2022-01-01")))) +
  geom_line(aes(y = Tot_aphids, colour = "red"), data = ww_null) +
  geom_density(aes(y = aphid_pop, fill = status),
               stat = "identity",
               position = "stack") +
  labs(x = "Date",
       y = "Total number of aphids",
       title = "Fitness cost = 0.5") +
  scale_fill_manual(
    values = c("dark green", "darkgoldenrod"),
    name = "Status",
    labels = c("Infected", "Healthy")
  ) +
  scale_colour_manual(values = "red",
                      labels = "Null",
                      name = "") +
  theme_bw()



# sample environmental conditions in Nhill (Victoria) in 2022:
dat_22 <- dir("Data/silo")
dat_22_rain <- dat_22[which(str_detect(dat_22, "rain"))]
dat_22_max_temp <- dat_22[which(str_detect(dat_22, "max_temp"))]
dat_22_min_temp <- dat_22[which(str_detect(dat_22, "min_temp"))]

tibble(t = 1:365,
       Precipitation = sapply(1:365,
                              function(x)
                                raster::extract(
                                  x = raster::raster(paste0("Data/silo/", dat_22_rain[x])),
                                  y = data.frame(x = 141.6268587, y = -36.2781177)
                                ))) %>%
  write_csv("Data/st_r_Data_Precipitation.csv")

tibble(t = 1:365,
       Temperature = (
         sapply(1:365,
                function(x)
                  raster::extract(
                    x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                    y = data.frame(x = 141.6268587, y = -36.2781177)
                  )) +
           sapply(1:365,
                  function(x)
                    raster::extract(
                      x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                      y = data.frame(x = 141.6268587, y = -36.2781177)
                    ))
       ) / 2) %>%
  write_csv("Data/st_r_Data_Temperature.csv")

chillR::stack_hourly_temps(tibble(date = as.Date(0:364,
                                                 origin = as.Date("2022-01-01")),
                                  Tmax = sapply(1:365,
                                                function(x)
                                                  raster::extract(
                                                    x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                                                    y = data.frame(x = 141.6268587, y = -36.2781177)
                                                  )),
                                  Tmin = sapply(1:365,
                                                function(x)
                                                  raster::extract(
                                                    x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                                                    y = data.frame(x = 141.6268587, y = -36.2781177)
                                                  ))) %>%
                             mutate(Year = year(date),
                                    Month = month(date),
                                    Day = day(date)) %>%
                             dplyr::select(Year, Month, Day, Tmax, Tmin),
                           latitude = -36.2781177)$hourtemps %>%
  group_by(JDay) %>%
  summarise(AUC_full = auc_thresh(Hour, Temp, 4, 30)[[1]],
            AUC_thresh = auc_thresh(Hour, Temp, 4, 30)[[2]],
            Prop_day = AUC_thresh/AUC_full) %>%
  dplyr::select(JDay, Prop_day) %>%
  rename(t = JDay) %>%
  write_csv("Data/st_r_Data_Prop_day.csv")


left_join(
  read_csv("Data/st_r_Data_Precipitation.csv"),
  read_csv("Data/st_r_Data_Temperature.csv"),
) %>%
  gather(variable, value, -t) %>%
  left_join(tibble(
    t = 1:365,
    Max =
      sapply(1:365,
             function(x)
               raster::extract(
                 x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                 y = data.frame(x = 141.6268587, y = -36.2781177)
               )),
    Min = sapply(1:365,
                 function(x)
                   raster::extract(
                     x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                     y = data.frame(x = 141.6268587, y = -36.2781177)
                   ))
    
  ) %>%
    mutate(variable = "Temperature")) %>%
  ggplot(aes(
    x = as.Date(t-1, origin = as.Date("2022-01-01")),
    y = value,
    colour = variable,
  )) +
  geom_ribbon(aes(ymax = Max, ymin = Min), colour = NA, fill = "darkred", alpha = .2) +
  geom_line() +
  geom_hline(aes(yintercept = yintercept), colour = "black", linetype = "dashed", data = tibble(yintercept = c(4, 30, 3), variable = c("Temperature", "Temperature", "Precipitation"))) +
  scale_colour_manual(values = c("darkblue", "darkred"), name = "Variable") +
  theme_bw() +
  facet_wrap(vars(variable), ncol = 1, scales = "free") +
  theme(legend.position = "none") +
  xlab("Date")

# model aphid population
time <- seq(1:365)
Y_init <- list(
  Tot_aphids = 1000,
  I_aphids = 100,
  WT_aphids = 900,
  HealthyCP = 100,
  InfectedCP = 0,
  WT_production = 0,
  I_production = 0
)
params <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0.5,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_m <- model_run(time, Y_init,
                  sowing_period = c(4:10),
                  start_date = "2022-01-01", params, plot = T, constant = F)

# model null aphid population (no infected)
params_null <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_null <-
  model_run(time,
            Y_init,
            sowing_period = c(4:10),
            start_date = "2022-01-01",
            params_null,
            plot = T,
            constant = F)

ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
  ggplot(aes(x = as.Date(time, origin = as.Date("2022-01-01")))) +
  geom_line(aes(y = Tot_aphids, colour = "red"), data = ww_null) +
  geom_density(aes(y = aphid_pop, fill = status),
               stat = "identity",
               position = "stack") +
  labs(x = "Date",
       y = "Total number of aphids",
       title = "Fitness cost = 0.5") +
  scale_fill_manual(
    values = c("dark green", "darkgoldenrod"),
    name = "Status",
    labels = c("Infected", "Healthy")
  ) +
  scale_colour_manual(values = "red",
                      labels = "Null",
                      name = "") +
  theme_bw()


# sample environmental conditions in Kwinwana (WA) in 2022:
dat_22 <- dir("Data/silo")
dat_22_rain <- dat_22[which(str_detect(dat_22, "rain"))]
dat_22_max_temp <- dat_22[which(str_detect(dat_22, "max_temp"))]
dat_22_min_temp <- dat_22[which(str_detect(dat_22, "min_temp"))]

tibble(t = 1:365,
       Precipitation = sapply(1:365,
                              function(x)
                                raster::extract(
                                  x = raster::raster(paste0("Data/silo/", dat_22_rain[x])),
                                  y = data.frame(x = 117.177886, y = -32.874615)
                                ))) %>%
  write_csv("Data/st_r_Data_Precipitation.csv")

tibble(t = 1:365,
       Temperature = (
         sapply(1:365,
                function(x)
                  raster::extract(
                    x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                    y = data.frame(x = 117.177886, y = -32.874615)
                  )) +
           sapply(1:365,
                  function(x)
                    raster::extract(
                      x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                      y = data.frame(x = 117.177886, y = -32.874615)
                    ))
       ) / 2) %>%
  write_csv("Data/st_r_Data_Temperature.csv")

chillR::stack_hourly_temps(tibble(date = as.Date(0:364,
                                                 origin = as.Date("2022-01-01")),
                                  Tmax = sapply(1:365,
                                                function(x)
                                                  raster::extract(
                                                    x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                                                    y = data.frame(x = 117.177886, y = -32.874615)
                                                  )),
                                  Tmin = sapply(1:365,
                                                function(x)
                                                  raster::extract(
                                                    x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                                                    y = data.frame(x = 117.177886, y = -32.874615)
                                                  ))) %>%
                             mutate(Year = year(date),
                                    Month = month(date),
                                    Day = day(date)) %>%
                             dplyr::select(Year, Month, Day, Tmax, Tmin),
                           latitude = -32.874615)$hourtemps %>%
  group_by(JDay) %>%
  summarise(AUC_full = auc_thresh(Hour, Temp, 4, 30)[[1]],
            AUC_thresh = auc_thresh(Hour, Temp, 4, 30)[[2]],
            Prop_day = AUC_thresh/AUC_full) %>%
  dplyr::select(JDay, Prop_day) %>%
  rename(t = JDay) %>%
  write_csv("Data/st_r_Data_Prop_day.csv")


left_join(
  read_csv("Data/st_r_Data_Precipitation.csv"),
  read_csv("Data/st_r_Data_Temperature.csv"),
) %>%
  gather(variable, value, -t) %>%
  left_join(tibble(
    t = 1:365,
    Max =
      sapply(1:365,
             function(x)
               raster::extract(
                 x = raster::raster(paste0("Data/silo/", dat_22_max_temp[x])),
                 y = data.frame(x = 117.177886, y = -32.874615)
               )),
    Min = sapply(1:365,
                 function(x)
                   raster::extract(
                     x = raster::raster(paste0("Data/silo/", dat_22_min_temp[x])),
                     y = data.frame(x = 117.177886, y = -32.874615)
                   ))
    
  ) %>%
    mutate(variable = "Temperature")) %>%
  ggplot(aes(
    x = as.Date(t-1, origin = as.Date("2022-01-01")),
    y = value,
    colour = variable,
  )) +
  geom_ribbon(aes(ymax = Max, ymin = Min), colour = NA, fill = "darkred", alpha = .2) +
  geom_line() +
  geom_hline(aes(yintercept = yintercept), colour = "black", linetype = "dashed", data = tibble(yintercept = c(4, 30, 3), variable = c("Temperature", "Temperature", "Precipitation"))) +
  scale_colour_manual(values = c("darkblue", "darkred"), name = "Variable") +
  theme_bw() +
  facet_wrap(vars(variable), ncol = 1, scales = "free") +
  theme(legend.position = "none") +
  xlab("Date")

# model aphid population
time <- seq(1:365)
Y_init <- list(
  Tot_aphids = 1000,
  I_aphids = 100,
  WT_aphids = 900,
  HealthyCP = 100,
  InfectedCP = 0,
  WT_production = 0,
  I_production = 0
)
params <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0.5,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_m <- model_run(time, Y_init,
                  sowing_period = c(4:10),
                  start_date = "2022-01-01", params, plot = T, constant = F)

# model null aphid population (no infected)
params_null <- list(
  max_net_reproductive_rate = 6,
  CP_aphid_capacity = 5000,
  aphid_natural_losses = 0.1,
  fitness_cost = 0,
  aphid_walks = 0.025,
  aphid_flights = 0.05
)

ww_null <-
  model_run(time,
            Y_init,
            sowing_period = c(4:10),
            start_date = "2022-01-01",
            params_null,
            plot = T,
            constant = F)

ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
  ggplot(aes(x = as.Date(time, origin = as.Date("2022-01-01")))) +
  geom_line(aes(y = Tot_aphids, colour = "red"), data = ww_null) +
  geom_density(aes(y = aphid_pop, fill = status),
               stat = "identity",
               position = "stack") +
  labs(x = "Date",
       y = "Total number of aphids",
       title = "Fitness cost = 0.5") +
  scale_fill_manual(
    values = c("dark green", "darkgoldenrod"),
    name = "Status",
    labels = c("Infected", "Healthy")
  ) +
  scale_colour_manual(values = "red",
                      labels = "Null",
                      name = "") +
  theme_bw()
