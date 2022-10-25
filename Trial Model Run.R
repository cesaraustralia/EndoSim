source("R/EndosymbiontModel.R")

time <- seq(1:365)
Y_init <- list(Tot_aphids = 1000,
               I_aphids = 100,
               WT_aphids = 900,
               HealthyCP = 100,
               InfectedCP = 0,
               WT_production = 0,
               I_production = 0)
params <- list(max_net_reproductive_rate = 1,
               CP_aphid_capacity = 5000,
               aphid_natural_losses = 0.1,
               fitness_cost = 0.1,
               aphid_walks = 0.025,
               aphid_flights = 0.05)

# test model under ranges of constant temperatures and rainfall
Y_temp <- list()
for (i in 1:6){
  temperature = c(0, 10, 20, 30, 40, 50)[[i]]
  precipitation = 1
  
  Y_temp[[i]] <- model_run(time, Y_init, params, constant = T, temperature, precipitation) %>%
    mutate(Temperature = c(0, 10, 20, 30, 40, 50)[[i]])
}

Y_prec <- list()
for (i in 1:6){
  temperature = 25
  precipitation = seq(0:5)[[i]]
  
  Y_prec[[i]] <- model_run(time, Y_init, params, constant = T, temperature, precipitation) %>%
    mutate(Precipitation = seq(0:5)[[i]])
}

Y_fit <- list()
for (i in 1:6){
  temperature = 25
  precipitation = 1
  params <- list(max_net_reproductive_rate = 1,
                 CP_aphid_capacity = 5000,
                 aphid_natural_losses = 0.1,
                 fitness_cost = seq(0, 1, by = 0.2)[[i]],
                 aphid_walks = 0.025,
                 aphid_flights = 0.05)
  
  Y_fit[[i]] <- model_run(time, Y_init, params, constant = T, temperature, precipitation) %>%
    mutate(Fitness = seq(0, 1, by = 0.2)[[i]])
}

# plot outputs of simulations
png("Figures/temp_prop.png", width = 600)
bind_rows(Y_temp) %>%
  rowwise() %>%
  mutate(I_prop = I_aphids/Tot_aphids) %>%
  mutate(I_prop = case_when(is.infinite(I_prop) ~ 0,
                            TRUE ~ I_prop)) %>%
  mutate(I_prop = min(I_prop, 1)) %>%
  ggplot(aes(x = time, y = I_prop, colour = as.factor(Temperature))) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Temperature (⁰C)") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Proportion of aphids infected",
       title = "Rainfall = 1 cm; Fitness cost = 0.1")
dev.off()

png("Figures/temp_total.png", width = 600)
bind_rows(Y_temp) %>%
  ggplot(aes(x = time, y = Tot_aphids, colour = as.factor(Temperature))) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Temperature (⁰C)") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Aphid population density",
       title = "Rainfall = 1 cm; Fitness cost = 0.1")
dev.off()

png("Figures/prec_prop.png", width = 600)
bind_rows(Y_prec) %>%
  rowwise() %>%
  mutate(I_prop = I_aphids/Tot_aphids) %>%
  mutate(I_prop = case_when(is.infinite(I_prop) ~ 0,
                            TRUE ~ I_prop)) %>%
  mutate(I_prop = min(I_prop, 1)) %>%
  ggplot(aes(x = time, y = I_prop, colour = as.factor(Precipitation))) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Rainfall (cm)") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Proportion of aphids infected",
       title = "Temperature = 25⁰C; Fitness cost = 0.1")
dev.off()

png("Figures/prec_total.png", width = 600)
bind_rows(Y_prec) %>%
  ggplot(aes(x = time, y = Tot_aphids, colour = as.factor(Precipitation))) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Rainfall (cm)") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Aphid population density",
       title = "Temperature = 25⁰C; Fitness cost = 0.1")
dev.off()

png("Figures/fit_prop.png", width = 600)
bind_rows(Y_fit) %>%
  rowwise() %>%
  mutate(I_prop = I_aphids/Tot_aphids) %>%
  mutate(I_prop = case_when(is.infinite(I_prop) ~ 0,
                            TRUE ~ I_prop)) %>%
  mutate(I_prop = min(I_prop, 1)) %>%
  ggplot(aes(x = time, y = I_prop, colour = as.factor(Fitness))) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Fitness cost") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Proportion of aphids infected",
       title = "Temperature = 25⁰C; Rainfall = 1 cm")
dev.off()

png("Figures/fit_total.png", width = 600)
bind_rows(Y_fit) %>%
  ggplot(aes(x = time, y = Tot_aphids, colour = as.factor(Fitness))) +
  geom_line(size = 1) +
  scale_colour_viridis_d(name = "Fitness cost") +
  theme_bw() +
  labs(x = "Time from introduction",
       y = "Aphid population density",
       title = "Temperature = 25⁰C; Rainfall = 1 cm")
dev.off()

# plot population dynamics through time under different fitness costs
for(i in 1:6){
  png(paste("Figures/fit_", unique(Y_fit[[i]]$Fitness), "_prop.png", sep = ""), width = 600)
  print(Y_fit[[i]] %>%
          gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
          ggplot(aes(x = time, y = aphid_pop, fill = status)) +
          geom_density(stat = "identity", position = "stack") +
          labs(x = "Time from introduction",
               y = "Total number of aphids",
               title = paste("Temperature = 25⁰C; Rainfall = 1 cm; Fitness cost = ", unique(Y_fit[[i]]$Fitness), sep = "")) +
          scale_fill_manual(values = c("dark green", "darkgoldenrod"),
                            name = "Status",
                            labels = c("Infected", "Healthy")) +
          theme_bw())
  dev.off()
}

# conditions throughout 2021 in Wagga Wagga:
png("Figures/Waga_waga_2021.png", width = 600)
left_join(read_csv("Data/st_r_Data_Precipitation.csv"),
          read_csv("Data/st_r_Data_Temperature.csv")) %>%
  gather(variable, value, -t) %>%
  ggplot(aes(x = as.Date(t, origin = as.Date("2021-01-01")), y = value, colour = variable)) +
  geom_line() +
  scale_colour_manual(values = c("dark blue", "dark red"), name = "Variable") +
  theme_bw() +
  facet_wrap(vars(variable), ncol = 1, scales = "free") +
  theme(legend.position = "none") +
  xlab("Time")
dev.off()

# model aphid population in Wagga Wagga over 2021
time <- seq(1:365)
Y_init <- list(Tot_aphids = 1000,
               I_aphids = 100,
               WT_aphids = 900,
               HealthyCP = 100,
               InfectedCP = 0,
               WT_production = 0,
               I_production = 0)
params <- list(max_net_reproductive_rate = 1,
               CP_aphid_capacity = 5000,
               aphid_natural_losses = 0.1,
               fitness_cost = 0.5,
               aphid_walks = 0.025,
               aphid_flights = 0.05)

png("Figures/Wagga_Wagga_2021_model.png", width = 600)
ww_m <- model_run(time, Y_init, params, plot = T, constant = F)
dev.off()

# model null aphid population in Wagga Wagga (no infected)
params_null <- list(max_net_reproductive_rate = 1,
                    CP_aphid_capacity = 5000,
                    aphid_natural_losses = 0.1,
                    fitness_cost = 0,
                    aphid_walks = 0.025,
                    aphid_flights = 0.05)

ww_null <- model_run(time, Y_init, params_null, plot = T, constant = F)

png("Figures/Wagga_wagga_2021_prop.png", width = 600)
ww_m %>%
  gather(status, aphid_pop, c(I_aphids, WT_aphids)) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = Tot_aphids, colour = "red"), data = ww_null) +
  geom_density(aes(y = aphid_pop, fill = status), stat = "identity", position = "stack") +
  labs(x = "Date",
       y = "Total number of aphids",
       title = "Wagga Wagga 2021; Fitness cost = 0.5") +
  scale_fill_manual(values = c("dark green", "darkgoldenrod"),
                    name = "Status",
                    labels = c("Infected", "Healthy")) +
  scale_colour_manual(values = "red", labels = "Null", name = "") +
  theme_bw()
dev.off()
