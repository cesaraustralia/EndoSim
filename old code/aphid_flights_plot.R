library(tidyverse)
library(sf)
library(rnaturalearth)

dat <- read_csv("Data/aphid_flights.csv")

dat %>%
  mutate(State = sapply(str_split(dat$Site, ", "), "[[", 2)) %>%
  pivot_longer(cols = 2:13, names_to = "Month") %>%
  mutate(Month = factor(Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))) %>%
  ggplot(aes(x = as.numeric(Month), y = value, group = Site)) +
  geom_line(color = "red") +
  scale_x_continuous(breaks = c(1:12),
                     labels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"),
                     name = "Month") +
  ylab("No. of aphids") +
  theme_minimal() +
  facet_wrap(~Site, scale = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

oz <- ne_countries(scale = "large", country = "Australia", returnclass = "sf")

oz %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = dat %>% st_as_sf(coords = c("Long", "Lat"), crs = "WGS84")) +
  theme_bw()
