library(tidyverse)
library(lemon)

shift_legend2 <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  reposition_legend(p, 'center', panel=names)
}

png("Figures/At.png")
plot(0:50, sapply(0:50, function(x) aphid_growth(x)), type = 'l', ylab = 'At', xlab = "Temperature (⁰C)", main = "Aphid increase rate")
dev.off()

png("Figures/Wl.png")
plot(0:50, sapply(0:50, function(x) aphid_loss(x)), type = 'l', ylab = 'Wl', xlab = "Rainfall (cm)", main = "Aphid losses due to rainfall")
dev.off()

png("Figures/As.png")
plot(0:50, sapply(0:50, function(x) aphid_suscept(x)), type = 'l', ylab = 'As', xlab = "Temperature (⁰C)", main = "Aphid tendency to become infected")
dev.off()

png("Figures/An.png")
plot(0:50, sapply(0:50, function(x) transmission_efficiency(x)), type = 'l', ylab = 'An', xlab = "Temperature (⁰C)", main = "Aphid transmission efficiency")
dev.off()

png("Figures/App.png")
plot(seq(0, 1, by = 0.1), sapply(seq(0, 1, by = 0.1), function(x) alate_production(x)), type = 'l', ylab = 'App', xlab = "Af (fraction of carrying capacity)", main = "Alate proportion of newly produced aphids")
dev.off()

png("Figures/functional_response.png", res = 300, height = 3000, width = 2000)
shift_legend2(tibble(Temperature = seq(0, 50,length.out = 501),
                     At = sapply(seq(0, 50,length.out = 501), function(x) aphid_growth(x)),
                     At_inf = sapply(seq(0, 50,length.out = 501), function(x) aphid_growth(x))*0.5,
                     As = sapply(seq(0, 50,length.out = 501), function(x) aphid_suscept(x)),
                     An = sapply(seq(0, 50,length.out = 501), function(x) transmission_efficiency(x)),
                     Wl = sapply(seq(0, 50,length.out = 501), function(x) aphid_loss(x)),
                     App = sapply(seq(0, 50,length.out = 501), function(x) alate_production(x))) %>%
                gather(variable, value, -Temperature) %>%
                mutate(variable2 = case_when(str_detect(variable, "inf") ~ "At",
                                             T ~ variable),
                       value = case_when(!((variable == "Wl" & Temperature > 10)|(variable == "App" & Temperature > 1)) ~ value)) %>%
                drop_na() %>%
                ggplot(aes(x = Temperature, y = value)) +
                geom_line(aes(colour = variable), size = 1) +
                theme_bw() +
                scale_colour_manual(values = c("purple4", "green4", "darkred", "orange3", "salmon", "blue2"),
                                    name = "Modelled parameter",
                                    labels = c("Aphid transmission efficiency",
                                               "Alate proportion of newly produced aphids",
                                               "Aphid tendency to become infected",
                                               "Aphid increase rate (healthy)",
                                               "Aphid increase rate (infected)",
                                               "Aphid losses due to rainfall")) +
                xlab("Input") +
                ylab("Output") +
                facet_wrap(~variable2, ncol = 2, scales = "free", strip.position = "bottom",
                           labeller = labeller(variable2 = set_names(c("Temperature (⁰C)", "Fraction of carrying capacity", "Temperature (⁰C)", "Temperature (⁰C)", "Rainfall (cm)"),
                                                                     c("An", "App", "As", "At", "Wl")))) +
                theme(strip.background = element_blank(),
                      strip.placement = "outside"))
dev.off()
