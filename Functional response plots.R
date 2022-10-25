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