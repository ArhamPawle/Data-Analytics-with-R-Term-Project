ver_ad_2021_lap44 <- read.csv("ham_data_lap_2021_ad_44.csv")
ham_ad_2021_lap44 <- read.csv("ver_data_lap_2021_ad_44.csv")

library(gridExtra)
library(ggplot2)


p1 <- ggplot(data = ver_ad_2021_lap44, aes(x = Distance, y = Speed, color = "Max Verstappen Lap 44")) +
  geom_line() +
  geom_line(data = ham_ad_2021_lap44, aes(x = Distance, y = Speed, color = "Lewis Hamilton Lap 44")) +
  labs(y = "Speed(km/h)", x = "Distance(in meters)") + 
  scale_color_manual(values = c("darkred", "steelblue"), 
                     name = "Max Verstappen Vs Lewis Hamilton",
                     breaks = c("Max Verstappen Lap 44", "Lewis Hamilton Lap 44"),
                     labels = c("Lap 44", "Lap 44"))

p2 <- ggplot(data = ver_ad_2021_lap44, aes(x = Distance, y = Throttle, color = "Max Verstappen Lap 44")) +
  geom_line() +
  geom_line(data = ham_ad_2021_lap44, aes(x = Distance, y = Throttle, color = "Lewis Hamilton Lap 44")) +
  labs(y = "Throttle(%)", x = "Distance(in meters)") + 
  scale_color_manual(values = c("darkred", "steelblue"), 
                     name = "Max Verstappen vs Lewis Hamilton",
                     breaks = c("Max Verstappen Lap 44", "Lewis Hamilton Lap 44"),
                     labels = c("Lap 44", "Lap 44")) +
  guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))


p3 <- ggplot(data = ver_ad_2021_lap44, aes(x = Distance, y = Brake, color = "Max Verstappen Lap 44")) +
  geom_line() +
  geom_line(data = ham_ad_2021_lap44, aes(x = Distance, y = Brake, color = "Lewis Hamilton Lap 44")) +
  labs(y = "Brake", x = "Distance(in meters)") + 
  scale_color_manual(values = c("darkred", "steelblue"), 
                     name = "Max Verstappen vs Lewis Hamilton",
                     breaks = c("Max Verstappen Lap 44", "Lewis Hamilton Lap 44"),
                     labels = c("Lap 44", "Lap 44")) +
  guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))

p4 <- ggplot(data = ver_ad_2021_lap44, aes(x = Distance, y = nGear, color = "Max Verstappen Lap 44")) +
  geom_line() +
  geom_line(data = ham_ad_2021_lap44, aes(x = Distance, y = nGear, color = "Lewis Hamilton Lap 44")) +
  labs(y = "Gear Number", x = "Distance(in meters)") + 
  scale_color_manual(values = c("darkred", "steelblue"), 
                     name = "Max Verstappen vs Lewis Hamilton",
                     breaks = c("Max Verstappen Lap 44", "Lewis Hamilton Lap 44"),
                     labels = c("Lap 44", "Lap 44")) +
  guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))

p5 <- ggplot(data = ver_ad_2021_lap44, aes(x = Distance, y = RPM, color = "Max Verstappen Lap 44")) +
  geom_line() +
  geom_line(data = ham_ad_2021_lap44, aes(x = Distance, y = RPM, color = "Lewis Hamilton Lap 44")) +
  labs(y = "RPM", x = "Distance(in meters)") + 
  scale_color_manual(values = c("darkred", "steelblue"), 
                     name = "Max Verstappen vs Lewis Hamilton",
                     breaks = c("Max Verstappen Lap 44", "Lewis Hamilton Lap 44"),
                     labels = c("Lap 44", "Lap 44")) +
  guides(color = guide_legend(override.aes = list(title = "Max Verstappen vs Lewis Hamilton")))

grid.arrange(p1, p2, p3, p4, p5 ,ncol = 1, nrow = 5)
