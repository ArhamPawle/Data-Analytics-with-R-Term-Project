lap_times<- read.csv("lap_times.csv")
library(ggplot2)

time_sec <- unlist(lapply(strsplit(lap_times$time, split = ":", fixed = TRUE)
                          , function(xx){ as.numeric(xx[1])*60 + as.numeric(xx[2]) }))

lap_times$time_sec <- time_sec


lap_times$driverId_fac <- as.factor(lap_times$driverId)
lap_times_subset <- subset(lap_times, raceId == 1073 & (driverId == 1 | driverId == 830))
ggplot(lap_times_subset, aes(x = lap, y = time_sec, colour = driverId_fac)) +
  geom_line() + 
  scale_colour_manual(values = c("steelblue", "darkred") 
                      , name = "Driver", breaks = c("1", "830") 
                      , labels = c("Lewis Hamilton", "Max Verstappen")) + 
  labs(y = "Lap Time (sec)", x = "Lap") + theme(legend.position = "top")