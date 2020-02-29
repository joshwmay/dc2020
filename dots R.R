library(dplyr)
library(lubridate)
library(ggplot2)

dat <- read.csv("~/INST377/Food_Inspection_Build/dc2020/dots_data.csv")
dat2 <- mutate(dat, total = pedestrians + bicyclists + cars + buses + trucks)
write.csv(dat2, "dots_data_total.csv")

dat <- read.csv("~/INST377/Food_Inspection_Build/dc2020/dots_data_time.csv")
dat$time_of_day <- dat$hours + dat$minutes/60
dat$time_round <- round(strptime(dat$time,format = "%Y-%m-%dT%H:%M:%OSZ"), unit="hour")
dat$time_round <- as.POSIXct(dat$time_round)

dat$weekday <- weekdays(as.Date(dat$time_round))

dat_weather <- read.csv("~/INST377/Food_Inspection_Build/dc2020/weather_data_csv.csv")
dat_weather$time <- strptime(dat_weather$time,format = "%Y-%m-%dT%H:%M:%OSZ")
dat_weather$time_round <- as.POSIXct(dat_weather$time)

dat_combined <- left_join(dat, dat_weather, by = "time_round")
write.csv(dat_combined, "dots_data_combined.csv")


# Add weekday
dat <- read.csv("~/INST377/Food_Inspection_Build/dc2020/dots_data.csv")
dat$weekday <- weekdays(as.Date(dat$time))
