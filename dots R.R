library(dplyr)
dat <- read.csv("~/INST377/Food_Inspection_Build/dc2020/dots_data.csv")
dat2 <- mutate(dat, total = pedestrians + bicyclists + cars + buses + trucks)
write.csv(dat2, "dots_data_total.csv")
