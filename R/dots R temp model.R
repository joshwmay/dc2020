library(dplyr)
library(lubridate)
library(ggplot2)
library(splines)
library(caret)

# Read dat
dat <- read.csv("~/INST377/Food_Inspection_Build/dc2020/dots_data.csv")
dat <- na.omit(dat)

# Partition Data
dat_filter <- filter(dat, location == "South_Gate_South_View")
set.seed(123)
training.samples <- dat_filter$time_of_day %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat_filter[training.samples, ]
test.data <- dat_filter[-training.samples, ]

# Model Cars
knots <- quantile(train.data$time_of_day, p = c(0, 0.25, 0.5, 0.75, 1))
cmodel <- lm(cars ~ bs(time_of_day, knots = knots) + Temperature, data = train.data)
ggplot(train.data, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 5))
predictions <- cmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)
summary(cmodel)

# Model Pedestrians
tmodel <- lm(pedestrians ~ bs(time_of_day, knots = knots) + Temperature, data = train.data)
ggplot(train.data, aes(time_of_day, pedestrians), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 5))
predictions <- tmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)
summary(tmodel)
