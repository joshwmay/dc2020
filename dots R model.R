library(dplyr)
library(lubridate)
library(ggplot2)
library(splines)
library(caret)

dat <- read.csv("~/INST377/Food_Inspection_Build/dc2020/dots_data.csv")
m <- lm(cars ~ time_of_day + location,data = dat_combined)
dat_filter <- filter(dat, location == "South_Gate_South_View")

knots <- quantile(dat_filter$time_of_day, p = c(0.25, 0.5, 0.75))
model <- lm(cars ~ bs(time_of_day, knots = knots), data = dat_filter)
ggplot(dat_filter, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 15))

set.seed(123)
training.samples <- dat_filter$time_of_day %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat_filter[training.samples, ]
test.data <- dat_filter[-training.samples, ]


knots <- quantile(train.data$time_of_day, p = c(0.4, 0.5, 0.6, 0.75))
knots <- smooth$knots
tmodel <- lm(cars ~ bs(time_of_day, knots = knots), data = train.data)
ggplot(train.data, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))
predictions <- tmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)

library(mgcv)
model <- gam(cars ~ s(time_of_day, k=30, bs="ps"), data = train.data)
predictions <- model %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)
ggplot(train.data, aes(time_of_day, cars) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
smooth <- model$smooth[[1]]
smooth$knots
