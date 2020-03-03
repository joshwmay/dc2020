library(dplyr)
library(lubridate)
library(ggplot2)
library(splines)
library(caret)

# Read dat
dat <- read.csv("~/INST377/Food_Inspection_Build/dc2020/dots_data.csv")
dat <- na.omit(dat)

# Whole Model (Time + T + Location)
set.seed(123)
training.samples <- dat$time_of_day %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

knots <- quantile(train.data$time_of_day, p = c(0, 0.2, 0.4, 0.5, 0.6, 0.75, 0.9, 1))
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) + location + Temperature + weekday, data = train.data)
ggplot(train.data, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))
predictions <- tmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)
summary(tmodel)

dat_filter <- filter(dat, location == "South_Gate_South_View")

# Model on whole data
knots <- quantile(dat_filter$time_of_day, p = c(0.25, 0.5, 0.75))
model <- lm(cars ~ bs(time_of_day, knots = knots), data = dat_filter)
ggplot(dat_filter, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 10))

# Split data
set.seed(123)
training.samples <- dat_filter$time_of_day %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat_filter[training.samples, ]
test.data <- dat_filter[-training.samples, ]

# Model with split data
knots <- quantile(train.data$time_of_day, p = c(0.4, 0.5, 0.6, 0.75))
tmodel <- lm(cars ~ bs(time_of_day, knots = knots), data = train.data)
ggplot(train.data, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 4))
predictions <- tmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)

# GAM Model
library(mgcv)
model <- gam(cars ~ s(time_of_day, k=20, bs="ps"), data = train.data)
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

# Model with GAM knots
knots <- smooth$knots
tmodel <- lm(cars ~ bs(time_of_day, knots = knots), data = train.data)
ggplot(train.data, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 12))
predictions <- tmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)

# Filter out 0s
dat_filter2 <- filter(dat_filter, cars != 0)
set.seed(123)
training.samples <- dat_filter2$time_of_day %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat_filter2[training.samples, ]
test.data <- dat_filter2[-training.samples, ]

# Model without 0s
knots <- smooth$knots
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) + , data = train.data)
ggplot(train.data, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 12))
predictions <- tmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)

# Expanded Mode without 0s
knots <- smooth$knots
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) + Temperature, data = train.data)
ggplot(train.data, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 12))
predictions <- tmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)
predictions <- tmodel %>% predict(train.data)
data.frame(
  RMSE = RMSE(predictions, train.data$cars),
  R2 = R2(predictions, train.data$cars)
)

summary(tmodel)

# Weekdays
dat_filter <- filter(dat, location == "South_Gate_South_View")
dat_filter <- filter(dat_filter, cars != 0)
dat_filter <- dat
dat_week <- filter(dat_filter, weekday == "Monday" | weekday == "Tuesday" | weekday == "Wednesday" | weekday == "Thursday" | weekday == "Friday")
set.seed(123)
training.samples <- dat_filter$time_of_day %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat_filter[training.samples, ]
test.data <- dat_filter[-training.samples, ]

#GAM Model
model <- gam(cars ~ s(time_of_day, k=20, bs="ps") + Temperature, data = train.data)
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
summary(model)

# Expanded Model without 0s
knots <- smooth$knots
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) + Temperature, data = train.data)
ggplot(train.data, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 12))
predictions <- tmodel %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$cars),
  R2 = R2(predictions, test.data$cars)
)
predictions <- tmodel %>% predict(train.data)
data.frame(
  RMSE = RMSE(predictions, train.data$cars),
  R2 = R2(predictions, train.data$cars)
)

# Monday Model
dat_filter <- filter(dat, location == "South_Gate_South_View")
dat_filter <- filter(dat_filter, cars != 0)
dat_M <- filter(dat_filter, weekday == "Monday")

model <- gam(cars ~ s(time_of_day, k=20, bs="ps") + Temperature + location + weekday, data = dat_filter)
ggplot(dat_M, aes(time_of_day, cars) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
smooth <- model$smooth[[1]]
smooth$knots
summary(model)

# Expanded Model without 0s
knots <- smooth$knots
tmodel <- lm(cars ~ bs(time_of_day, knots = c(0,2,4,6,8,10,12,14,16,18,20,22,24)) + Temperature + location + weekday, data = dat_filter)
ggplot(dat_filter, aes(time_of_day, cars), time_of_day) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 12))
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter)
data.frame(
  RMSE = RMSE(predictions, dat_filter$cars),
  R2 = R2(predictions, dat_filter$cars)
)
dat_M$predictions <- predictions

dat$predcars <- predictions
write.csv(dat, "dots_data_pred.csv")
