library(dplyr)
library(lubridate)
library(ggplot2)
library(splines)
library(caret)

# Data
dat <- read.csv("~/INST377/Food_Inspection_Build/dc2020/dots_data.csv")
dat <- na.omit(dat)
dat$weekday <- weekdays(as.Date(dat$time))
knots <- c(0,2,4,6,8,10,12,14,16,18,20,22,24)

## South Gate South
dat_filter <- filter(dat, location == "South_Gate_South_View")
dat_filter <- filter(dat_filter, cars != 0)

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dat2 <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dat2 <- bind_cols(dat2, data.frame(predictions))
  
# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

## South Gate Nothern View
dat_filter <- filter(dat, location == "South_Gate_Northern_View")
dat_filter <- filter(dat_filter, cars != 0)

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


## Stadium_Dr_East_Of_Regents_Dr
dat_filter <- filter(dat, location == "Stadium_Dr_East_Of_Regents_Dr")

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


## University_Blvd_&_Paint_Branch_Dr
dat_filter <- filter(dat, location == "University_Blvd_&_Paint_Branch_Dr")

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


## Campus_Dr_&_Presidential_Dr
dat_filter <- filter(dat, location == "Campus_Dr_&_Presidential_Dr")

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


## Campus_Dr_At_Paint_Branch_Dr
dat_filter <- filter(dat, location == "Campus_Dr_At_Paint_Branch_Dr")
dat_filter <- filter(dat_filter, cars!= 0)
                  
# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


## Paint_Branch_Dr_At_Campus_Dr
dat_filter <- filter(dat, location == "Paint_Branch_Dr_At_Campus_Dr")
dat_filter <- filter(dat_filter, cars != 0)

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


## Regents_Dr_&_Stadium_Dr_1
dat_filter <- filter(dat, location == "Regents_Dr_&_Stadium_Dr_1")
dat_filter <- filter(dat_filter, cars != 0)

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


## Regents_Dr_&_Stadium_Dr_2
dat_filter <- filter(dat, location == "Regents_Dr_&_Stadium_Dr_2")
dat_filter <- filter(dat_filter, cars != 0)

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


## Regents_Dr_&_Stadium_Dr_3
dat_filter <- filter(dat, location == "Regents_Dr_&_Stadium_Dr_3")
dat_filter <- filter(dat_filter, cars != 0)

# Monday
dat_filter2 <- filter(dat_filter, weekday == "Monday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Tuesday
dat_filter2 <- filter(dat_filter, weekday == "Tuesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Wednesday
dat_filter2 <- filter(dat_filter, weekday == "Wednesday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Thursday
dat_filter2 <- filter(dat_filter, weekday == "Thursday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Friday
dat_filter2 <- filter(dat_filter, weekday == "Friday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Saturday
dat_filter2 <- filter(dat_filter, weekday == "Saturday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)

# Sunday
dat_filter2 <- filter(dat_filter, weekday == "Sunday")
dattemp <- select(dat_filter2, time, location)
tmodel <- lm(cars ~ bs(time_of_day, knots = knots) , data = dat_filter2)
summary(tmodel)
predictions <- tmodel %>% predict(dat_filter2)
data.frame(
  RMSE = RMSE(predictions, dat_filter2$cars),
  R2 = R2(predictions, dat_filter2$cars)
)
predictions[predictions<0] <- 0
dattemp <- bind_cols(dattemp, data.frame(predictions))
dat2 <- bind_rows(dat2, dattemp)


# Join
dat <- left_join(dat, dat2, by= c("time", "location"))
write.csv(dat, "dots_data_pred.csv")

