## Packages
library(tidyverse)
library(Mcomp)
##remotes::install_github("carlanetto/M4comp2018", ref = "6e75d59eb30e47cbb6bd5093d5bf2515493a6050")

library(M4comp2018)

## ----- length calculations ----
## M4 competition
data(M4)
yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)
quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)
monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
weekly_M4 <- Filter(function(l) l$period == "Weekly", M4)
daily_M4 <- Filter(function(l) l$period == "Daily", M4)
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)

length_Y <- data.frame(length=sapply(yearly_M4, function(temp){length(temp$x)}))
length_Q <- data.frame(length=sapply(quarterly_M4, function(temp){length(temp$x)}))
length_M <- data.frame(length=sapply(monthly_M4, function(temp){length(temp$x)}))
length_W <- data.frame(length=sapply(weekly_M4, function(temp){length(temp$x)}))
length_D <- data.frame(length=sapply(daily_M4, function(temp){length(temp$x)}))
length_H <- sapply(hourly_M4, function(temp){length(temp$x)})


## Training data length (M1+M3+Simulated)

## Yearly
yt <- load("data/yearly/yearly_training.rda")
length_y_train <- yearly_training$N
summary(length_y_train)
length(length_y_train)
sd(length_y_train)

## Quarterly
qt <- load("data/quarterly/quarterly_training.rda")
length_q_train <- quarterly_training$N
summary(length_q_train)
length(length_q_train)
sd(length_q_train)

## Monthly
mt <- load("data/monthly/monthly_training.rda")
length_m_train <- monthly_training$N
summary(length_m_train)
length(length_m_train)
sd(length_m_train)

## Weekly
wt <- load("data/weekly/weekly_training.rda")
length_w_train <- weekly_training$N
summary(length_w_train)
length(length_w_train)
sd(length_w_train)

## Daily
dt <- load("data/daily/daily_training.rda")
length_d_train <- daily_training$N
summary(length_q_train)
length(length_d_train)
sd(length_d_train)

## Hourly
ht <- load("data/hourly/hourly_training.rda")
length_h_train <- hourly_training$N
summary(length_h_train)
length(length_h_train)
sd(length_h_train)


## Relationship between accuracy and length
## MASE calculation code
## src -> forecast_evaluation.R
load("data/MASE_and_length/MASE_yearly.rda")
load("data/MASE_and_length/MASE_quarterly.rda")
load("data/MASE_and_length/MASE_monthly.rda")
load("data/MASE_and_length/MASE_weekly.rda")
load("data/MASE_and_length/MASE_daily.rda")
load("data/MASE_and_length/MASE_hourly.rda")

length(MASE_yearly) #23000
length(MASE_quarterly) #24000
length(MASE_monthly) #48000
length(MASE_weekly) #359
length(MASE_daily) #4227
length(MASE_hourly) #414

category <- c(rep("Yearly", 23000),
              rep("Quarterly", 24000),
              rep("Monthly", 48000),
              rep("Weekly", 359),
              rep("Daily", 4227),
              rep("Hourly", 414))
category <- factor(category, levels = c("Yearly",
      "Quarterly", "Monthly", "Weekly", "Daily", "Hourly"))
MASE <- c(MASE_yearly,
          MASE_quarterly,
          MASE_monthly,
          MASE_weekly,
          MASE_daily,
          MASE_hourly)

length_y_test <- data.frame(length=sapply(yearly_M4, function(temp){length(temp$x)}))
length_q_test <- data.frame(length=sapply(quarterly_M4, function(temp){length(temp$x)}))
length_m_test <- data.frame(length=sapply(monthly_M4, function(temp){length(temp$x)}))
length_w_test <- data.frame(length=sapply(weekly_M4, function(temp){length(temp$x)}))
length_d_test <- data.frame(length=sapply(daily_M4, function(temp){length(temp$x)}))
length_h_test <- sapply(hourly_M4, function(temp){length(temp$x)})


length(length_y_test[, 1]) #23000
length(length_q_test[, 1]) #24000
length(length_m_test[, 1]) #48000
length(length_w_test[, 1]) #359
length(length_d_test[, 1]) #4227
length(length_h_test)      #414


length_test <- c(length_y_test[, 1],
            length_q_test[, 1],
            length_m_test[, 1],
            length_w_test[, 1],
            length_d_test[, 1],
            length_h_test)
length(length_test)
length(MASE)
length(category)

accuracy_length_error <- data.frame(
  length=length_test, MASE=MASE, category=category
)

save(accuracy_length_error, file="data/accuracy_length_error.rda")

## visualization: The accuracy between the relationship and length
library(tidyverse)
ggplot(data=accuracy_length_error,
       aes(x=length, y=MASE, col=category)) + geom_point(alpha=0.5) + 
  facet_wrap(vars(category), scales = "free") + xlab("Length-training period")
