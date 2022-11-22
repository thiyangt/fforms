## Packages
library(tidyverse)
library(Mcomp)
##install.packages("https://github.com/carlanetto/M4comp2018/releases/download/0.2.0/M4comp2018_0.2.0.tar.gz",
##                repos=NULL)
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

class(length_Y) # dataframe
summary(length_Y[,1])
length(length_Y[, 1])

summary(length_Q[, 1])
length(length_Q[, 1])

summary(length_M[, 1])
length(length_M[, 1])

summary(length_D[,1])
length(length_D[,1])

summary(length_H[, 1])
length(length_H[, 1])
## Training data length (M1+M3+Simulated)

## Yearly
yt <- load("data/yearly/yearly_training.rda")
length_y_train <- yearly_training$N
summary(length_y_train)
length(length_y_train)
## Quarterly
qt <- load("data/quarterly/quarterly_training.rda")
length_q_train <- quarterly_training$N
summary(length_q_train)
length(length_q_train)
## Monthly
mt <- load("data/monthly/monthly_training.rda")
length_m_train <- monthly_training$N
summary(length_m_train)
length(length_m_train)
## Daily
dt <- load("data/daily/daily_training.rda")
length_d_train <- daily_training$N
summary(length_q_train)
length(length_d_train)
## Hourly
ht <- load("data/hourly/hourly_training.rda")
length_h_train <- hourly_training$N
summary(length_h_train)
length(length_h_train)


