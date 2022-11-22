## Packages
library(tidyverse)
library(Mcomp)
##install.packages("https://github.com/carlanetto/M4comp2018/releases/download/0.2.0/M4comp2018_0.2.0.tar.gz",
##                repos=NULL)
##remotes::install_github("carlanetto/M4comp2018", ref = "6e75d59eb30e47cbb6bd5093d5bf2515493a6050")

library(M4comp2018)

## ----- length calculations ----
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
length_H <- sapply(hourly_M4, function(temp){length(temp$x))})