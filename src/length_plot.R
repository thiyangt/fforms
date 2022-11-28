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
## Quarterly
qt <- load("data/quarterly/quarterly_training.rda")
length_q_train <- quarterly_training$N
## Monthly
mt <- load("data/monthly/monthly_training.rda")
length_m_train <- monthly_training$N
## Weekly
wt <- load("data/weekly/weekly_training.rda")
length_w_train <- weekly_training$N
## Daily
dt <- load("data/daily/daily_training.rda")
length_d_train <- daily_training$N
## Hourly
ht <- load("data/hourly/hourly_training.rda")
length_h_train <- hourly_training$N

length(length_y_train) #482813
length(length_q_train) # 263957
length(length_m_train) # 289042
length(length_w_train) # 3849
length(length_d_train) # 45784
length(length_h_train) # 4490


length_y_testm4 <- data.frame(length=sapply(yearly_M4, function(temp){length(temp$x)}))
length_q_testm4 <- data.frame(length=sapply(quarterly_M4, function(temp){length(temp$x)}))
length_m_testm4 <- data.frame(length=sapply(monthly_M4, function(temp){length(temp$x)}))
length_w_testm4 <- data.frame(length=sapply(weekly_M4, function(temp){length(temp$x)}))
length_d_testm4 <- data.frame(length=sapply(daily_M4, function(temp){length(temp$x)}))
length_h_testm4 <- sapply(hourly_M4, function(temp){length(temp$x)})


length(length_y_testm4[, 1]) #23000
length(length_q_testm4[, 1]) #24000
length(length_m_testm4[, 1]) #48000
length(length_w_testm4[, 1]) #359
length(length_d_testm4[, 1]) #4227
length(length_h_testm4)      #414


yearly <- c(length_y_train, length_y_testm4[, 1])
quarterly <- c(length_q_train, length_q_testm4[, 1])
monthly <- c(length_m_train, length_m_testm4[, 1])
weekly <- c(length_w_train, length_w_testm4[, 1])
daily <- c(length_d_train, length_d_testm4[, 1])
hourly <- c(length_h_train, length_h_testm4)

length(length_y_train) + length(length_y_testm4[, 1])
length(length_q_train) + length(length_q_testm4[, 1])
length(length_m_train) + length(length_m_testm4[, 1])
length(length_w_train) + length(length_w_testm4[, 1])
length(length_d_train) + length(length_d_testm4[, 1])
length(length_h_train) + length(length_h_testm4)



N <- c(yearly, quarterly, monthly, weekly, daily, hourly)
length(N) # 1189935
frequency <- c(rep("yearly", 505813),
               rep("quarterly", 287957),
               rep("monthly", 337042),
               rep("weekly", 4208),
               rep("daily", 50011),
               rep("hourly", 4904))
frequency <- factor(frequency, levels = c("yearly",
                                    "quarterly",
                                    "monthly",
                                    "weekly",
                                    "daily",
                                    "hourly"))

length(frequency) #1189935
#period <- c(rep("training", 482813), rep("test", 23000),
#            rep("training", 263957), rep("test", 24000),
#            rep("training", 289042), rep("test", 48000),
#            rep("training", 3849), rep("test", 359),
#            rep("training", 45784), rep("test", 4227),
#            rep("training", 4490), rep("test", 414))


collection <- c(rep("reference set", 482813), rep("new series", 23000),
            rep("reference set", 263957), rep("new series", 24000),
            rep("reference set", 289042), rep("new series", 48000),
            rep("reference set", 3849), rep("new series", 359),
            rep("reference set", 45784), rep("new series", 4227),
            rep("reference set", 4490), rep("new series", 414))
length(collection) #1189935
collection <- factor(collection, levels = c("reference set", "new series"))

lengthdis <- data.frame(N=N,
                        frequency=frequency,
                        collection = collection)
save(lengthdis, file="data/lengthdis.rda")
head(lengthdis)
library(tidyverse)
library(lvplot)
cols <- c("new series" = "red", "reference set" = "blue")
ggplot(lengthdis, aes(x=collection, y=N)) +
  geom_violin(aes(fill = collection), trim = FALSE) +
  #geom_boxplot(width = 0.1) + 
  facet_wrap(vars(frequency), scales = "free") +
  scale_fill_manual(values = cols)
