# Compute mstlets forecasts for weekly time series

library(seer)
library(M4comp2018)
data(M4)
weekly_M4 <- Filter(function(l) l$period == "Weekly", M4)
M4_weekly_constant_train <- sapply(weekly_M4, function(temp){
  temp$x <- ts(c(temp$x,temp$xx), frequency = 52)})


MASE_m4_weekly <- fcast_accuracy(M4_weekly_constant_train , 
                        models=c("mstlets"),
                        database="other",
                        h=13, accuracyFun=cal_medianscaled, fcast_save = TRUE,  length_out = 2)
save(accuracy_m4_weekly, file="data/M4_benchmar_Accuracy/weekly/accuracy_m4_weekly.rda")
