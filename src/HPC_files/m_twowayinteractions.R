####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid

#Predictions based on different variables
## load random forest
load("phdproject2/monthly_training.rda") # training data
load("phdproject2/rfu_m4monthly.rda") # random forest model
load("phdproject2/subset_monthly.rda")

subset_monthly <- subset_monthly[,1:30]


y_pacf5.linearity.m <- twowayinteraction(rfu_m4monthly, y_pacf5, linearity, 
                                         fulldf = monthly_training,
                                         subsetdf = subset_monthly, grid.resolution=100, trim2=TRUE)
save(y_pacf5.linearity.m, file="fforms/y_pacf5.linearity.m.rda")

sediff_seacf1.linearity.m <- twowayinteraction(rfu_m4monthly, sediff_seacf1, linearity, 
                                         fulldf = monthly_training,
                                         subsetdf = subset_monthly, grid.resolution=100, trim2=TRUE)
save(sediff_seacf1.linearity.m, file="fforms/sediff_seacf1.linearity.m.rda")