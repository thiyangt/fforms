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


stability.N.m <- twowayinteraction(rfu_m4monthly, stability, N, 
                                             fulldf = monthly_training,
                                             subsetdf = subset_monthly, grid.resolution=20, trim1=TRUE, trim2=TRUE)
save(stability.N.m, file="fforms/stability.N.m.rda")

stability.trend.m <- twowayinteraction(rfu_m4monthly, stability, trend, 
                                             fulldf = monthly_training,
                                             subsetdf = subset_monthly, grid.resolution=20, trim1=TRUE, trim2=TRUE)
save(stability.trend.m, file="fforms/stability.trend.m.rda")
