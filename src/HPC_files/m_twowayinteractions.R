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


stability.linearity.m <- twowayinteraction(rfu_m4monthly, stability, linearity, 
                                               fulldf = monthly_training,
                                               subsetdf = subset_monthly, grid.resolution=20, trim2=TRUE)
save(stability.linearity.m, file="fforms/stability.linearity.m.rda")

hwgamma.linearity.m <- twowayinteraction(rfu_m4monthly, hwgamma, linearity, 
                                         fulldf = monthly_training,
                                         subsetdf = subset_monthly, grid.resolution=20, trim2=TRUE)
save(hwgamma.linearity.m, file="fforms/hwgamma.linearity.m.rda")

e_acf1.linearity.m <- twowayinteraction(rfu_m4monthly, e_acf1, linearity, 
                                         fulldf = monthly_training,
                                         subsetdf = subset_monthly, grid.resolution=20, trim2=TRUE)
save(e_acf1.linearity.m, file="fforms/e_acf1.linearity.m.rda")

entropy.linearity.m <- twowayinteraction(rfu_m4monthly, entropy, linearity, 
                                        fulldf = monthly_training,
                                        subsetdf = subset_monthly, grid.resolution=20, trim2=TRUE)
save(entropy.linearity.m, file="fforms/entropy.linearity.m.rda")

hwalpha.linearity.m <- twowayinteraction(rfu_m4monthly, hwalpha, linearity, 
                                         fulldf = monthly_training,
                                         subsetdf = subset_monthly, grid.resolution=20, trim2=TRUE)
save(hwalpha.linearity.m, file="fforms/hwalpha.linearity.m.rda")


diff1y_acf5.linearity.m <- twowayinteraction(rfu_m4monthly, diff1y_acf5, linearity, 
                                         fulldf = monthly_training,
                                         subsetdf = subset_monthly, grid.resolution=20, trim2=TRUE)
save(diff1y_acf5.linearity.m, file="fforms/diff1y_acf5.linearity.m.rda")


N.linearity.m <- twowayinteraction(rfu_m4monthly, N, linearity, 
                                             fulldf = monthly_training,
                                             subsetdf = subset_monthly, grid.resolution=20, trim2=TRUE, trim1=TRUE)
save(N.linearity.m, file="fforms/N.linearity.m.rda")