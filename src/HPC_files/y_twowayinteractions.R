####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid
#Predictions based on different variables
## load random forest
load("phdproject2/yearly_training.rda") # training data
load("phdproject2/rfu_m4yearly.rda") # random forest model
load("phdproject2/subset_yearly.rda")
subset_yearly <- subset_yearly[,1:25]


y_pacf5.linearityrmout.y <- twowayinteraction(rfu_m4yearly, y_pacf5, linearity, 
                                            fulldf = yearly_training,
                                            subsetdf = subset_yearly, grid.resolution=100, trim2=TRUE)
save(y_pacf5.linearityrmout.y, file="fforms/y_pacf5.linearityrmout.y.rda")


stability.linearityrmout.y <- twowayinteraction(rfu_m4yearly, stability, linearity, 
                                              fulldf = yearly_training,
                                              subsetdf = subset_yearly, grid.resolution=100, trim2=TRUE)
save(stability.linearityrmout.y, file="fforms/stability.linearityrmout.y.rda")

curvature.linearityrmout.y <- twowayinteraction(rfu_m4yearly, curvature, linearity, 
                                                fulldf = yearly_training,
                                                subsetdf = subset_yearly, grid.resolution=100, trim1=TRUE,trim2=TRUE)
save(curvature.linearityrmout.y, file="fforms/curvature.linearityrmout.y.rda")

y_acf1.linearityrmout.y <- twowayinteraction(rfu_m4yearly, y_acf1, linearity, 
                                                fulldf = yearly_training,
                                                subsetdf = subset_yearly, grid.resolution=100, trim2=TRUE)
save(y_acf1.linearityrmout.y, file="fforms/y_acf1.linearityrmout.y.rda")


diff1y_acf1.linearityrmout.y <- twowayinteraction(rfu_m4yearly, diff1y_acf1, linearity, 
                                             fulldf = yearly_training,
                                             subsetdf = subset_yearly, grid.resolution=100, trim2=TRUE)
save(diff1y_acf1.linearityrmout.y, file="fforms/diff1y_acf1.linearityrmout.y.rda")

lmres_acf1.linearityrmout.y <- twowayinteraction(rfu_m4yearly, lmres_acf1, linearity, 
                                                  fulldf = yearly_training,
                                                  subsetdf = subset_yearly, grid.resolution=100, trim2=TRUE)
save(lmres_acf1.linearityrmout.y, file="fforms/lmres_acf1.linearityrmout.y.rda")

