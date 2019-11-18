## ---- pkg
library(randomForest)

## ---- data
load("phdproject2/rfu_m4daily.rda")
features_M4D <- readRDS("fforms/features_M4D.rds")

## ---- calculations
dailym4_votes <- predict(rfu_m4daily, features_M4D, type="vote")
save(dailym4_votes, file="fforms/dailym4_votes.rda")
