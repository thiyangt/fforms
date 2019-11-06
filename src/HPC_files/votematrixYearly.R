## ---- pkg
library(randomForest)

## ---- data
load("phdproject2/rfu_m4yearly.rda")
features_M4Y <- readRDS("fforms/features_M4Y.rds")

## ---- calculations
yearlym4_votes <- predict(rfu_m4yearly, features_M4Y, type="vote")
save(yearlym4_votes, file="fforms/yearlym4_votes.rda")
