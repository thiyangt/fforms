## ---- pkg
library(randomForest)

## ---- data
load("phdproject2/rfu_m4monthly.rda")
features_M4M <- readRDS("fforms/features_M4M.rds")

## ---- calculations
monthlym4_votes <- predict(rfu_m4monthly, features_M4M, type="vote")
saveRDS(monthlym4_votes, file="fforms/monthlym4_votes.rds")
