## ---- pkg
library(randomForest)

## ---- data
load("phdproject2/rfu_m4weekly.rda")
load("fforms/features_M4W.rda")

## ---- calculations
weeklym4_votes <- predict(rfu_m4weekly, features_M4W, type="vote")
save(weeklym4_votes, file="fforms/weeklym4_votes.rda")
