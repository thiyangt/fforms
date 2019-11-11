## ---- pkg
library(randomForest)

## ---- data
load("phdproject2/rfu_m4quarterly.rda")
features_M4Q <- readRDS("fforms/features_M4Q.rds")

## ---- calculations
quarterlym4_votes <- predict(rfu_m4quarterly, features_M4Q, type="vote")
saveRDS(quarterlym4_votes, file="fforms/quarterlym4_votes.rds")
