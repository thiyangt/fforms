## ---- pkg
library(randomForest)

## ---- data
load("phdproject2/rfu_m4quarterly.rda")
load("fforms/features_M4Q.rda")

## ---- calculations
quarterlym4_votes <- predict(rfu_m4quarterly, features_M4Q, type="vote")
save(quarterlym4_votes, file="fforms/quarterlym4_votes.rda")
