## ---- pkg
library(seer)

## ---- data
load("fforms/quarterlym4_votes.rda")
library(M4comp2018)
data(M4)
quarterly_m4 <- Filter(function(l) l$period == "Quarterly", M4)


## ---- calculations
models.weights <- fforms_ensemble(quarterlym4_votes, threshold = 0.6)
fcast.combination.m4quarterly <- fforms_combinationforecast(models.weights, quarterly_m4, "M4", 8, parallel=TRUE)
saveRDS(fcast.combination.m4quarterly, file="fforms/fcast.combination.m4quarterly.rds")




