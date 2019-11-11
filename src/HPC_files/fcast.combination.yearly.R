## ---- pkg
library(seer)

## ---- data
load("fforms/yearlym4_votes.rda")
library(M4comp2018)
data(M4)
yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)


## ---- calculations
models.weights <- fforms_ensemble(yearlym4_votes, threshold = 0.6)
fcast.combination.yearlym4 <- fforms_combinationforecast(models.weights, yearly_m4, "M4", 6)
save(fcast.combination.m4yearly, file="fforms/fcast.combination.m4yearly.rda")




