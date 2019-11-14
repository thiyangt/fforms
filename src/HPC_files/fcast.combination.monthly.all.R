## ---- pkg
library(seer)

## ---- data
load("fforms/monthlym4_votes.rda")
library(M4comp2018)
data(M4)
monthly_m4 <- Filter(function(l) l$period == "Monthly", M4)


## ---- calculations
models.weights <- fforms_ensemble(monthlym4_votes, threshold = 0.6)
fcast.combination.m4monthly.all <- fforms_combinationforecast(models.weights, monthly_m4, "M4", 18, parallel=TRUE)
saveRDS(fcast.combination.m4monthly.all, file="fforms/fcast.combination.m4monthly.all.rds")




