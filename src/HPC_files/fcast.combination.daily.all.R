## ---- pkg
library(seer)

## ---- data
load("fforms/dailym4_votes.rda")
library(M4comp2018)
data(M4)
daily_m4 <- Filter(function(l) l$period == "Daily", M4)
## convert data into msts object
dailym4_msts <- lapply(daily_m4, function(temp){
  temp$x <- convert_msts(temp$x, "daily")
  return(temp)
})


## ---- calculations
models.weights <- fforms_ensemble(dailym4_votes, threshold = 0.6)
fcast.combination.m4daily.all <- fforms_combinationforecast(models.weights, dailym4_msts, "M4", 14)
saveRDS(fcast.combination.m4daily.all, file="fforms/fcast.combination.m4daily.all.rds")




