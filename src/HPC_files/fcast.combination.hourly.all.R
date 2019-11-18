## ---- pkg
library(seer)

## ---- data
load("fforms/hourlym4_votes.rda")
library(M4comp2018)
data(M4)
hourly_m4 <- Filter(function(l) l$period == "Hourly", M4)
length(hourly_m4)
## convert data into msts object
hourlym4_msts <- lapply(hourly_m4, function(temp){
  temp$x <- convert_msts(temp$x, "hourly")
  return(temp)
})

## ---- calculations
models.weights <- fforms_ensemble(hourlym4_votes, threshold = 0.6)
fcast.combination.m4hourly.all <- fforms_combinationforecast(models.weights, hourlym4_msts, "M4", 48)
saveRDS(fcast.combination.m4hourly.all, file="fforms/fcast.combination.m4hourly.all.rds")




