## ---- pkg
library(seer)

## ---- data
load("fforms/weeklym4_votes.rda")
library(M4comp2018)
data(M4)
weekly_m4 <- Filter(function(l) l$period == "Weekly", M4)


weekly_list <- list(359)
for (i in 1: 359){
  y <- weekly_m4
  weekly_list[[i]] <- list(x=ts(weekly_m4[[1]]$x, frequency=52))
}

## ---- calculations
models.weights <- fforms_ensemble(weeklym4_votes, threshold = 0.6)
fcast.combination.m4weekly.all <- fforms_combinationforecast(models.weights, weekly_list, "M4", 13)
saveRDS(fcast.combination.m4weekly.all, file="fforms/fcast.combination.m4weekly.all.rds")




