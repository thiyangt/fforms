## Run on my laptop
## ---- pkg
library(randomForest)
library(seer)

## ---- data
data("hourly_fforms")
features_M4H <- readRDS("~/PhD_journey/fforms/data/HPCfiles/features_M4H.rds")

## ---- calculations
hourlym4_votes <- predict(hourly_fforms, features_M4H, type="vote")
save(hourlym4_votes, file="data/HPCfiles/hourlym4_votes.rda")
