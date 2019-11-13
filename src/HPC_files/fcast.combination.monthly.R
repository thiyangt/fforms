## ---- pkg
library(seer)

## ---- data
load("fforms/quarterlym4_votes.rda")
library(M4comp2018)
data(M4)
monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
models.weights <- fforms_ensemble(quarterlym4_votes, threshold = 0.6)

## ---- read command line argument
args <- commandArgs(TRUE)
aa <- as.numeric(args[1])
start <- (aa)*2400+1
end <- start+2399

datats <- monthly_M4[start:end]
dataweights <- models.weights[start:end]

## ---- calculations
name1 <- paste0("M4Mcombination_",args[1])
name1 <- fforms_combinationforecast(dataweights, datats, "M4", 8, parallel=TRUE)
name<-paste0("fforms/M4monthly","_",args[1],".rds")
saveRDS(name1, file=name)





