## Packages
library(seer)

## Data
data(M4)

## Yearly
yearly_m4 <- subset(M4, "yearly")
length(yearly_m4) # 23000
features_M4Y<- cal_features(yearly_m4, database="M4", highfreq = FALSE, seasonal = FALSE, m = 1, lagmax = 2L)
dim(features_M4Y) # 23000 25
summary(features_M4Y)
saveRDS(features_M4Y, file="data/HPCfiles/features_M4Y.rds")


## Quarterly
quarterly_m4 <- subset(M4, "quarterly")
length(quarterly_m4) # 24000
features_M4Q<- cal_features(quarterly_m4, seasonal=TRUE, m=4,lagmax=5L, 
                            database="M4", highfreq = FALSE)
dim(features_M4Q) # 24000 30
summary(features_M4Q)
save(features_M4Q, file="data/HPCfiles/features_M4Q.rda")


## Monthly
monthly_m4 <- subset(M4, "monthly")
length(monthly_m4) # 48000
features_M4M<- cal_features(monthly_m4, seasonal=TRUE, m=12,lagmax=13L, 
                            database="M4", highfreq = FALSE)
dim(features_M4M) # 48000 30
summary(features_M4M)
saveRDS(features_M4M, file="data/HPCfiles/features_M4M.rda")

## Weekly
weekly_m4 <- subset(M4, "weekly")
length(weekly_m4) # 359
features_M4W <- cal_features(weekly_m4, seasonal=TRUE, m=52, lagmax=53L, database="M4", highfreq = FALSE)
dim(features_M4W) # 359 30
features_M4W$sediff_seacf1[is.na(features_M4W$sediff_seacf1)] <- 0 
summary(features_M4W)
save(features_M4W, file="data/HPCfiles/features_M4W.rda")

## Daily
daily_m4 <- subset(M4, "daily")
length(daily_m4) # 4227
## convert data into msts object
dailym4_msts <- lapply(daily_m4, function(temp){
  temp$x <- convert_msts(temp$x, "daily")
  return(temp)
})
features_M4D <- cal_features(dailym4_msts, seasonal=TRUE, m=7,lagmax=8L, 
                             database="M4", highfreq = TRUE)
dim(features_M4D) # 4227 26
summary(features_M4D)
saveRDS(features_M4D, file="data/HPCfiles/features_M4D.rds")

 ## Hourly
hourly_m4 <- subset(M4, "hourly")
length(hourly_m4)
## convert data into msts object
hourlym4_msts <- lapply(hourly_m4, function(temp){
  temp$x <- convert_msts(temp$x, "hourly")
  return(temp)
})
features_M4H <- cal_features(hourlym4_msts, seasonal=TRUE, m=24,lagmax=25L, 
                             database="M4", highfreq = TRUE)
dim(features_M4H) # 414 26
summary(features_M4H)
saveRDS(features_M4H, file="data/HPCfiles/features_M4H.rds")

