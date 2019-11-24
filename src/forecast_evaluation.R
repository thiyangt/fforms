## Yearly series
## Forecasts
load("data/yearly/fcast.combination.m4yearly.rda")

## Accuracy calculations
library(M4comp2018)
yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)
yearly_M4_test <- lapply(yearly_M4, function(temp){temp$xx})
yearly_M4_training <- lapply(yearly_M4, function(temp){temp$x})

mase_cal <- function(insample, outsample, forecasts){
  #Used to estimate MASE
  frq <- frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- (abs(outsample-forecasts))/masep
  return(mase)
}


cal_MASE <- function(training, test, forecast){
  m <- frequency(training)
  q_t <- abs(as.vector(test)-forecast)/mean(abs(diff(training, lag=m)))
  return(mean(q_t))
}


MASE_yearly <- rep(NA, 23000)

for(i in 1:23000){
  insample <- yearly_M4_training[[i]]
  outsample <- yearly_M4_test[[i]]
  forecasts <- fcast.combination.m4yearly[[i]]$mean
  #MASE_yearly[i] <- mean(mase_cal(insample, outsample, forecasts))
  MASE_yearly[i] <- cal_MASE(insample, outsample, forecasts)
}

mean(MASE_yearly) # 3.233323


## Quarterly series
readRDS("data/HPCfiles/fcast.combination.m4quarterly.rds")
MASE_quarterly <- rep(NA, 24000)
quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)
quarterly_M4_test <- lapply(quarterly_M4, function(temp){temp$xx})
quarterly_M4_training <- lapply(quarterly_M4, function(temp){temp$x})

MASE_quarterly <- rep(NA, 24000)

for(i in 1:24000){
  insample <- quarterly_M4_training[[i]]
  outsample <- quarterly_M4_test[[i]]
  forecasts <- fcast.combination.m4quarterly[[i]]$mean
  MASE_quarterly[i] <- mean(mase_cal(insample, outsample, forecasts))
  #MASE_quarterly[i] <- cal_MASE(insample, outsample, forecasts)
}

mean(MASE_quarterly) # 1.16

## Monthly
M4monthly_0 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_0.rds")
M4monthly_1 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_1.rds")
M4monthly_2 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_2.rds")
M4monthly_3 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_3.rds")
M4monthly_4 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_4.rds")
M4monthly_5 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_5.rds")
M4monthly_6 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_6.rds")
M4monthly_7 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_7.rds")
M4monthly_8 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_8.rds")
M4monthly_9 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_9.rds")
M4monthly_10 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_10.rds")
M4monthly_11 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_11.rds")
M4monthly_12 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_12.rds")
M4monthly_13 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_13.rds")
M4monthly_14 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_14.rds")
M4monthly_15 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_15.rds")
M4monthly_16 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_16.rds")
M4monthly_17 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_17.rds")
M4monthly_18 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_18.rds")
M4monthly_19 <- readRDS("~/PhD_journey/fforms/data/monthly/M4monthly_19.rds")


fcast.combination.m4monthly <- do.call(c, list(M4monthly_0, M4monthly_1, M4monthly_2,
                                       M4monthly_3, M4monthly_4, M4monthly_5,
                                       M4monthly_6, M4monthly_7, M4monthly_8,
                                       M4monthly_9, M4monthly_10, M4monthly_11,
                                       M4monthly_12, M4monthly_13, M4monthly_14,
                                       M4monthly_15, M4monthly_16, M4monthly_17,
                                       M4monthly_18, M4monthly_19))

MASE_monthly <- rep(NA, 48000)
monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
monthly_M4_test <- lapply(monthly_M4, function(temp){temp$xx})
monthly_M4_training <- lapply(monthly_M4, function(temp){temp$x})

MASE_monthly <- rep(NA, 48000)

for(i in 1:48000){
  insample <- monthly_M4_training[[i]]
  outsample <- monthly_M4_test[[i]]
  forecasts <- round(fcast.combination.m4monthly[[i]]$mean,2)
  MASE_monthly[i] <- mean(mase_cal(insample, outsample, forecasts))
  #MASE_quarterly[i] <- cal_MASE(insample, outsample, forecasts)
}

mean(MASE_monthly) # 0.9454616

## Weekly

fcast.combination.m4weekly.all <- readRDS("data/HPCfiles/fcast.combination.m4weekly.all.rds")
MASE_weekly <- rep(NA, 359)
weekly_M4 <- Filter(function(l) l$period == "Weekly", M4)
weekly_M4_test <- lapply(weekly_M4, function(temp){temp$xx})
weekly_M4_training <- lapply(weekly_M4, function(temp){temp$x})

MASE_weekly <- rep(NA, 359)


cal_MASE <- function(training, test, forecast){
  m <- 1
  q_t <- abs(as.vector(test)-forecast)/mean(abs(diff(training, lag=1)))
  return(mean(q_t))
}


for(i in 1:359){
  insample <- weekly_M4_training[[i]]
  outsample <- weekly_M4_test[[i]]
  forecasts <- tryCatch({fcast.combination.m4weekly.all[[i]]$mean}, error=function(e){return(NA)})
  #MASE_weekly[i] <- mean(mase_cal(insample, outsample, forecasts))
  MASE_weekly[i] <- cal_MASE(insample, outsample, forecasts)
}

mean(MASE_weekly, na.rm=TRUE) # 2.55

## Daily

fcast.combination.m4weekly.all <- readRDS("data/HPCfiles/fcast.combination.m4daily.all.rds")
MASE_daily <- rep(NA, 4227)
daily_M4 <- Filter(function(l) l$period == "Daily", M4)
daily_M4_test <- lapply(daily_M4, function(temp){temp$xx})
daily_M4_training <- lapply(daily_M4, function(temp){temp$x})

MASE_daily <- rep(NA, 4227)


cal_MASE <- function(training, test, forecast){
  m <- 1
  q_t <- abs(as.vector(test)-forecast)/mean(abs(diff(training, lag=1)))
  return(mean(q_t))
}


for(i in 1:4227){
  insample <- daily_M4_training[[i]]
  outsample <- daily_M4_test[[i]]
  forecasts <- tryCatch({fcast.combination.m4daily.all[[i]]$mean}, error=function(e){return(NA)})
  #MASE_weekly[i] <- mean(mase_cal(insample, outsample, forecasts))
  MASE_daily[i] <- cal_MASE(insample, outsample, forecasts)
}

mean(MASE_daily, na.rm=TRUE) # 3.42


## HOURLY

fcast.combination.m4hourly.all <- readRDS("data/HPCfiles/fcast.combination.m4hourly.all.rds")
MASE_hourly <- rep(NA, 414)
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)
hourly_M4_test <- lapply(hourly_M4, function(temp){temp$xx})
hourly_M4_training <- lapply(hourly_M4, function(temp){temp$x})

MASE_hourly <- rep(NA, 414)


cal_MASE <- function(training, test, forecast){
  m <- 1
  q_t <- abs(as.vector(test)-forecast)/mean(abs(diff(training)))
  return(mean(q_t))
}


for(i in 1:414){
  insample <- hourly_M4_training[[i]]
  outsample <- hourly_M4_test[[i]]
  forecasts <- fcast.combination.m4hourly.all[[i]]$mean
  MASE_hourly[i] <- mean(mase_cal(insample, outsample, forecasts))
  #MASE_weekly[i] <- cal_MASE(insample, outsample, forecasts)
}

mean(MASE_hourly) # 0.81
