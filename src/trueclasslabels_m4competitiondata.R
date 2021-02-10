# packages
library(here)
library(seer)

# General function to extract labels

cal_medianscaled <- function(x){
  
  accuracy_mat <- x$accuracy
  mat_devidebymedian <- t(apply(accuracy_mat, 1, function(x) x/median(x, na.rm=TRUE)))
  accuracy_scaled <- rowsum(mat_devidebymedian, group = as.integer(gl(nrow(mat_devidebymedian),2,
                                                                      nrow(mat_devidebymedian))))
  accuracy <- accuracy_scaled/2
  ETS <- x$ETS
  ARIMA <- x$ARIMA
  
  accuracy_list <- list(accuracy=accuracy, ETS=ETS, ARIMA=ARIMA)
  accuracy_list
}


labels <- function(y){
  dim <- dim(y$accuracy)[1]
  dummy <- data.frame(x=rep(0, dim))
  prep_tset <- prepare_trainingset(accuracy_set = y, feature_set = dummy)
  prep_tset$trainingset$classlabels
}

# Yearly
## Load files
y0 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_0.rda"))
y1 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_1.rda"))
y2 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_2.rda") )
y3 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_3.rda")) 
y4 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_4.rda")) 
y5 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_5.rda"))
y6 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_6.rda")) 
y7 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_7.rda")) 
y8 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_8.rda")) 
y9 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/yearly/M4yearly_9.rda")) 


z0 <- cal_medianscaled(y0)
yl0 <- labels(z0)
z1 <- cal_medianscaled(y1)
yl1 <- labels(z1)
z2 <- cal_medianscaled(y2)
yl2 <- labels(z2)
z3 <- cal_medianscaled(y3)
yl3 <- labels(z3)
z4 <- cal_medianscaled(y4)
yl4 <- labels(z4)
z5 <- cal_medianscaled(y5)
yl5 <- labels(z5)
z6 <- cal_medianscaled(y6)
yl6 <- labels(z6)
z7 <- cal_medianscaled(y7)
yl7 <- labels(z7)
z8 <- cal_medianscaled(y8)
yl8 <- labels(z8)
z9 <- cal_medianscaled(y9)
yl9 <- labels(z9)

ym4_true_classlabels <- c(yl0, yl1, yl2, yl3, yl4,
                          yl5, yl6, yl7, yl8, yl9)
length(ym4_true_classlabels)
table(ym4_true_classlabels)
save(ym4_true_classlabels, file="data/ym4_true_classlabels.rda")


# Quarterly
## Load files
q0 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_0.rda"))
q1 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_1.rda"))
q2 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_2.rda"))
q3 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_3.rda"))
q4 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_4.rda"))
q5 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_5.rda"))
q6 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_6.rda"))
q7 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_7.rda"))
q8 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_8.rda"))
q9 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/quarterly/M4quarterly_9.rda"))


z0 <- cal_medianscaled(q0)
ql0 <- labels(z0)
z1 <- cal_medianscaled(q1)
ql1 <- labels(z1)
z2 <- cal_medianscaled(q2)
ql2 <- labels(z2)
z3 <- cal_medianscaled(q3)
ql3 <- labels(z3)
z4 <- cal_medianscaled(q4)
ql4 <- labels(z4)
z5 <- cal_medianscaled(q5)
ql5 <- labels(z5)
z6 <- cal_medianscaled(q6)
ql6 <- labels(z6)
z7 <- cal_medianscaled(q7)
ql7 <- labels(z7)
z8 <- cal_medianscaled(q8)
ql8 <- labels(z8)
z9 <- cal_medianscaled(q9)
ql9 <- labels(z9)

qm4_true_classlabels <- c(ql0, ql1, ql2, ql3, ql4,
                          ql5, ql6, ql7, ql8, ql9)
length(qm4_true_classlabels)
table(qm4_true_classlabels)
save(qm4_true_classlabels, file="data/qm4_true_classlabels.rda")


# Monthly
## Load files
m0 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_0.rda"))
m1 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_1.rda"))
m2 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_2.rda"))
m3 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_3.rda"))
m4 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_4.rda"))
m5 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_5.rda"))
m6 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_6.rda"))
m7 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_7.rda"))
m8 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_8.rda"))
m9 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_9.rda"))
m10 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_10.rda"))
m11 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_11.rda"))
m12 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_12.rda"))
m13 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_13.rda"))
m14 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_14.rda"))
m15 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_15.rda"))
m16 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_16.rda"))
m17 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_17.rda"))
m18 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_18.rda"))
m19 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_19.rda"))


z0 <- cal_medianscaled(m0)
ml0 <- labels(z0)
z1 <- cal_medianscaled(m1)
ml1 <- labels(z1)
z2 <- cal_medianscaled(m2)
ml2 <- labels(z2)
z3 <- cal_medianscaled(m3)
ml3 <- labels(z3)
z4 <- cal_medianscaled(m4)
ml4 <- labels(z4)
z5 <- cal_medianscaled(m5)
ml5 <- labels(z5)
z6 <- cal_medianscaled(m6)
ml6 <- labels(z6)
z7 <- cal_medianscaled(m7)
ml7 <- labels(z7)
z8 <- cal_medianscaled(m8)
ml8 <- labels(z8)
z9 <- cal_medianscaled(m9)
ml9 <- labels(z9)
z10 <- cal_medianscaled(m10)
ml10 <- labels(z10)
z11 <- cal_medianscaled(m11)
ml11 <- labels(z11)
z12 <- cal_medianscaled(m12)
ml12 <- labels(z12)
z13 <- cal_medianscaled(m13)
ml13 <- labels(z13)
z14 <- cal_medianscaled(m14)
ml14 <- labels(z14)
z15 <- cal_medianscaled(m15)
ml15 <- labels(z15)
z16 <- cal_medianscaled(m16)
ml16 <- labels(z16)
z17 <- cal_medianscaled(m17)
ml17 <- labels(z17)
z18 <- cal_medianscaled(m18)
ml18 <- labels(z18)
z19 <- cal_medianscaled(m19)
ml19 <- labels(z19)


mm4_true_classlabels <- c(ml0, ml1, ml2, ml3, ml4,
                          ml5, ml6, ml7, ml8, ml9,
                         ml10, ml11, ml12, ml13, ml14, ml15,
                         ml16, ml17, ml18, ml19)
length(mm4_true_classlabels)
table(mm4_true_classlabels)
save(mm4_true_classlabels, file="data/mm4_true_classlabels.rda")

## Weekly
# weekly data
m4w0 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/weekly/M4weekly_0.rda")
m4w_0 <- get(m4w0)
m4w1 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/weekly/M4weekly_1.rda")
m4w_1 <- get(m4w1)
m4w2 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/weekly/M4weekly_2.rda")
m4w_2 <- get(m4w2)
m4w3 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/weekly/M4weekly_3.rda")
m4w_3 <- get(m4w3)
m4w4 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/weekly/M4weekly_4.rda")
m4w_4 <- get(m4w4)
m4w5 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/weekly/M4weekly_5.rda")
m4w_5 <- get(m4w5)
m4w6 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/weekly/M4weekly_6.rda")
m4w_6 <- get(m4w6)
m4w7 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/weekly/M4weekly_7.rda")
m4w_7 <- get(m4w7)



## weekly accuracy matrix processing

arima <- c(as.vector(m4w_0$accuracy$arima), as.vector(m4w_1$accuracy$arima),
           as.vector(m4w_2$accuracy$arima),
           as.vector(m4w_3$accuracy$arima),
           as.vector(m4w_4$accuracy$arima),
           as.vector(m4w_5$accuracy$arima),
           as.vector(m4w_6$accuracy$arima),
           as.vector(m4w_7$accuracy$arima))

rw <- c(as.vector(m4w_0$accuracy$rw), as.vector(m4w_1$accuracy$rw),
        as.vector(m4w_2$accuracy$rw),
        as.vector(m4w_3$accuracy$rw),
        as.vector(m4w_4$accuracy$rw),
        as.vector(m4w_5$accuracy$rw),
        as.vector(m4w_6$accuracy$rw),
        as.vector(m4w_7$accuracy$rw))

rwd <- c(as.vector(m4w_0$accuracy$rwd), as.vector(m4w_1$accuracy$rwd),
         as.vector(m4w_2$accuracy$rwd),
         as.vector(m4w_3$accuracy$rwd),
         as.vector(m4w_4$accuracy$rwd),
         as.vector(m4w_5$accuracy$rwd),
         as.vector(m4w_6$accuracy$rwd),
         as.vector(m4w_7$accuracy$rwd))

wn <- c(as.vector(m4w_0$accuracy$wn), as.vector(m4w_1$accuracy$wn),
        as.vector(m4w_2$accuracy$wn),
        as.vector(m4w_3$accuracy$wn),
        as.vector(m4w_4$accuracy$wn),
        as.vector(m4w_5$accuracy$wn),
        as.vector(m4w_6$accuracy$wn),
        as.vector(m4w_7$accuracy$wn))

theta <- c(as.vector(m4w_0$accuracy$theta), as.vector(m4w_1$accuracy$theta),
           as.vector(m4w_2$accuracy$theta),
           as.vector(m4w_3$accuracy$theta),
           as.vector(m4w_4$accuracy$theta),
           as.vector(m4w_5$accuracy$theta),
           as.vector(m4w_6$accuracy$theta),
           as.vector(m4w_7$accuracy$theta))

stlar <- c(as.vector(m4w_0$accuracy$stlar), as.vector(m4w_1$accuracy$stlar),
           as.vector(m4w_2$accuracy$stlar),
           as.vector(m4w_3$accuracy$stlar),
           as.vector(m4w_4$accuracy$stlar),
           as.vector(m4w_5$accuracy$stlar),
           as.vector(m4w_6$accuracy$stlar),
           as.vector(m4w_7$accuracy$stlar))

nn <- c(as.vector(m4w_0$accuracy$nn), as.vector(m4w_1$accuracy$nn),
        as.vector(m4w_2$accuracy$nn),
        as.vector(m4w_3$accuracy$nn),
        as.vector(m4w_4$accuracy$nn),
        as.vector(m4w_5$accuracy$nn),
        as.vector(m4w_6$accuracy$nn),
        as.vector(m4w_7$accuracy$nn))

snaive <- c(as.vector(m4w_0$accuracy$snaive), as.vector(m4w_1$accuracy$snaive),
            as.vector(m4w_2$accuracy$snaive),
            as.vector(m4w_3$accuracy$snaive),
            as.vector(m4w_4$accuracy$snaive),
            as.vector(m4w_5$accuracy$snaive),
            as.vector(m4w_6$accuracy$snaive),
            as.vector(m4w_7$accuracy$snaive))

tbats <- c(as.vector(m4w_0$accuracy$tbats), as.vector(m4w_1$accuracy$tbats),
           as.vector(m4w_2$accuracy$tbats),
           as.vector(m4w_3$accuracy$tbats),
           as.vector(m4w_4$accuracy$tbats),
           as.vector(m4w_5$accuracy$tbats),
           as.vector(m4w_6$accuracy$tbats),
           as.vector(m4w_7$accuracy$tbats))

weeklyM4MASE_accuracy <- data.frame(arima, rw, rwd, wn, theta, stlar, nn, snaive, tbats)
weeklyM4MASE_accuracy <- as.matrix(weeklyM4MASE_accuracy )
weeklyM4MASE_accuracy2 <- list(accuracy = weeklyM4MASE_accuracy, ARIMA=rep(NA, 359), ETS=rep(NA, 359))


weekly <- cal_medianscaled(weeklyM4MASE_accuracy2)
wm4_true_classlabels <- colnames(weekly$accuracy)[apply(weekly$accuracy,1,which.min)]
save(wm4_true_classlabels, file="data/wm4_true_classlabels.rda")

## Daily
## Load files
m4d0 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_0.rda")
m4d_0 <- get(m4d0)
m4d1 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_1.rda")
m4d_1 <- get(m4d1)
m4d2 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_2.rda")
m4d_2 <- get(m4d2)
m4d3 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_3.rda")
m4d_3 <- get(m4d3)
m4d4 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_4.rda")
m4d_4 <- get(m4d4)
m4d5 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_5.rda")
m4d_5 <- get(m4d5)
m4d6 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_6.rda")
m4d_6 <- get(m4d6)
m4d7 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_7.rda")
m4d_7 <- get(m4d7)
m4d8 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_8.rda")
m4d_8 <- get(m4d8)
m4d9 <- load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/daily/M4daily_9.rda")
m4d_9 <- get(m4d9)


## daily accuracy matrix processing
dailyM4_accuracy <- rbind(m4d_0$accuracy, m4d_1$accuracy)
dailyM4_accuracy <- rbind(dailyM4_accuracy, m4d_2$accuracy)
dailyM4_accuracy <- rbind(dailyM4_accuracy, m4d_3$accuracy)
dailyM4_accuracy <- rbind(dailyM4_accuracy, m4d_4$accuracy)
dailyM4_accuracy <- rbind(dailyM4_accuracy, m4d_5$accuracy)
dailyM4_accuracy <- rbind(dailyM4_accuracy, m4d_6$accuracy)
dailyM4_accuracy <- rbind(dailyM4_accuracy, m4d_7$accuracy)
dailyM4_accuracy <- rbind(dailyM4_accuracy, m4d_8$accuracy)
dailyM4_accuracy <- rbind(dailyM4_accuracy, m4d_9$accuracy)
dim(dailyM4_accuracy)


dailyM4_accuracy2 <- list(accuracy = dailyM4_accuracy, ARIMA=rep(NA, 4227), ETS=rep(NA, 4227))
daily <- cal_medianscaled(dailyM4_accuracy2)
dm4_true_classlabels <- colnames(daily$accuracy)[apply(daily$accuracy,1,which.min)]
length(dm4_true_classlabels)
save(dm4_true_classlabels, file="data/dm4_true_classlabels.rda")


## Hourly
## Load files
h0 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/hourly/M4hourly_0.rda"))
h1 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/hourly/M4hourly_1.rda"))

hz0 <- cal_medianscaled(h0)
hl0 <- colnames(hz0$accuracy)[apply(hz0$accuracy,1,which.min)]
hz1 <- cal_medianscaled(h1)
hl1 <- colnames(hz1$accuracy)[apply(hz1$accuracy,1,which.min)]

hm4_true_classlabels <- c(hl0, hl1)
length(hm4_true_classlabels)
table(hm4_true_classlabels)
save(hm4_true_classlabels, file="data/hm4_true_classlabels.rda")
