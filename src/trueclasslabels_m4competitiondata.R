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
ql0 <- labels(q0)
z1 <- cal_medianscaled(q1)
ql1 <- labels(q1)
z2 <- cal_medianscaled(q2)
ql2 <- labels(q2)
z3 <- cal_medianscaled(q3)
ql3 <- labels(q3)
z4 <- cal_medianscaled(q4)
ql4 <- labels(q4)
z5 <- cal_medianscaled(q5)
ql5 <- labels(q5)
z6 <- cal_medianscaled(q6)
ql6 <- labels(q6)
z7 <- cal_medianscaled(q7)
ql7 <- labels(q7)
z8 <- cal_medianscaled(q8)
ql8 <- labels(q8)
z9 <- cal_medianscaled(q9)
ql9 <- labels(q9)

qm4_true_classlabels <- c(ql0, ql1, ql2, ql3, ql4,
                          ql5, ql6, ql7, ql8, ql9)
length(qm4_true_classlabels)
table(qm4_true_classlabels)
save(qm4_true_classlabels, file="data/qm4_true_classlabels.rda")


# Monthly
## Load files
m0 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_0.rda"))
m0 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_0.rda"))
m0 <- get(load("~/PhD_journey/fforms/data/M4_benchmar_Accuracy/monthly/M4monthly_0.rda"))

z0 <- cal_medianscaled(q0)
ql0 <- labels(q0)
z1 <- cal_medianscaled(q1)
ql1 <- labels(q1)
z2 <- cal_medianscaled(q2)
ql2 <- labels(q2)
z3 <- cal_medianscaled(q3)
ql3 <- labels(q3)
z4 <- cal_medianscaled(q4)
ql4 <- labels(q4)
z5 <- cal_medianscaled(q5)
ql5 <- labels(q5)
z6 <- cal_medianscaled(q6)
ql6 <- labels(q6)
z7 <- cal_medianscaled(q7)
ql7 <- labels(q7)
z8 <- cal_medianscaled(q8)
ql8 <- labels(q8)
z9 <- cal_medianscaled(q9)
ql9 <- labels(q9)

qm4_true_classlabels <- c(ql0, ql1, ql2, ql3, ql4,
                          ql5, ql6, ql7, ql8, ql9)
length(qm4_true_classlabels)
table(qm4_true_classlabels)
save(qm4_true_classlabels, file="data/qm4_true_classlabels.rda")
