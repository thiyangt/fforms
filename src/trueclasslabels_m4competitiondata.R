# packages
library(here)

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
