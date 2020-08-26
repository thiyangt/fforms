## ---- packages
library(Mcomp)
library(tidyverse)
library(magrittr)
library(ggbump)

## ---- parallelyearly
## Yearly
load("data/yearly/yearly_training.rda") # random forest training set 
load("data/yearly/train_votes.rda") # oob votes from the random forest (see: yearly_cluster_results for more info)
load("data/yearly/train_predictions_oob.rda") # based on oob prediction (see: yearly_cluster_results for more info)
votes_oob <- data.frame(train_votes)
names(votes_oob) <- names(table(train_predictions_oob))
votes_oob$predicted <- train_predictions_oob
votes_oob$classlabel <- yearly_training$classlabels
votes_oob <- votes_oob[, -c(11)]
#head(votes_oob)
paralle_y_df <- votes_oob %>%
  group_by(classlabel) %>%
  summarise_all(.funs = c(mean="mean"))
head(paralle_y_df)
paralle_y_df_pivot <- paralle_y_df %>% pivot_longer(2:11, "mean", "rf_classlabel")


paralle_y_df_pivot <- paralle_y_df_pivot %>%
  mutate(mean = recode(mean, "nn_mean"="nn",
                             "theta_mean" = "theta", "wn_mean" = "wn", "ARMA/AR/MA_mean" = "ARMA", "ARIMA_mean" = "ARIMA", "ETS-notrendnoseasonal_mean" = "ETS_NTNS",
                             "ETS-dampedtrend_mean" = "ETS_DT", "ETS-trend_mean" = "ETS_T", "rwd_mean" = "rwd", "rw_mean" = "rw" ))

paralle_y_df_pivot <- paralle_y_df_pivot %>%
  mutate(classlabel = recode(classlabel, nn="nn", theta = "theta",
                            wn = "wn", "ARMA/AR/MA" = "ARMA", ARIMA = "ARIMA",
                            "ETS-notrendnoseasonal" = "ETS_NTNS", "ETS-dampedtrend" = "ETS_DT",
                            "ETS-trend" = "ETS_T","rwd" = "rwd", "rw" = "rw" ))
paralle_y_df_pivot$mean <- factor(paralle_y_df_pivot$mean,
                             levels = c(
                               "wn",
                               "rwd",
                               "rw",
                               "nn",
                               "theta",
                               "ARMA",
                               "ARIMA",
                               "ETS_NTNS",
                               "ETS_DT",
                               "ETS_T"))



ggplot(paralle_y_df_pivot, aes(x=mean, y=value, group=classlabel))+
  geom_point(aes(color=classlabel)) +
  geom_line(aes(color=classlabel), alpha=0.5) +
  scale_color_manual(breaks=c("wn", "rwd", "rw", "nn", "theta", "ARMA", "ARIMA", "ETS_NTNS", "ETS_DT", "ETS_T"),
                     values=c("wn"="#a6cee3","rwd"="#1f78b4","rw"="#b2df8a",
                              "nn"="#33a02c", "theta"="#fb9a99","ARMA"="#e31a1c", 
                              "ARIMA"="#fdbf6f","ETS_NTNS"="#ff7f00","ETS_DT"="#cab2d6","ETS_T"="#6a3d9a"))
  
#p <- ggplot(paralle_y_df_pivot, aes(x=mean, y=value, group=classlabel))+
#  geom_bump(aes(color=classlabel), alpha=0.9) +   
#  geom_point(aes(color=classlabel)) +
#  scale_color_manual(breaks=c("wn", "rwd", "rw", "nn", "theta", "ARMA", "ARIMA", "ETS_NTNS", "ETS_DT", "ETS_T"),
#                    values=c("wn"="#a6cee3","rwd"="#1f78b4","rw"="#b2df8a",
#                             "nn"="#33a02c", "theta"="#fb9a99","ARMA"="#e31a1c", 
#                             "ARIMA"="#fdbf6f","ETS_NTNS"="#ff7f00","ETS_DT"="#cab2d6","ETS_T"="#6a3d9a"))


## ---- parallelquarterly  
## Quarterly
load("data/quarterly/trainQ_votes.rda") #oob votes from the random forest
load("data/quarterly/trainQ_predictions_oob.rda") # based on oob prediction
load("data/quarterly/quarterly_training.rda") # random forest training set
votes_oobQ <- data.frame(trainQ_votes)
names(votes_oobQ) <- names(table(trainQ_predictions_oob))
votes_oobQ$predicted <- trainQ_predictions_oob
votes_oobQ$classlabel <- as.factor(quarterly_training$classlabels)

paralle_q_df <- votes_oobQ %>%
  group_by(classlabel) %>%
  summarise_all(.funs = c(mean="mean"))
#head(paralle_m_df)
paralle_q_df_pivot <- paralle_q_df %>% pivot_longer(2:18, "mean", "rf_classlabel")
#head(paralle_m_df_pivot)
paralle_q_df_pivot <- paralle_q_df_pivot %>%
  mutate(mean = recode(mean, "ARIMA_mean"="ARIMA",
                       "ARMA/AR/MA_mean" = "ARMA",
                       "ETS-dampedtrend_mean" = "ETS_DT", 
                       "ETS-dampedtrendseasonal_mean" = "ETS_DTS", 
                       "ETS-notrendnoseasonal_mean" = "ETS_NTNS", 
                       "ETS-seasonal_mean" = "ETS_S",
                       "ETS-dampedtrend_mean" = "ETS_DT",
                       "ETS-trend_mean" = "ETS_T", 
                       "ETS-trendseasonal_mean" = "ETS_TS", 
                       "nn_mean" = "nn",
                       "rw_mean" = "rw",
                       "rwd_mean" = "rwd",
                       "SARIMA_mean" = "SARIMA",
                       "snaive_mean" = "snaive",
                       "stlar_mean" = "stlar",
                       "tbats_mean" = "tbats",
                       "theta_mean" = "theta",
                       "wn_mean" = "wn"))

paralle_q_df_pivot <- paralle_q_df_pivot %>%
  mutate(classlabel = recode(classlabel, "ARIMA"="ARIMA",
                             "ARMA/AR/MA" = "ARMA",
                             "ETS-dampedtrend" = "ETS_DT", 
                             "ETS-dampedtrendseasonal" = "ETS_DTS", 
                             "ETS-notrendnoseasonal" = "ETS_NTNS", 
                             "ETS-seasonal" = "ETS_S",
                             "ETS-dampedtrend" = "ETS_DT",
                             "ETS-trend" = "ETS_T", 
                             "ETS-trendseasonal" = "ETS_TS", 
                             "nn" = "nn",
                             "rw" = "rw",
                             "rwd" = "rwd",
                             "SARIMA" = "SARIMA",
                             "snaive" = "snaive",
                             "stlar" = "stlar",
                             "tbats" = "tbats",
                             "theta" = "theta",
                             "wn" = "wn"))

paralle_q_df_pivot$mean <- factor(paralle_q_df_pivot$mean,
                                  levels = c("wn","rwd","rw","nn","theta","ARMA","ARIMA","ETS_NTNS",
                                             "ETS_DT","ETS_T","ETS_DTS","ETS_TS","ETS_S",
                                             "SARIMA","stlar","tbats","snaive"))



ggplot(paralle_q_df_pivot, aes(x=mean, y=value, group=classlabel))+
  geom_point(aes(color=classlabel)) +
  geom_line(aes(color=classlabel), alpha=0.5) +
  scale_colour_manual(breaks=c("wn","rwd","rw","nn","theta","ARMA","ARIMA","ETS_NTNS","ETS_DT","ETS_T","ETS_DTS","ETS_TS","ETS_S","SARIMA","stlar","tbats","snaive"),
                      values=c("wn"="#a6cee3","rwd"="#1f78b4","rw"="#b2df8a",
                               "nn"="#33a02c", "theta"="#fb9a99","ARMA"="#e31a1c", "ARIMA"="#fdbf6f",
                               "ETS_NTNS"="#ff7f00","ETS_DT"="#cab2d6","ETS_T"="#6a3d9a",
                               "ETS_DTS"="#ffff99","ETS_TS"="#b15928","ETS_S"="#1b9e77","SARIMA"="#d95f02","stlar"="#7570b3","tbats"="#e7298a","snaive"="#66a61e"))



## ---- parallelmonthly
## Monthly
load("data/monthly/trainM_votes.rda") #oob votes from the random forest
load("data/monthly/trainM_predictions_oob.rda") # based on oob prediction
load("data/monthly/monthly_training.rda") # random forest training set
votes_oobM <- data.frame(trainM_votes)
names(votes_oobM) <- names(table(trainM_predictions_oob))
votes_oobM$predicted <- trainM_predictions_oob
votes_oobM$classlabel <- as.factor(monthly_training$classlabels)

paralle_m_df <- votes_oobM %>%
  group_by(classlabel) %>%
  summarise_all(.funs = c(mean="mean"))
#head(paralle_m_df)
paralle_m_df_pivot <- paralle_m_df %>% pivot_longer(2:18, "mean", "rf_classlabel")
#head(paralle_m_df_pivot)
paralle_m_df_pivot <- paralle_m_df_pivot %>%
  mutate(mean = recode(mean, "ARIMA_mean"="ARIMA",
                       "ARMA/AR/MA_mean" = "ARMA",
                       "ETS-dampedtrend_mean" = "ETS_DT", 
                       "ETS-dampedtrendseasonal_mean" = "ETS_DTS", 
                       "ETS-notrendnoseasonal_mean" = "ETS_NTNS", 
                       "ETS-seasonal_mean" = "ETS_S",
                       "ETS-dampedtrend_mean" = "ETS_DT",
                       "ETS-trend_mean" = "ETS_T", 
                       "ETS-trendseasonal_mean" = "ETS_TS", 
                       "nn_mean" = "nn",
                       "rw_mean" = "rw",
                       "rwd_mean" = "rwd",
                       "SARIMA_mean" = "SARIMA",
                       "snaive_mean" = "snaive",
                       "stlar_mean" = "stlar",
                       "tbats_mean" = "tbats",
                       "theta_mean" = "theta",
                       "wn_mean" = "wn"))

paralle_m_df_pivot <- paralle_m_df_pivot %>%
  mutate(classlabel = recode(classlabel, "ARIMA"="ARIMA",
                       "ARMA/AR/MA" = "ARMA",
                       "ETS-dampedtrend" = "ETS_DT", 
                       "ETS-dampedtrendseasonal" = "ETS_DTS", 
                       "ETS-notrendnoseasonal" = "ETS_NTNS", 
                       "ETS-seasonal" = "ETS_S",
                       "ETS-dampedtrend" = "ETS_DT",
                       "ETS-trend" = "ETS_T", 
                       "ETS-trendseasonal" = "ETS_TS", 
                       "nn" = "nn",
                       "rw" = "rw",
                       "rwd" = "rwd",
                       "SARIMA" = "SARIMA",
                       "snaive" = "snaive",
                       "stlar" = "stlar",
                       "tbats" = "tbats",
                       "theta" = "theta",
                       "wn" = "wn"))

paralle_m_df_pivot$mean <- factor(paralle_m_df_pivot$mean,
                                  levels = c("wn","rwd","rw","nn","theta","ARMA","ARIMA","ETS_NTNS",
                                             "ETS_DT","ETS_T","ETS_DTS","ETS_TS","ETS_S",
                                             "SARIMA","stlar","tbats","snaive"))



ggplot(paralle_m_df_pivot, aes(x=mean, y=value, group=classlabel))+
  geom_point(aes(color=classlabel)) +
  geom_line(aes(color=classlabel), alpha=0.5) +
  scale_colour_manual(breaks=c("wn","rwd","rw","nn","theta","ARMA","ARIMA","ETS_NTNS","ETS_DT","ETS_T","ETS_DTS","ETS_TS","ETS_S","SARIMA","stlar","tbats","snaive"),
                    values=c("wn"="#a6cee3","rwd"="#1f78b4","rw"="#b2df8a",
                             "nn"="#33a02c", "theta"="#fb9a99","ARMA"="#e31a1c", "ARIMA"="#fdbf6f",
                             "ETS_NTNS"="#ff7f00","ETS_DT"="#cab2d6","ETS_T"="#6a3d9a",
                             "ETS_DTS"="#ffff99","ETS_TS"="#b15928","ETS_S"="#1b9e77","SARIMA"="#d95f02","stlar"="#7570b3","tbats"="#e7298a","snaive"="#66a61e"))



## ---- parallelweekly
## Weekly

## ---- paralleldaily
## Daily

## ---- parallelhourly
## Hourly


