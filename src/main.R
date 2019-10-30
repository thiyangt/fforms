## ---- packages
library(Mcomp)

## ---- yearlyoob
load("data/yearly/yearly_training.rda") # random forest training set 
load("data/yearly/train_votes.rda") # oob votes from the random forest (see: yearly_cluster_results for more info)
load("data/yearly/train_predictions_oob.rda") # based on oob prediction (see: yearly_cluster_results for more info)
votes_oob <- data.frame(train_votes)
names(votes_oob) <- names(table(train_predictions_oob))
votes_oob$predicted <- train_predictions_oob
votes_oob$classlabel <- yearly_training$classlabels
votes_oob <- votes_oob %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)
votes_oob <- votes_oob %>%
  mutate(classlabel = recode(classlabel, nn="nn",
                             theta = "theta", wn = "wn", "ARMA/AR/MA" = "ARMA", ARIMA = "ARIMA", "ETS-notrendnoseasonal" = "ETS_NTNS",
                             "ETS-dampedtrend" = "ETS_DT", "ETS-trend" = "ETS_T", "rwd" = "rwd", "rw" = "rw" ))

votes_oob <- votes_oob %>%
  mutate(predicted = recode(predicted, nn="nn", theta = "theta",
                            wn = "wn", "ARMA/AR/MA" = "ARMA", ARIMA = "ARIMA",
                            "ETS-notrendnoseasonal" = "ETS_NTNS", "ETS-dampedtrend" = "ETS_DT",
                            "ETS-trend" = "ETS_T","rwd" = "rwd", "rw" = "rw" ))

votes_oob <- votes_oob %>%
  mutate(variable = recode(variable, nn="nn", theta = "theta", wn = "wn", "ARMA/AR/MA" = "ARMA",
                           ARIMA = "ARIMA", "ETS-notrendnoseasonal" = "ETS_NTNS", "ETS-dampedtrend" = "ETS_DT",
                           "ETS-trend" = "ETS_T", "rwd" = "rwd", "rw" = "rw" ))
# arrange labels
votes_oob$variable <- factor(votes_oob$variable,
                             levels = rev(c(
                               "nn",
                               "theta",
                               "wn",
                               "ARMA",
                               "ARIMA",
                               "ETS_NTNS",
                               "ETS_DT",
                               "ETS_T",
                               "rwd",
                               "rw" )))

oob_boxplot_yearly <- ggplot(votes_oob, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 25), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = c("nn", "theta", "wn", "ARMA", "ARIMA", "ETS_NTNS", "ETS_DT", "ETS_T", "rwd", "rw" )) +
  coord_flip() + facet_wrap(. ~ variable, ncol=5)
oob_boxplot_yearly

## ---- oobmonthly
load("data/monthly/trainM_votes.rda") #oob votes from the random forest
load("data/monthly/trainM_predictions_oob.rda") # based on oob prediction
load("data/monthly/monthly_training.rda") # random forest training set
votes_oobM <- data.frame(trainM_votes)
names(votes_oobM) <- names(table(trainM_predictions_oob))
votes_oobM$predicted <- trainM_predictions_oob
votes_oobM$classlabel <- monthly_training$classlabels
votes_oobM <- votes_oobM %>% mutate(id=seq_len(n())) %>%
  melt(id.var=c('classlabel','id','predicted'), na.rm=T) %>%
  select(-id)
votes_oobM$classlabel <- factor(votes_oobM$classlabel, levels=rev(c("snaive","rwd", "rw", "ETS-notrendnoseasonal","ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal","ETS-seasonal","SARIMA",
                                                                    "ARIMA", "ARMA/AR/MA","stlar" ,"tbats","wn", "theta","nn"))
)


votes_oobM <- votes_oobM %>% mutate(classlabel = recode(classlabel,
                                                        "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS",
                                                        "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", 
                                                        "ETS-dampedtrendseasonal"="ETS_DTS", "ETS-trendseasonal"="ETS_TS", 
                                                        "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA", "ARIMA"="ARIMA",
                                                        "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
                                                        "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))

votes_oobM<- votes_oobM %>% mutate(predicted = recode(predicted,
                                                      "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS", 
                                                      "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", "ETS-dampedtrendseasonal"="ETS_DTS",
                                                      "ETS-trendseasonal"="ETS_TS", "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA",
                                                      "ARIMA"="ARIMA", "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
                                                      "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))

votes_oobM <- votes_oobM %>% mutate(variable= recode(variable,
                                                     "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS", 
                                                     "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", "ETS-dampedtrendseasonal"="ETS_DTS",
                                                     "ETS-trendseasonal"="ETS_TS", "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA",
                                                     "ARIMA"="ARIMA", "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
                                                     "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))
# new addition to arrange labels
votes_oobM$variable <- factor(votes_oobM$variable, levels = c(
  "snaive", "rwd", "rw", "ETS_NTNS", "ETS_DT", "ETS_T", "ETS_DTS", "ETS_TS", "ETS_S", "SARIMA",
  "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
))

oob_boxplot_monthly <- ggplot(votes_oobM, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("log(Proportion)") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 30), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 40),
        strip.background = element_rect(size=4)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c(
    "snaive", "rwd", "rw", "ETS_NTNS", "ETS_DT", "ETS_T", "ETS_DTS", "ETS_TS", "ETS_S", "SARIMA",
    "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
  ))) +
  coord_flip() + facet_wrap(. ~ variable, ncol=4)
oob_boxplot_monthly


## ---- oobhourly
load("data/hourly/trainH_votes.rda") #oob votes from the random forest
load("data/hourly/trainH_predictions_oob.rda") # based on oob prediction
load("data/hourly/hourly_training.rda") # random forest training set
votes_oobH <- data.frame(trainH_votes)
names(votes_oobH) <- names(table(trainH_predictions_oob))
votes_oobH$predicted <- trainH_predictions_oob
votes_oobH$classlabel <- hourly_training$classlabels
votes_oobH <- votes_oobH %>% mutate(id=seq_len(n())) %>%
  melt(id.var=c('classlabel','id','predicted'), na.rm=T) %>%
  select(-id)
#new addition to arrange labels 
votes_oobH$variable <- factor(votes_oobH$variable, 
                              levels=c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                       "theta","nn","wn")
)
oob_boxplot_hourly <- ggplot(votes_oobH, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 25), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                  "theta","nn","wn" ))) +
  coord_flip() + facet_wrap(. ~ variable, ncol=5)
oob_boxplot_hourly

