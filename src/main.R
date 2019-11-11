## ---- packages
library(Mcomp)
library(tidyverse)

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
  xlab("") +
  ylab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 20), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = c("nn", "theta", "wn", "ARMA", "ARIMA", "ETS_NTNS", "ETS_DT", "ETS_T", "rwd", "rw" )) +
  coord_flip() + facet_wrap(. ~ variable, ncol=5)+ggtitle("Yearly series")


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

#votes_oobM <- votes_oobM[!(votes_oobM$variable %in% c("rw", "ETS_NTNS", "ETS_DT", "ETS_T",
#                                                      "ARIMA", "ARMA","theta")),]


oob_boxplot_monthly <- ggplot(votes_oobM, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("")+
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 20), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20),
        strip.background = element_rect(size=4)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c(
    "snaive", "rwd", "rw", "ETS_NTNS", "ETS_DT", "ETS_T", "ETS_DTS", "ETS_TS", "ETS_S", "SARIMA",
    "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
  ))) +
  coord_flip() + facet_wrap(. ~ variable, ncol=5)+ggtitle("Monthly series")


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
  ylab("Random forest probability score (fraction of trees in the forest that vote for a certain class)") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 20), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                  "theta","nn","wn" ))) +
  coord_flip() + facet_wrap(. ~ variable, ncol=5)+ggtitle("Hourly series")



oob_boxplot_yearly + oob_boxplot_monthly + oob_boxplot_hourly + plot_layout(ncol = 1, heights = c(1.2, 4,1))


## ---- viplot
library(here)
library(tidyverse)
library(reshape2)
library(data.table)

# pre-processing variable importance plot
# Yearly series
# All variable scores into one dataframe
load(here("data", "yearly", "train_importance.rda"))
load(here("data", "yearly", "sd_pdf_df.rda"))
load(here("data", "yearly", "sd_ice_df.rda"))


## Permutation based
# head(train_importance)
# class(train_importance) #matrix
train_imp_df <- data.frame(train_importance)
train_imp_df <- rownames_to_column(train_imp_df, "Feature")
# names(train_imp_df)
train_imp_df <- within(train_imp_df, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_imp <- train_imp_df %>% melt(id.vars = "Feature")
# head(permutation_imp)
# dim(permutation_imp) # 250 3
colnames(permutation_imp) <- c("feature", "class", "score")
## PDP-based
# head(sd_pdf_df)
sd_pdf_df <- rownames_to_column(sd_pdf_df, "class")
# head(sd_pdf_df) %>% data.frame()
pdp_imp <- sd_pdf_df %>% melt(id.vars = "class")
# head(pdp_imp)
colnames(pdp_imp) <- c("class", "feature", "score")
# dim(pdp_imp) # 250 3
## ICE-based
# head(sd_ice_df)
sd_ice_df <- rownames_to_column(sd_ice_df, "class")
# head(sd_ice_df) %>% data.frame()
ice_imp <- sd_ice_df %>% melt(id.vars = "class")
# head(ice_imp)
colnames(ice_imp) <- c("class", "feature", "score")
# dim(ice_imp) # 250 3
## Combine the data frames
importancescoreY <- bind_rows(permutation_imp, pdp_imp)
importancescoreY <- bind_rows(importancescoreY, ice_imp)
importancescoreY$VI <- rep(c("permutation", "PDP", "ICE"), each = 250)
## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreY$class <- factor(importancescoreY$class,
                                 levels = c("rw", "rwd", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta", "nn"),
                                 labels = c("rw", "rwd", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta", "nn")
)
rank_vi_yearly_classes <- importancescoreY %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))
## compute mean rank
meanrank_viy_classes <- rank_vi_yearly_classes %>% 
  group_by(class, feature) %>% 
  summarise_at(vars(c(rank)), funs(mean))
## Note: highest rank is given to the most important feature
## data frame containing only the top five features in each class
top.yearly <- meanrank_viy_classes %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)


## Monthly data
load(here("data","monthly", "trainM_importance.rda"))
load(here("data", "monthly", "sd_pdf_dfM.rda"))
load(here("data", "monthly", "sd_ice_dfM.rda"))
## Permutation based
train_imp_dfM <- data.frame(trainM_importance)
train_imp_dfM <- tibble::rownames_to_column(train_imp_dfM, "Feature")
train_imp_dfM <- within(train_imp_dfM, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impM <- train_imp_dfM %>% melt(id.vars = "Feature")
colnames(permutation_impM) <- c("feature", "class", "score")
## PDP-based
sd_pdf_dfM <- tibble::rownames_to_column(sd_pdf_dfM, "class")
pdp_impM <- sd_pdf_dfM %>% melt(id.vars = "class")
colnames(pdp_impM) <- c("class", "feature", "score")
## ICE-based
sd_ice_dfM <- tibble::rownames_to_column(sd_ice_dfM, "class")
ice_impM <- sd_ice_dfM %>% melt(id.vars = "class")
colnames(ice_impM) <- c("class", "feature", "score")
## Combine the data frames
importancescoreM <- bind_rows(permutation_impM, pdp_impM)
importancescoreM <- bind_rows(importancescoreM, ice_impM)
importancescoreM$VI <- rep(c("permutation", "PDP", "ICE"), each = 510)
## rank permutation, sd_pdp, and sd_ice scores for each class
rank_vi_monthly_classes <- importancescoreM %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))
## compute mean rank
meanrank_vim_classes <- rank_vi_monthly_classes %>% 
  group_by(class, feature) %>% 
  summarise_at(vars(c(rank)), funs(mean))
top.monthly <- meanrank_vim_classes %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)


## Hourly data
# All variable scores into one dataframe
load(here("data", "hourly", "trainH_importance.rda"))
load(here("data", "hourly", "sd_pdf_dfH.rda"))
load(here("data", "hourly", "sd_ice_dfH.rda"))
## Permutation based
train_imp_dfH <- data.frame(trainH_importance)
train_imp_dfH <- tibble::rownames_to_column(train_imp_dfH, "Feature")
train_imp_dfH <- within(train_imp_dfH, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impH <- train_imp_dfH %>% melt(id.vars = "Feature")
#dim(permutation_impD) # 260 3
colnames(permutation_impH) <- c("feature", "class", "score")
## PDP-based
sd_pdf_dfH <- tibble::rownames_to_column(sd_pdf_dfH, "class")
pdp_imp <- sd_pdf_dfH %>% melt(id.vars = "class")
colnames(pdp_imp) <- c("class", "feature", "score")
## ICE-based
sd_ice_dfH <- tibble::rownames_to_column(sd_ice_dfH, "class")
ice_imp <- sd_ice_dfH %>% melt(id.vars = "class")
colnames(ice_imp) <- c("class", "feature", "score")
## Combine the data frames
importancescoreH <- bind_rows(permutation_impH, pdp_imp)
importancescoreH <- bind_rows(importancescoreH, ice_imp)
importancescoreH$VI <- rep(c("permutation", "PDP", "ICE"), each = 260)
## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreH$class <- factor(importancescoreH$class,
                                 levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                            "theta","nn","wn"),
                                 labels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                            "theta","nn","wn"))
rank_vi_hourly_classes <- importancescoreH %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))

## compute mean rank
meanrank_vih_classes <- rank_vi_hourly_classes %>% 
  group_by(feature, class) %>% 
  summarise_at(vars(c(rank)), funs(mean))
top.hourly <- meanrank_vih_classes %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)

## Match columns in yearly, monthly and hourly dataframes
yearly.monthly <- merge(top.yearly,top.monthly, by.y  = c("class","feature"), by.x=c("class", "feature"), all.x = TRUE, all.y = TRUE)
yearly.monthly <- yearly.monthly %>% rename("yearly.rank" = rank.x) 
yearly.monthly <- yearly.monthly %>% rename("monthly.rank" = rank.y) 
yearly.monthly.hourly.rank <- merge(yearly.monthly,top.hourly, by.y  = c("class","feature"), by.x=c("class", "feature"), all.x = TRUE, all.y = TRUE)
yearly.monthly.hourly.rank <- yearly.monthly.hourly.rank %>% rename("hourly.rank" = rank) 

yearly.monthly.hourly.rank <- yearly.monthly.hourly.rank %>% 
  mutate(ranks = case_when(is.na(yearly.rank) == FALSE & is.na(monthly.rank) == TRUE & is.na(hourly.rank) == TRUE ~ "Y", 
                           is.na(yearly.rank) == TRUE & is.na(monthly.rank) == FALSE & is.na(hourly.rank) == TRUE ~ "M",
                           is.na(yearly.rank) == TRUE & is.na(monthly.rank) == TRUE & is.na(hourly.rank) == FALSE ~ "H",
                           is.na(yearly.rank) == FALSE & is.na(monthly.rank) == FALSE & is.na(hourly.rank) == TRUE ~ "Y/M",
                           is.na(yearly.rank) == FALSE & is.na(monthly.rank) == TRUE & is.na(hourly.rank) == FALSE ~ "Y/H",
                           is.na(yearly.rank) == TRUE & is.na(monthly.rank) == FALSE & is.na(hourly.rank) == FALSE ~ "M/H",
                           is.na(yearly.rank) == FALSE & is.na(monthly.rank) == FALSE & is.na(hourly.rank) == FALSE ~ "Y/M/H"))

## Create a matrix to include the above information and colour it
## compile all names into a one place
yearly.feature.names.all <- levels(as.factor(meanrank_viy_classes$feature))
monthly.feature.names.all <- levels(as.factor(meanrank_vim_classes$feature))
hourly.feature.names.all <- levels(as.factor(meanrank_vih_classes$feature))
all.features <- unique(c(yearly.feature.names.all, monthly.feature.names.all, hourly.feature.names.all))


## compile all model names into a one place
yearly.models <- levels(as.factor(meanrank_viy_classes$class))
monthly.models <- levels(as.factor(meanrank_vim_classes$class))
hourly.models <- levels(as.factor(meanrank_vih_classes$class))
all.model.names <- unique(c(yearly.models, monthly.models, hourly.models))

matrix.fforms <- matrix(NA, ncol=19, nrow = 35)
colnames(matrix.fforms) <- c("snaive","rwd", "rw", "ETS.notrendnoseasonal", "ETS.dampedtrend",
                             "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal",
                             "ETS.seasonal", "SARIMA", "ARIMA", "ARMA.AR.MA",
                             "stlar", "tbats", "theta", "nn", "mstlarima", "mstlets", "wn")
rownames(matrix.fforms) <- all.features
longData <- melt(matrix.fforms)
longData <- longData %>% rename("feature" = Var1)
longData <- longData %>% rename("class" = Var2)
longData <- longData %>% rename("ranks" = value)
longData$ranks <- as.character(longData$ranks)
## Merge two data frames
vi.fforms <- merge(yearly.monthly.hourly.rank[,c("class", "feature", "ranks")] , longData, by.y  = c("class","feature"), by.x=c("class", "feature"), all.x = TRUE, all.y = TRUE)
vi.fforms  <- vi.fforms[, 1:3] 
vi.fforms <- vi.fforms %>% rename("ranks" = ranks.x)
library(dplyr)
vi.fforms <- 
  vi.fforms %>%
  mutate(classnew = case_when(
    class == "wn"  ~ 1,
    class == "rwd"  ~ 2,
    class == "rw"  ~ 3,
    class == "nn" ~ 4,
    class == "theta"  ~ 5,
    class == "ARMA.AR.MA"  ~ 6,
    class == "ARIMA"  ~ 7,
    class == "ETS.notrendnoseasonal" ~ 8,
    class == "ETS.dampedtrend"  ~ 9,
    class == "ETS.trend"  ~ 10,
    class == "ETS.dampedtrendseasonal"  ~ 11,
    class == "ETS.trendseasonal" ~ 12,
    class == "ETS.seasonal"  ~ 13,
    class == "SARIMA"  ~ 14,
    class == "stlar" ~ 15,
    class == "tbats"  ~ 16,
    class == "snaive"  ~ 17,
    class == "mstlarima"  ~ 18,
    class == "mstlets" ~ 19))

# vi.fforms <- 
#   vi.fforms %>%
#   mutate(featurenew = case_when(
#     feature == "diff2y_pacf5"  ~ 1,
#     feature == "hurst"  ~ 2,
#     feature == "hwbeta"  ~ 3,
#     feature == "lumpiness" ~ 4,
#     feature == "nonlinearity"  ~ 5,
#     feature == "ur_kpss"  ~ 6,
#     feature == "y_acf5"  ~ 7,
#     feature == "alpha" ~ 8,
#     feature == "diff2y_acf5"  ~ 9,
#     feature == "e_acf1"  ~ 10,
#     feature == "sediff_acf5"  ~ 11,
#     feature == "diff1y_acf5" ~ 12,
#     feature == "diff1y_pacf5"  ~ 13,
#     feature == "diff2y_acf1"  ~ 14,
#     feature == "hwalpha" ~ 15,
#     feature == "seas_pacf"  ~ 16,
#     feature == "sediff_acf1"  ~ 17,
#     feature == "spikiness"  ~ 18,
#     feature == "y_acf1" ~ 19,
#     feature == "beta"  ~ 20,
#     feature == "hwgamma"  ~ 21,
#     feature == "lmres_acf1"  ~ 22,
#     feature == "seasonal_strength2" ~ 23,
#     feature == "sediff_seacf1"  ~ 24,
#     feature == "curvature"  ~ 25,
#     feature == "entropy"  ~ 26,
#     feature == "diff1y_acf1" ~ 27,
#     feature == "stability"  ~ 28,
#     feature == "ur_pp"  ~ 29,
#     feature == "y_pacf5"  ~ 30,
#     feature == "N" ~ 31,
#     feature == "seasonal_strength1"  ~ 32,
#     feature == "linearity"  ~ 33,
#     feature == "trend" ~ 34,
#     feature == "seasonality"  ~ 35))


vi.fforms <- 
  vi.fforms %>%
  mutate(featurenew = case_when(
    feature == "diff2y_pacf5"  ~ 1,
    feature == "hurst"  ~ 2,
    feature == "hwbeta"  ~ 3,
    feature == "lumpiness" ~ 4,
    feature == "nonlinearity"  ~ 5,
    feature == "ur_kpss"  ~ 6,
    feature == "y_acf5"  ~ 7,
    feature == "seasonal_strength1"  ~ 8,
    feature == "seasonal_strength2" ~ 9,  
    feature == "seasonality"  ~ 10,
    feature == "hwgamma"  ~ 11,
    feature == "hwalpha" ~ 12,
    feature == "lmres_acf1"  ~ 13,
    feature == "ur_pp"  ~ 14,
    feature == "sediff_acf5"  ~ 15,
    feature == "seas_pacf"  ~ 16,
    feature == "sediff_acf1"  ~ 17,
    feature == "sediff_seacf1"  ~ 18,
    feature == "alpha" ~ 19,
    feature == "diff2y_acf5"  ~ 20,
    feature == "e_acf1"  ~ 21,
    feature == "diff1y_acf5" ~ 22,
    feature == "diff1y_pacf5"  ~ 23,
    feature == "diff2y_acf1"  ~ 24,    
    feature == "spikiness"  ~ 25,
    feature == "y_acf1" ~ 26,
    feature == "beta"  ~ 27,
    feature == "curvature"  ~ 28,
    feature == "entropy"  ~ 29,
    feature == "diff1y_acf1" ~ 30,
    feature == "stability"  ~ 31,
    feature == "y_pacf5"  ~ 32,
    feature == "N" ~ 33,
    feature == "linearity"  ~ 34,
    feature == "trend" ~ 35))
## Reserve a row for each entry
vi.fforms <- data.table(vi.fforms)
vi.fforms <- vi.fforms[, strsplit(as.character(ranks), "/"), by=list(classnew, featurenew)]
vi.fforms[, shift:=(1:(.N))/.N - 1/(2 * .N) - 1/2, by=list(classnew, featurenew)]
vi.fforms[, height:=1/.N, by=list(classnew, featurenew)]

#ggplot(vi.fforms, aes(y = feature, x = class, fill= ranks, label = ranks)) + 
#  geom_raster() +geom_text(col = "black", size=2)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))




ggplot(vi.fforms, aes(x = classnew,y=featurenew+shift, fill=V1, height=height)) + 
  geom_tile(color="black") + scale_fill_manual(na.value="white", values=c("#1b9e77", "#d95f02", "#7570b3"),  name = "Frequency \ncategory")+
  scale_x_discrete(limit=c("wn","rwd", "rw", "nn", "theta", "ARMA.AR.MA","ARIMA",  "ETS.notrendnoseasonal", "ETS.dampedtrend",
                           "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal",
                           "ETS.seasonal", "SARIMA",  
                           "stlar", "tbats","snaive","mstlarima", "mstlets"))+
  scale_y_discrete(limit=c(
    "diff2y_pacf5","hurst","hwbeta","lumpiness",
    "nonlinearity" , "ur_kpss","y_acf5",
    "seasonal_strength1","seasonal_strength2","seasonality",
    "hwgamma", "hwalpha",
    "lmres_acf1","ur_pp","sediff_acf5", "seas_pacf","sediff_acf1",
    "sediff_seacf1","alpha","diff2y_acf5","e_acf1",
    "diff1y_acf5","diff1y_pacf5","diff2y_acf1",  "spikiness",
    "y_acf1","beta","curvature","entropy","diff1y_acf1","stability","y_pacf5","N","linearity","trend"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=30))+xlab("")+ylab("")

