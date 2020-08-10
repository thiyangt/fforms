## ---- packages
library(Mcomp)
library(tidyverse)
library(magrittr)

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

oob_boxplot_yearly <- ggplot(votes_oob, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 20), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c("wn", "rwd", "rw", "nn", "theta", "ARMA", "ARIMA", "ETS_NTNS", "ETS_DT", "ETS_T" ))) +
  scale_fill_manual(breaks=c("wn", "rwd", "rw", "nn", "theta", "ARMA", "ARIMA", "ETS_NTNS", "ETS_DT", "ETS_T"),
                                    values=c("wn"="#a6cee3","rwd"="#1f78b4","rw"="#b2df8a",
                                             "nn"="#33a02c", "theta"="#fb9a99","ARMA"="#e31a1c", 
                                             "ARIMA"="#fdbf6f","ETS_NTNS"="#ff7f00","ETS_DT"="#cab2d6","ETS_T"="#6a3d9a"))+
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
votes_oobM$classlabel <- factor(votes_oobM$classlabel, 
                                levels=rev(
                                  c("snaive","rwd", "rw", "ETS-notrendnoseasonal","ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal","ETS-seasonal","SARIMA",
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
  "wn","rwd","rw","nn","theta","ARMA","ARIMA","ETS_NTNS","ETS_DT","ETS_T","ETS_DTS","ETS_TS","ETS_S","SARIMA","stlar","tbats","snaive"))

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
    "wn","rwd","rw","nn","theta","ARMA","ARIMA","ETS_NTNS","ETS_DT","ETS_T","ETS_DTS","ETS_TS","ETS_S","SARIMA","stlar","tbats","snaive"
  ))) +
  scale_fill_manual(breaks=c("wn","rwd","rw","nn","theta","ARMA","ARIMA","ETS_NTNS","ETS_DT","ETS_T","ETS_DTS","ETS_TS","ETS_S","SARIMA","stlar","tbats","snaive"),
                    values=c("wn"="#a6cee3","rwd"="#1f78b4","rw"="#b2df8a",
                             "nn"="#33a02c", "theta"="#fb9a99","ARMA"="#e31a1c", "ARIMA"="#fdbf6f",
                             "ETS_NTNS"="#ff7f00","ETS_DT"="#cab2d6","ETS_T"="#6a3d9a",
                             "ETS_DTS"="#ffff99","ETS_TS"="#b15928","ETS_S"="#1b9e77","SARIMA"="#d95f02","stlar"="#7570b3","tbats"="#e7298a","snaive"="#66a61e"))+
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
                              levels=c("wn","rwd","rw","nn","theta","stlar","tbats","snaive", "mstlarima", "mstlets")
)
oob_boxplot_hourly <- ggplot(votes_oobH, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Random forest probability score (fraction of trees in the forest that vote for a certain class)") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 20), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c("wn","rwd","rw","nn","theta","stlar","tbats","snaive", "mstlarima", "mstlets"))) +
  scale_fill_manual(breaks=c("wn","rwd","rw","nn","theta","stlar","tbats","snaive", "mstlarima", "mstlets"),
                    values=c("wn"="#a6cee3","rwd"="#1f78b4","rw"="#b2df8a",
                             "nn"="#33a02c", "theta"="#fb9a99","stlar"="#7570b3","tbats"="#e7298a","snaive"="#66a61e", 
                             "mstlarima"="#e6ab02", "mstlets"="#a6761d"))+
  coord_flip() + facet_wrap(. ~ variable, ncol=5)+ggtitle("Hourly series")



oob_boxplot_yearly + oob_boxplot_monthly + oob_boxplot_hourly + plot_layout(ncol = 1, heights = c(1.2, 4,1))

## ---- parallel
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
  geom_line(aes(color=classlabel), alpha=0.5) 
  

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
                           "stlar", "tbats","snaive","mstlarima", "mstlets"),
                   labels=c("wn"="wn","rwd"="rwd", "rw"="rw", "nn"="nn", "theta"="theta", 
                            "ARMA.AR.MA"="ARMA", "ARIMA"="ARIMA",  "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.dampedtrend"="ETS_DT",
                           "ETS.trend"="ETS_T", "ETS.dampedtrendseasonal"="ETS_DTS", "ETS.trendseasonal"="ETS_TS",
                           "ETS.seasonal"="ETS_S", "SARIMA"="SARIMA",  
                           "stlar"="stlar", "tbats"="tbats","snaive"="snaive","mstlarima"="mstlarima", "mstlets"="mstlets"))+
  scale_y_discrete(limit=c(
    "diff2y_pacf5","hurst","hwbeta","lumpiness",
    "nonlinearity" , "ur_kpss","y_acf5",
    "seasonal_D","seasonal_W","seasonality_M",
    "hwgamma", "hwalpha",
    "lmres_acf1","ur_pp","sediff_acf5", "seas_pacf","sediff_acf1",
    "sediff_seacf1","alpha","diff2y_acf5","e_acf1",
    "diff1y_acf5","diff1y_pacf5","diff2y_acf1",  "spikiness",
    "y_acf1","beta","curvature","entropy","diff1y_acf1","stability","y_pacf5","T","linearity","trend"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=30))+xlab("")+ylab("")

## ---- pdpyearlyurpp
load("data/yearly/ur_ppgrid_rmout.rda")
## Arrange graphs for faceting
keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.notrendnoseasonal",
                     "ETS.trend", "nn", "rw", "rwd", "theta", "wn")
keepur <- c(keep.modelnames, "ur_pp")
ur_ppgrid_rmout <- ur_ppgrid_rmout[, names(ur_ppgrid_rmout) %in% keepur]
ur_ppgrid_long <- gather(ur_ppgrid_rmout, class, probability, "ARIMA":"wn", factor_key = TRUE)

ur_ppgrid_long <- ur_ppgrid_long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))
ur_ppgrid_long$class <- factor(ur_ppgrid_long$class,
                               levels = c("rw", "rwd", "ETS_T", "ETS_DT", "ETS_NTNS",
                                          "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_yearly <- ggplot(data = ur_ppgrid_long, aes_string(x = ur_ppgrid_long$ur_pp, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "#7570b3", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="#7570b3", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3, se=TRUE)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 10))+xlab("test statistic based on Phillips-Perron unit root test (ur_pp)")+ylab("probability of selecting forecast-models")
plot_pdp_yearly



## ---- pdpmonthlyseasonality
load("data/monthly/seasonalitygridM.rda")
keep.modelnamesm <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                      "ETS.notrendnoseasonal", "ETS.seasonal", 
                      "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                      "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keepseasonal <- c(keep.modelnamesm, "seasonality")
seasonalitygridM <- seasonalitygridM[, names(seasonalitygridM) %in% keepseasonal]
seasonalitygridM_long <- gather(seasonalitygridM, class, probability, "ARIMA":"wn", factor_key = TRUE)

seasonalitygridM_long <- seasonalitygridM_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
seasonalitygridM_long <- seasonalitygridM_long %>% rename("seasonality_M"="seasonality")

seasonalitygridM_long$class <- factor(seasonalitygridM_long$class,
                                      levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                                 "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                                 "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_monthly_seasonal <- ggplot(data = seasonalitygridM_long, aes_string(x = seasonalitygridM_long$"seasonality_M", y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "#d95f02", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="#d95f02", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3, se=TRUE)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_wrap(. ~ class, ncol=9)+theme(strip.text.x = element_text(size = 10))+xlab("strength of seasonality")+ylab("probability of selecting forecast-models")
plot_pdp_monthly_seasonal

## ---- pdpmonthlyT
load("data/monthly/NgridM.rda")
keep.modelnamesM <- c("ARIMA", "ETS.dampedtrendseasonal",
                      "ETS.notrendnoseasonal", "ETS.seasonal", 
                      "ETS.trend", "rw",
                      "rwd", "SARIMA","theta")
keepN <- c(keep.modelnamesM, "N")
NgridM <- NgridM[, names(NgridM) %in% keepN]
NgridM_long <- gather(NgridM, class, probability, "ARIMA":"theta", factor_key = TRUE)

NgridM_long <- NgridM_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T" , "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","theta"="theta"))

NgridM_long$class <- factor(NgridM_long$class,
                            levels = c("rw", "rwd", "ETS_NTNS", "ETS_T", "ETS_DTS",
                                        "ETS_S","SARIMA",
                                       "ARIMA", "wn", "theta"))
NgridM_long <- NgridM_long %>% rename("T"="N")

plot_pdp_monthlyN <- ggplot(data = NgridM_long, aes_string(x = NgridM_long$"T", y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "#d95f02", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="#d95f02", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_wrap(. ~ class, ncol=9)+theme(strip.text.x = element_text(size = 10))+xlab("length of time series (T)")+ylab("probability of selecting forecast-models")
plot_pdp_monthlyN

## ---- pdpmonthlyhourlyStability
load("data/HPCfiles/stabilitygridM.rda")
keep.modelnamesM <- c("ARMA.AR.MA", "wn",
                      "rwd", "rw",
                      "nn", "theta",
                      "ETS.seasonal", "stlar","snaive")
keepStability <- c(keep.modelnamesM, "stability")
stabilitygridM <- stabilitygridM[, names(stabilitygridM) %in% keepStability]
stabilitygridM_long <- gather(stabilitygridM, class, probability, "ARMA.AR.MA":"wn", factor_key = TRUE)

stabilitygridM_long <- stabilitygridM_long %>%
  mutate(class = recode(class, "ARMA.AR.MA"="ARMA", "ETS.seasonal"="ETS_S", 
                       "wn"="wn",
                        "rwd"="rwd", "rw"="rw","nn"="nn", "theta"="theta", "stlar"="stlar", "snaive"="snaive"))

stabilitygridM_long$class <- factor(stabilitygridM_long$class,
                            levels = c("ARMA", "wn",
                                       "rwd", "rw",
                                       "nn", "theta",
                                       "ETS_S", "stlar","snaive"))

load("data/HPCfiles/stabilitygridH.rda")
keep.modelnamesH <- c("rwd", "rw")
keepStability <- c(keep.modelnamesH, "stability")
stabilitygridH <- stabilitygridH[, names(stabilitygridH) %in% keepStability]
stabilitygridH_long <- gather(stabilitygridH, class, probability, "rw":"rwd", factor_key = TRUE)

stabilitygridH_long <- stabilitygridH_long %>%
  mutate(class = recode(class, "rwd"="rwd", "rw"="rw"))

stabilitygridH_long$class <- factor(stabilitygridH_long$class,
                                    levels = c( "rwd", "rw"))

stability1_long_mean <- stabilitygridM_long%>%
  group_by(stability, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
stability2_long_mean <- stabilitygridH_long %>%
  group_by(stability, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)

stability_MH <- dplyr::bind_rows(stability1_long_mean, stability2_long_mean)
stability_MH$feature <- c(rep("Monthly", 180), rep("Hourly", 40))
stability_MH$feature <- factor(stability_MH$feature,levels = c("Monthly", "Hourly"))


plot_pdp_MH <- ggplot(stability_MH, aes(x=stability, y=mean, color=feature))+
  geom_line(aes(x=stability, y=mean, color=feature), size = 1)+
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=feature),alpha=0.4, colour = NA)+
  facet_wrap(. ~ class, ncol=9)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 16))+xlab("stability")+
  ylab("probability of selecting forecast-models")+
  theme(legend.position="bottom", legend.title=element_blank())+
  scale_colour_manual("",values=c("#d95f02", "#1b9e77"))+
  scale_fill_manual("",values=c("#d95f02", "#1b9e77"))
plot_pdp_MH


## ---- seasonalityhourly
load("data/hourly/seasonality1gridH.rda")
seasonality1gridH$variable <- rep(1:1000, 20)
load("data/hourly/seasonality2gridH.rda")
seasonality2gridH$variable <- rep(1:1000, 20)
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keeps1 <- c(keep.modelnames, "seasonal_strength1")
keeps2 <- c(keep.modelnames, "seasonal_strength2")
seasonal1 <- seasonality1gridH[, names(seasonality1gridH) %in% keeps1]
seasonal1 <- rename(seasonal1, seasonal = seasonal_strength1) 
seasonal2 <- seasonality2gridH[, names(seasonality2gridH) %in% keeps2]
seasonal2 <- rename(seasonal2, seasonal = seasonal_strength2) 
seasonal1_long <- gather(seasonal1, class, probability, "mstlarima":"wn", factor_key = TRUE)
seasonal2_long <- gather(seasonal2, class, probability, "mstlarima":"wn", factor_key = TRUE)
seasonal1_long_mean <- seasonal1_long %>%
  group_by(seasonal, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
seasonal2_long_mean <- seasonal2_long %>%
  group_by(seasonal, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)

seasonal_DW <- dplyr::bind_rows(seasonal1_long_mean, seasonal2_long_mean)
seasonal_DW$feature <- c(rep("seasonal_d", 200), rep("seasonal_w", 200))
seasonal_DW$class <- factor(seasonal_DW$class,
                            levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                       "theta","nn","wn"))

plot_pdp_hourly_seasonal <- ggplot(seasonal_DW, aes(x=seasonal, y=mean, color=feature))+
  geom_line(aes(x=seasonal, y=mean, color=feature), size = 1)+
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=feature),alpha=0.4, colour = NA)+
  facet_grid(. ~ class)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 18))+xlab("strength of seasonality")+
  ylab("probability of selecting forecast-models")+
  theme(legend.position="bottom", legend.title=element_blank())+
  scale_colour_manual("",values=c("#31a354", "#addd8e"))+
  scale_fill_manual("",values=c("#31a354", "#addd8e"))
plot_pdp_hourly_seasonal

## ---- entropyhourly
load("data/hourly/entropygridH.rda")
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rwd","tbats","stlar","nn","wn")
keepentropy <- c(keep.modelnames, "entropy")
entropy1 <- entropygridH[, names(entropygridH) %in% keepentropy]
entropy1_long <- gather(entropy1, class, probability,  "nn":"wn", factor_key = TRUE)
entropy1_long$class <- factor(entropy1_long$class,
                              levels = c("snaive", "rwd","tbats","stlar","nn","wn"))

plot_pdp_hourly_entropy <- ggplot(data = entropy1_long, aes_string(x = entropy1_long$entropy, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="#1b9e77", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 16))+xlab("entropy")+ylab("probability of selecting forecast-models")
plot_pdp_hourly_entropy

## ---- curvaturehourly
load("data/hourly/curvaturegridH.rda")
## Arrange graphs for faceting
keep.modelnames <- c("mstlarima", "mstlets","theta")
keepcurvature <- c(keep.modelnames, "curvature")
curvaturehourly1 <- curvaturegridH[, names(curvaturegridH) %in% keepcurvature]
curvaturehourly1_long <- gather(curvaturehourly1 , class, probability,  "mstlarima":"theta", factor_key = TRUE)
curvaturehourly1_long$class <- factor(curvaturehourly1_long$class,
                                      levels = c("mstlarima", "mstlets","theta"))

plot_pdp_hourly_curvature <- ggplot(data = curvaturehourly1_long, aes_string(x = curvaturehourly1_long$curvature, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="#1b9e77", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 16))+xlab("curvature")+ylab("probability of selecting forecast-models")
plot_pdp_hourly_curvature


## ----diff1yacf1
load("data/yearly/diff1y_acf1grid.rda")
keep.modelnames <- c("ARIMA", "ETS.dampedtrend","ETS.notrendnoseasonal", 
                     "ETS.trend", "nn", "rwd", "theta", "wn")
keepdiff1yacf1 <- c(keep.modelnames, "diff1y_acf1")
diff1yacf1grid <- diff1y_acf1grid[, names(diff1y_acf1grid) %in% keepdiff1yacf1]
diff1yacf1grid_long <- gather(diff1yacf1grid, class, probability, "ARIMA":"wn", factor_key = TRUE)
diff1yacf1grid_long <- diff1yacf1grid_long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd" ))

diff1yacf1grid_long$class <- factor(diff1yacf1grid_long$class,
                                 levels = c("rwd", "ETS_NTNS", "ETS_DT", "ETS_T", "ARIMA", 
                                            "theta", "nn", "wn"))

load("data/monthly/diff1y_acf1gridM.rda")
keep.modelnamesMdiff1yacf1 <- c("rwd", "ARIMA", "ETS.dampedtrend", "tbats")
keepdiff1yacf1 <- c(keep.modelnamesMdiff1yacf1, "diff1y_acf1")
diff1y_acf1gridM <- diff1y_acf1gridM[, names(diff1y_acf1gridM) %in% keepdiff1yacf1]
diff1y_acf1gridM_long <- gather(diff1y_acf1gridM, class, probability, "ARIMA":"tbats", factor_key = TRUE)

diff1y_acf1gridM_long <- diff1y_acf1gridM_long %>%
  mutate(class = recode(class,  ARIMA = "ARIMA",
                        "ETS.dampedtrend" = "ETS_DT",
                        "tbats" = "tbats",
                        "rwd" = "rwd" ))

diff1y_acf1gridM_long$class <- factor(diff1y_acf1gridM_long$class,
                                  levels = c("rwd", "ETS_DT", "ARIMA", "tbats"))


diff1yacf1grid_long_mean <- diff1yacf1grid_long %>%
  group_by(diff1y_acf1, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)

diff1y_acf1gridM_long_mean <- diff1y_acf1gridM_long %>%
  group_by(diff1y_acf1, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)


diff1yacf1_YM <- dplyr::bind_rows(diff1yacf1grid_long_mean, diff1y_acf1gridM_long_mean)
diff1yacf1_YM$feature <- c(rep("Yearly", dim(diff1yacf1grid_long_mean)[1]), 
                         rep("Monthly", dim(diff1y_acf1gridM_long_mean)[1]))
diff1yacf1_YM$class <- factor(diff1yacf1_YM$class,
                            levels = c("rwd", "ETS_NTNS", "ETS_DT", "ETS_T", "ARIMA", 
                                       "theta", "nn", "wn","tbats"))
diff1yacf1_YM$feature <- factor(diff1yacf1_YM$feature,levels = c("Yearly", "Monthly"))

ggplot(diff1yacf1_YM, aes(x=diff1y_acf1, y=mean, color=feature))+
  geom_line(aes(x=diff1y_acf1, y=mean, color=feature), size = 1)+
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=feature),alpha=0.4, colour = NA)+
  facet_wrap(. ~ class, ncol=9)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 16))+xlab("diff1y_acf1")+
  ylab("probability of selecting forecast-models")+
  theme(legend.position="bottom", legend.title=element_blank())+
  scale_colour_manual("",values=c("#7570b3", "#d95f02"))+
  scale_fill_manual("",values=c("#7570b3", "#d95f02"))



## ---- ypacf5
load("data/yearly/y_pacf5grid.rda")
keep.modelnames <- c( "nn", "rw", "rwd", "theta", "wn")
keepy_pacf5 <- c(keep.modelnames, "y_pacf5")
y_pacf5grid <- y_pacf5grid[, names(y_pacf5grid) %in% keepy_pacf5]
y_pacf5grid_long <- gather(y_pacf5grid, class, probability, "nn":"wn", factor_key = TRUE)
y_pacf5grid_long <- y_pacf5grid_long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "rwd" = "rwd",
                        "rw" = "rw" ))

y_pacf5grid_long$class <- factor(y_pacf5grid_long$class,
                                 levels = c("rw", "rwd", "wn", "theta", "nn" ))

load("data/monthly/y_pacf5gridM.rda")
keep.modelnamesM <- c("nn", "rw",
                      "rwd","stlar","tbats","theta", "wn")
keepy_pacf5 <- c(keep.modelnamesM, "y_pacf5")
y_pacf5gridM <- y_pacf5gridM[, names(y_pacf5gridM) %in% keepy_pacf5]
y_pacf5gridM_long <- gather(y_pacf5gridM, class, probability, "nn":"wn", factor_key = TRUE)

y_pacf5gridM_long <- y_pacf5gridM_long %>%
  mutate(class = recode(class, "nn"="nn", "rw"="rw",
                        "rwd"="rwd", "stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))

y_pacf5gridM_long$class <- factor(y_pacf5gridM_long$class,
                                  levels = c("rw", "rwd", "tbats","stlar", "wn", "theta", "nn" ))

load("data/hourly/y_pacf5gridH.rda")
## Arrange graphs for faceting
keep.modelnames <- c("rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keepy_pacf5 <- c(keep.modelnames, "y_pacf5")
y_pacf5hourly1 <- y_pacf5gridH[, names(y_pacf5gridH) %in% keepy_pacf5]
y_pacf5hourly1_long <- gather(y_pacf5hourly1 , class, probability,  "mstlarima":"wn", factor_key = TRUE)
y_pacf5hourly1_long$class <- factor(y_pacf5hourly1_long$class,
                                    levels = c("rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                               "theta","nn","wn"))

y_pacf5y_long_mean <- y_pacf5grid_long%>%
  group_by(y_pacf5, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
y_pacf5m_long_mean <- y_pacf5gridM_long %>%
  group_by(y_pacf5, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
y_pacf5h_long_mean <- y_pacf5hourly1_long %>%
  group_by(y_pacf5, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)

y_pacf5_YMH <- dplyr::bind_rows(y_pacf5y_long_mean, y_pacf5m_long_mean)
y_pacf5_YMH <- dplyr::bind_rows(y_pacf5_YMH, y_pacf5h_long_mean)
y_pacf5_YMH$feature <- c(rep("Yearly", dim(y_pacf5y_long_mean)[1]), 
                         rep("Monthly", dim(y_pacf5m_long_mean)[1]), 
                         rep("Hourly", dim(y_pacf5h_long_mean)[1]))
y_pacf5_YMH$class <- factor(y_pacf5_YMH$class,
                            levels = c("rw", "rwd", "tbats","stlar","wn", "theta", "nn","mstlarima", "mstlets"))
y_pacf5_YMH$feature <- factor(y_pacf5_YMH$feature,levels = c("Yearly", "Monthly", "Hourly"))


plot_pdp_YMH_y_pacf5 <- ggplot(y_pacf5_YMH, aes(x=y_pacf5, y=mean, color=feature))+
  geom_line(aes(x=y_pacf5, y=mean, color=feature), size = 1)+
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=feature),alpha=0.4, colour = NA)+
  facet_wrap(. ~ class, ncol=9)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 16))+xlab("y_pacf5")+
  ylab("probability of selecting forecast-models")+
  theme(legend.position="bottom", legend.title=element_blank())+
  scale_colour_manual("",values=c("#7570b3", "#d95f02", "#1b9e77"))+
  scale_fill_manual("",values=c("#7570b3", "#d95f02", "#1b9e77"))
plot_pdp_YMH_y_pacf5


## ---- pdpyearlytrend
load("data/yearly/trendgrid.rda")
keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.notrendnoseasonal",
                     "ETS.trend", "nn", "rw", "rwd", "theta", "wn")
keeptrend <- c(keep.modelnames, "trend")
trendgrid <- trendgrid[, names(trendgrid) %in% keeptrend]
trendgrid_long <- gather(trendgrid, class, probability, "ARIMA":"wn", factor_key = TRUE)
trendgrid_long <- trendgrid_long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))
trendgrid_long$class <- factor(trendgrid_long$class,
                               levels = c("rw", "rwd", "ETS_T", "ETS_DT", "ETS_NTNS",
                                          "ARIMA", "ARMA", "wn", "theta", "nn" ))


load("data/monthly/trendgridM.rda")
keep.modelnamet <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                     "ETS.notrendnoseasonal", "ETS.seasonal", 
                     "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                     "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keeptrend <- c(keep.modelnamet, "trend")
trendgridM <- trendgridM[, names(trendgridM) %in% keeptrend]
trendgridM_long <- gather(trendgridM, class, probability, "ARIMA":"wn", factor_key = TRUE)
trendgridM_long <- trendgridM_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
trendgridM_long$class <- factor(trendgridM_long$class,
                                levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                           "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                           "ARIMA", "ARMA", "wn", "theta", "nn" ))


load("data/HPCfiles/trendgridH.rda")
keep.modelnamet <- c("wn", "mstlarima")
keeptrend <- c(keep.modelnamet, "trend")
trendgridH <- trendgridH[, names(trendgridH) %in% keeptrend]
trendgridH_long <- gather(trendgridH, class, probability, "mstlarima":"wn", factor_key = TRUE)
trendgridH_long <- trendgridH_long %>%
  mutate(class = recode(class, "mstlarima"="mstlarima", "wn"="wn"))
trendgridH_long$class <- factor(trendgridH_long$class,
                                levels = c("mstlarima","wn"))



trend1_long_mean <- trendgrid_long%>%
  group_by(trend, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
trend2_long_mean <- trendgridM_long %>%
  group_by(trend, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
trend3_long_mean <- trendgridH_long %>%
  group_by(trend, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)


trend_YMH <- dplyr::bind_rows(trend1_long_mean, trend2_long_mean, trend3_long_mean)
trend_YMH$feature <- c(rep("Yearly", 200), rep("Monthly", 340), rep("Hourly", 40))
trend_YMH$class <- factor(trend_YMH$class,
                            levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                       "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                       "ARIMA", "ARMA", "wn", "theta", "nn", "mstlarima"))
trend_YMH$feature <- factor(trend_YMH$feature,levels = c("Yearly", "Monthly", "Hourly"))


plot_pdp_YMH <- ggplot(trend_YMH, aes(x=trend, y=mean, color=feature))+
  geom_line(aes(x=trend, y=mean, color=feature), size = 1)+
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=feature),alpha=0.4, colour = NA)+
  facet_wrap(. ~ class, ncol=9)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 16))+xlab("strength of trend")+
  ylab("probability of selecting forecast-models")+
  theme(legend.position="bottom", legend.title=element_blank())+
  scale_colour_manual("",values=c("#7570b3", "#d95f02", "#1b9e77"))+
  scale_fill_manual("",values=c("#7570b3", "#d95f02", "#1b9e77"))
plot_pdp_YMH


## ---- linearity
load("data/yearly/linearitygrid.rda")
keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.notrendnoseasonal",
                     "ETS.trend", "nn", "rw", "rwd", "theta", "wn")
keeplinearity <- c(keep.modelnames, "linearity")
linearitygrid <- linearitygrid[, names(linearitygrid) %in% keeplinearity]
linearitygrid_long <- gather(linearitygrid, class, probability, "ARIMA":"wn", factor_key = TRUE)
linearitygrid_long <- linearitygrid_long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))

linearitygrid_long$class <- factor(linearitygrid_long$class,
                                   levels = c("rw", "rwd", "ETS_T", "ETS_DT", "ETS_NTNS",
                                              "ARIMA", "ARMA", "wn", "theta", "nn" ))

load("data/monthly/linearitygridM.rda")
keep.modelnamesM <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                      "ETS.notrendnoseasonal", "ETS.seasonal", 
                      "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                      "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keeplinearity <- c(keep.modelnamesM, "linearity")
linearitygridM <- linearitygridM[, names(linearitygridM) %in% keeplinearity]
linearitygridM_long <- gather(linearitygridM, class, probability, "ARIMA":"wn", factor_key = TRUE)

linearitygridM_long <- linearitygridM_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))

linearitygridM_long$class <- factor(linearitygridM_long$class,
                                    levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                               "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                               "ARIMA", "ARMA", "wn", "theta", "nn" ))

load("data/hourly/linearitygridH.rda")
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keeplinearity <- c(keep.modelnames, "linearity")
linearityhourly1 <- linearitygridH[, names(linearitygridH) %in% keeplinearity]
linearityhourly1_long <- gather(linearityhourly1 , class, probability,  "mstlarima":"wn", factor_key = TRUE)
linearityhourly1_long$class <- factor(linearityhourly1_long$class,
                                      levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                                 "theta","nn","wn"))

ly_long_mean <- linearitygrid_long%>%
  group_by(linearity, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
lm_long_mean <- linearitygridM_long %>%
  group_by(linearity, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
lh_long_mean <- linearityhourly1_long %>%
  group_by(linearity, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)

linearity_YMH <- dplyr::bind_rows(ly_long_mean, lm_long_mean)
linearity_YMH <- dplyr::bind_rows(linearity_YMH, lh_long_mean)
linearity_YMH$feature <- c(rep("Yearly", 200), rep("Monthly", 340), rep("Hourly", 200))
linearity_YMH$class <- factor(linearity_YMH$class,
                         levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                    "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                    "ARIMA", "ARMA", "wn", "theta", "nn","mstlarima", "mstlets"))
linearity_YMH$feature <- factor(linearity_YMH$feature,levels = c("Yearly", "Monthly", "Hourly"))


plot_pdp_YMH_linearity <- ggplot(linearity_YMH, aes(x=linearity, y=mean, color=feature))+
  geom_line(aes(x=linearity, y=mean, color=feature), size = 1)+
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=feature),alpha=0.4, colour = NA)+
  facet_wrap(. ~ class, ncol=7)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 16))+xlab("strength of linearity")+
  ylab("probability of selecting forecast-models")+
  theme(legend.position="bottom", legend.title=element_blank())+
  scale_colour_manual("",values=c("#7570b3", "#d95f02", "#1b9e77"))+
  scale_fill_manual("",values=c("#7570b3", "#d95f02", "#1b9e77"))
plot_pdp_YMH_linearity+xlim(-15,15)


## ----y2dpdp
load("data/HPCfiles/lmres_acf1.linearityrmout.y.rda")
colNamesls <- colnames(lmres_acf1.linearityrmout.y)[27:36]

keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.notrendnoseasonal",
                     "ETS.trend", "nn", "rw", "rwd", "theta", "wn")
keepy <- c(keep.modelnames, c("lmres_acf1", "linearity"))
lmres_acf1.linearityrmout.y <- lmres_acf1.linearityrmout.y[, names(lmres_acf1.linearityrmout.y) %in% keepy]
lmres_acf1.linearityrmout.y.long <- gather(lmres_acf1.linearityrmout.y, class, probability, "ARIMA":"wn", factor_key = TRUE)
lmres_acf1.linearityrmout.y.long <- lmres_acf1.linearityrmout.y.long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))
lmres_acf1.linearityrmout.y.long$class <- factor(lmres_acf1.linearityrmout.y.long$class,
                                                 levels = c("rw", "rwd", "theta", "nn", "wn", "ARIMA","ETS_T", "ETS_DT", "ETS_NTNS",
                                                             "ARMA"))

y1 <- lmres_acf1.linearityrmout.y.long %>%
  ggplot(aes(y = lmres_acf1, x = linearity, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=10) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.45,20),
                       limits=c(0,0.45))+
  theme(strip.text.x = element_text(size = 10))+ylim(-1,1)

load("~/PhD_journey/fforms/data/HPCfiles/diff1y_acf1.linearityrmout.y.rda")
colNamesls <- colnames(diff1y_acf1.linearityrmout.y)[27:36]

keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.notrendnoseasonal",
                     "ETS.trend", "nn", "rw", "rwd", "theta", "wn")
keepy <- c(keep.modelnames, c("diff1y_acf1", "linearity"))
diff1y_acf1.linearityrmout.y <- diff1y_acf1.linearityrmout.y[, names(diff1y_acf1.linearityrmout.y) %in% keepy]
diff1y_acf1.linearityrmout.y.long <- gather(diff1y_acf1.linearityrmout.y, class, probability, "ARIMA":"wn", factor_key = TRUE)
diff1y_acf1.linearityrmout.y.long <- diff1y_acf1.linearityrmout.y.long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))
diff1y_acf1.linearityrmout.y.long$class <- factor(diff1y_acf1.linearityrmout.y.long$class,
                                                  levels = c("rw", "rwd", "theta", "nn", "wn", "ARIMA","ETS_T", "ETS_DT", "ETS_NTNS",
                                                             "ARMA"))

y2 <- diff1y_acf1.linearityrmout.y.long %>%
  ggplot(aes(y = diff1y_acf1, x = linearity, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=10) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.45,100),
                       limits=c(0,0.45))+
  theme(strip.text.x = element_text(size = 10))+ylim(-1,1)

y1/y2

## ----m2dpdp
load("data/HPCfiles/sediff_seacf1.linearity.m.rda")
colNamesds <-colnames(sediff_seacf1.linearity.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("sediff_seacf1", "linearity"))
sediff_seacf1.linearity.m <- sediff_seacf1.linearity.m[, names(sediff_seacf1.linearity.m) %in% keepm]
sediff_seacf1.linearity.m.long <- gather(sediff_seacf1.linearity.m, class, probability, "ETS.notrendnoseasonal":"theta", factor_key = TRUE)
sediff_seacf1.linearity.m.long <- sediff_seacf1.linearity.m.long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
sediff_seacf1.linearity.m.long$class <- factor(sediff_seacf1.linearity.m.long$class,
                                               levels = c("stlar","nn", "theta", "tbats","snaive", "rw", "ETS_NTNS"))
m12d <- sediff_seacf1.linearity.m.long %>%
  ggplot(aes(y = sediff_seacf1, x = linearity, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.15,100),
                       limits=c(0,0.15), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))

load("data/HPCfiles/stability.linearity.m.rda")
colNamesds <-colnames(stability.linearity.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("stability", "linearity"))
stability.linearity.m <- stability.linearity.m[, names(stability.linearity.m) %in% keepm]
stability.linearity.m.long <- gather(stability.linearity.m, class, probability, "ETS.notrendnoseasonal":"theta", factor_key = TRUE)
stability.linearity.m.long <- stability.linearity.m.long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
stability.linearity.m.long$class <- factor(stability.linearity.m.long$class,
                                           levels = c("stlar","nn", "theta", "tbats","snaive", "rw", "ETS_NTNS"))
m22d <- stability.linearity.m.long %>%
  ggplot(aes(y = stability, x = linearity, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.2,100),
                       limits=c(0,0.2), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))

load("data/HPCfiles/e_acf1.linearity.m.rda")
colNamesds <-colnames(e_acf1.linearity.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("e_acf1", "linearity"))
e_acf1.linearity.m <- e_acf1.linearity.m[, names(e_acf1.linearity.m) %in% keepm]
e_acf1.linearity.m.long <- gather(e_acf1.linearity.m, class, probability, "ETS.notrendnoseasonal":"theta", factor_key = TRUE)
e_acf1.linearity.m.long <- e_acf1.linearity.m.long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
e_acf1.linearity.m.long$class <- factor(e_acf1.linearity.m.long$class,
                                        levels = c("stlar","nn", "theta", "tbats", "snaive", "rw", "ETS_NTNS"))
m32d <- e_acf1.linearity.m.long %>%
  ggplot(aes(y = e_acf1, x = linearity, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.2,100),
                       limits=c(0,0.2), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))

load("data/HPCfiles/hwalpha.linearity.m.rda")
colNamesds <-colnames(hwalpha.linearity.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("hwalpha", "linearity"))
hwalpha.linearity.m <- hwalpha.linearity.m[, names(hwalpha.linearity.m) %in% keepm]
hwalpha.linearity.m.long <- gather(hwalpha.linearity.m, class, probability, "ETS.notrendnoseasonal":"theta", factor_key = TRUE)
hwalpha.linearity.m.long <- hwalpha.linearity.m.long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
hwalpha.linearity.m.long$class <- factor(hwalpha.linearity.m.long$class,
                                         levels = c("stlar","nn", "theta", "tbats",  "snaive", "rw", "ETS_NTNS"))
m42d <- hwalpha.linearity.m.long %>%
  ggplot(aes(y = hwalpha, x = linearity, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.2,100),
                       limits=c(0,0.2), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))

m12d/m22d/m32d/m42d

## ---- thetapdp
load("data/HPCfiles/stability.trend.m.rda")
colNamesds <-colnames(stability.trend.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("stability", "trend"))
stability.trend.m <- stability.trend.m[, names(stability.trend.m) %in% keepm]
stability.trend.m.long <- gather(stability.trend.m, class, probability, "theta", factor_key = TRUE)
stability.trend.m.long$class <- factor(stability.trend.m.long$class,levels = c("theta"))
theta1 <- stability.trend.m.long %>%
  ggplot(aes(y = stability, x = trend, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.13,100),
                       limits=c(0,0.13), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))+ theme(legend.position = "none")

load("data/HPCfiles/stability.N.m.rda")
colNamesds <-colnames(stability.N.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("stability", "N"))
stability.N.m <- stability.N.m[, names(stability.N.m) %in% keepm]
stability.N.m.long <- gather(stability.N.m, class, probability, "theta", factor_key = TRUE)
stability.N.m.long$class <- factor(stability.N.m.long$class,levels = c( "theta"))
theta2 <- stability.N.m.long %>%
  ggplot(aes(y = stability, x = N, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.13,100),
                       limits=c(0,0.13), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))+ theme(legend.position = "none")

load("data/HPCfiles/stability.diff1y_acf1.m.rda")
colNamesds <-colnames(stability.diff1y_acf1.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("stability", "diff1y_acf1"))
stability.diff1y_acf1.m <- stability.diff1y_acf1.m[, names(stability.diff1y_acf1.m) %in% keepm]
stability.diff1y_acf1.m.long <- gather(stability.diff1y_acf1.m, class, probability, "theta", factor_key = TRUE)
stability.diff1y_acf1.m.long$class <- factor(stability.diff1y_acf1.m.long$class,levels = c( "theta"))
theta3 <- stability.diff1y_acf1.m.long %>%
  ggplot(aes(y = stability, x = diff1y_acf1, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.13,100),
                       limits=c(0,0.13), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))

theta1|theta2|theta3

## ----h2dpdp
load("data/HPCfiles/linearity.sediff_seacf1.h.rda")
colNamesss <- colnames(linearity.sediff_seacf1.h)[28:37]

keep.modelnames <- c("tbats", "nn", "stlar", "rw", "theta", "mstlarima")
keepd <- c(keep.modelnames, c("linearity", "sediff_seacf1"))
linearity.sediff_seacf1.h <- linearity.sediff_seacf1.h[, names(linearity.sediff_seacf1.h) %in% keepd]
linearity.sediff_seacf1.h.long <- gather(linearity.sediff_seacf1.h, class, probability, "mstlarima":"theta", factor_key = TRUE)
linearity.sediff_seacf1.h.long$class <- factor(linearity.sediff_seacf1.h.long$class,
                                               levels = c("tbats", "nn", "stlar", "rw",  "mstlarima", "theta"))

linearity.sediff_seacf1.h.long %>%
  ggplot(aes(x = linearity, y = sediff_seacf1, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=6) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.2,100),
                       limits=c(0,0.2))+
  theme(strip.text.x = element_text(size = 10))+ylab("sediff_seacf1")


## ----pcayearly
library(seer)
load("data/HPCfiles/yearlym4_votes.rda")
models.weights <- fforms_ensemble(yearlym4_votes, threshold = 0.4)
a <- lapply(models.weights, function(temp){length(temp)})
aunlist <- unlist(a)
features_M4Y <- readRDS("~/PhD_journey/fforms/data/HPCfiles/features_M4Y.rds")
calculate_pca <- function(feature_dataset){
  pcaY_cal <- prcomp(feature_dataset, center = TRUE, scale = TRUE)
  PCAresults <- data.frame(PC1 = pcaY_cal$x[, 1], 
                           PC2 = pcaY_cal$x[, 2], 
                           PC3 = pcaY_cal$x[, 3])
  return(list(prcomp_out =pcaY_cal,pca_components = PCAresults))
}
pca_ref_calc <- calculate_pca(features_M4Y)
df <- pca_ref_calc$pca_components
highlight <- which(aunlist==1)
df2 <- cbind(df, features_M4Y)
df3 <- data.frame(name = as.factor(unlist(lapply(models.weights[highlight], function(temp){names(temp)}))),
                  highlight = highlight,
                  PC1=df$PC1[highlight],
                  PC2=df$PC1[highlight])

pca1y <- ggplot(df,aes(x=PC1,y=PC2)) + 
  geom_point(colour="grey", alpha=0.5) +
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2, color=df3$name))+labs(color="Model")+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position="bottom", legend.text=element_text(size=15), legend.title = element_blank(), aspect.ratio=1)

pca2y <- ggplot(df2,aes(x=PC1,y=PC2, color=trend)) + 
  geom_point(alpha=0.5)+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red")+
  theme(legend.position="bottom", legend.text=element_text(size=15),aspect.ratio=1)

pca3y <- ggplot(df2,aes(x=PC1,y=PC2, color=beta)) + 
  geom_point(alpha=0.5)+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red")+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1)


pca5y <- ggplot(df2,aes(x=PC1,y=PC2, color=diff1y_acf5)) + 
  geom_point(alpha=0.5)+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red")+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1)

pca1y|pca2y|pca3y|pca5y

## ----pcamonthly
monthlym4_votes <- readRDS("data/HPCfiles/monthlym4_votes.rds")
models.weights.monthly <- fforms_ensemble(monthlym4_votes, threshold = 0.4)
a <- lapply(models.weights.monthly, function(temp){length(temp)})
aunlist <- unlist(a)
features_M4M <- readRDS("data/HPCfiles/features_M4M.rds")
calculate_pca <- function(feature_dataset){
  pcaY_cal <- prcomp(feature_dataset, center = TRUE, scale = TRUE)
  PCAresults <- data.frame(PC1 = pcaY_cal$x[, 1], 
                           PC2 = pcaY_cal$x[, 2], 
                           PC3 = pcaY_cal$x[, 3])
  return(list(prcomp_out =pcaY_cal,pca_components = PCAresults))
}
pca_ref_calc <- calculate_pca(features_M4M)
df <- pca_ref_calc$pca_components
highlight <- which(aunlist==1)
df2 <- cbind(df, features_M4M)
df3 <- data.frame(name = as.factor(unlist(lapply(models.weights.monthly[highlight], function(temp){names(temp)}))),
                  highlight = highlight,
                  PC1=df$PC1[highlight],
                  PC2=df$PC1[highlight])
df3 <- df3 %>%
  mutate(name = recode(name, "ARIMA"="other", "ETS-dampedtrendseasonal"="other", 
                        "ETS-seasonal"="other", "ETS-trendseasonal"="other",
                        "nn"="nn", "rw"="other", 
                        "rwd"="other","SARIMA"="SARIMA"  ,"snaive"="other", "stlar"="stlar",
                        "tbats"="other", "theta"="other","wn"="other"))
df3$name <- factor(df3$name, levels = c("stlar", "SARIMA", "nn", "other"))

pca1m <- ggplot(df,aes(x=PC1,y=PC2)) + 
  geom_point(colour="grey") +
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2, color=df3$name))+labs(color="Model")+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1, legend.title = element_blank())

pca2m <- ggplot(df2,aes(x=PC1,y=PC2, color=beta)) + ##important
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red", shape=1)+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1)

pca3m <- ggplot(df2,aes(x=PC1,y=PC2, color=sediff_acf5)) + 
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red", shape=1)+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1)

pca4m <- ggplot(df2,aes(x=PC1,y=PC2, color=entropy)) + 
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red", shape=1)+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1)

pca1m|pca2m|pca3m|pca4m


## ----pcahourly
load("data/HPCfiles/hourlym4_votes.rda")
models.weights.hourly <- fforms_ensemble(hourlym4_votes, threshold = 0.4)
a <- lapply(models.weights.hourly, function(temp){length(temp)})
aunlist <- unlist(a)
features_M4H <- readRDS("data/HPCfiles/features_M4H.rds")
calculate_pca <- function(feature_dataset){
  pcaY_cal <- prcomp(feature_dataset, center = TRUE, scale = TRUE)
  PCAresults <- data.frame(PC1 = pcaY_cal$x[, 1], 
                           PC2 = pcaY_cal$x[, 2], 
                           PC3 = pcaY_cal$x[, 3])
  return(list(prcomp_out =pcaY_cal,pca_components = PCAresults))
}
pca_ref_calc <- calculate_pca(features_M4H)
df <- pca_ref_calc$pca_components
highlight <- which(aunlist==1)
df2 <- cbind(df, features_M4H)
df3 <- data.frame(name = as.factor(unlist(lapply(models.weights.hourly[highlight], function(temp){names(temp)}))),
                  highlight = highlight,
                  PC1=df$PC1[highlight],
                  PC2=df$PC1[highlight])

df3 <- df3 %>%
  mutate(name = recode(name, "mstlarima"="mstlarima", "mstlets"="mstlets", 
                       "nn"="nn", "snaive"="other",
                       "stlar"="other", "tbats"="other"))
df3$name <- factor(df3$name, levels = c("mstlarima", "mstlets", "nn", "other"))

pca1h <- ggplot(df,aes(x=PC1,y=PC2)) + 
  geom_point(colour="grey") +
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2, color=df3$name))+labs(color="Model")+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1, legend.title = element_blank())

names(df2)[names(df2) == 'N'] <- 'T'
pca2h <- ggplot(df2,aes(x=PC1,y=PC2, color=T)) + 
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red", shape=1)+
  theme(legend.position="bottom", legend.text=element_text(size=6), aspect.ratio=1)

pca3h <- ggplot(df2,aes(x=PC1,y=PC2, color=entropy)) + 
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red", shape=1)+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1)

names(df2)[names(df2) == 'seasonal_strength2'] <- 'seasonal_W'
pca4h <- ggplot(df2,aes(x=PC1,y=PC2, color=seasonal_W)) + 
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red", shape=1)+
  theme(legend.position="bottom", legend.text=element_text(size=15), aspect.ratio=1)



pca1h|pca2h|pca3h|pca4h

