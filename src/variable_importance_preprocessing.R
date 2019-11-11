# ---- packages
library(here)
library(tidyverse)
library(reshape2)
library(data.table)

# ---- pre-processing variable importance plot
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
    feature == "alpha" ~ 8,
    feature == "diff2y_acf5"  ~ 9,
    feature == "e_acf1"  ~ 10,
    feature == "sediff_acf5"  ~ 11,
    feature == "diff1y_acf5" ~ 12,
    feature == "diff1y_pacf5"  ~ 13,
    feature == "diff2y_acf1"  ~ 14,
    feature == "hwalpha" ~ 15,
    feature == "seas_pacf"  ~ 16,
    feature == "sediff_acf1"  ~ 17,
    feature == "spikiness"  ~ 18,
    feature == "y_acf1" ~ 19,
    feature == "beta"  ~ 20,
    feature == "hwgamma"  ~ 21,
    feature == "lmres_acf1"  ~ 22,
    feature == "seasonal_strength2" ~ 23,
    feature == "sediff_seacf1"  ~ 24,
    feature == "curvature"  ~ 25,
    feature == "entropy"  ~ 26,
    feature == "diff1y_acf1" ~ 27,
    feature == "stability"  ~ 28,
    feature == "ur_pp"  ~ 29,
    feature == "y_pacf5"  ~ 30,
    feature == "N" ~ 31,
    feature == "seasonal_strength1"  ~ 32,
    feature == "linearity"  ~ 33,
    feature == "trend" ~ 34,
    feature == "seasonality"  ~ 35))
## Reserve a row for each entry
vi.fforms <- data.table(vi.fforms)
vi.fforms <- vi.fforms[, strsplit(as.character(ranks), "/"), by=list(classnew, featurenew)]
vi.fforms[, shift:=(1:(.N))/.N - 1/(2 * .N) - 1/2, by=list(classnew, featurenew)]
vi.fforms[, height:=1/.N, by=list(classnew, featurenew)]

#ggplot(vi.fforms, aes(y = feature, x = class, fill= ranks, label = ranks)) + 
#  geom_raster() +geom_text(col = "black", size=2)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))




ggplot(vi.fforms, aes(x = classnew,y=featurenew+shift, fill=V1, height=height)) + 
  geom_tile(color="black") + scale_fill_manual(na.value="white", values=c("#1b9e77", "#d95f02", "#7570b3"),  name = "Frequency \ncombinations")+
  scale_x_discrete(limit=c("wn","rwd", "rw", "nn", "theta", "ARMA.AR.MA","ARIMA",  "ETS.notrendnoseasonal", "ETS.dampedtrend",
                            "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal",
                            "ETS.seasonal", "SARIMA",  
                            "stlar", "tbats","snaive","mstlarima", "mstlets"))+
  scale_y_discrete(limit=c(
    "diff2y_pacf5", "hurst", "hwbeta", "lumpiness", "nonlinearity",
     "ur_kpss", "y_acf5", "alpha", "diff2y_acf5", "e_acf1", "sediff_acf5",
    "diff1y_acf5", "diff1y_pacf5", "diff2y_acf1", "hwalpha", "seas_pacf",
    "sediff_acf1", "spikiness", "y_acf1", "beta", "hwgamma", "lmres_acf1",
    "seasonal_strength2", "sediff_seacf1", "curvature", "entropy",
    "diff1y_acf1", "stability", "ur_pp", "y_pacf5", "N", "seasonal_strength1",
    "linearity", "trend", "seasonality"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=10))

#https://stackoverflow.com/questions/15921799/draw-lines-around-specific-areas-in-geom-tile

library(tidyverse)

df1 = data.frame(x = c(1,2,1,2), y = c(1,1,2,2), COL = c(0, 2, 4, 5))
df2 = data.frame(x = c(1,2,1,2), y = c(1,1,2,2), COL = c(6, 5, 8, 12))
df3 = data.frame(x = c(1,2,1,2), y = c(1,1,2,2), COL = c(2, 2, 4, 5))

bind_rows(df1, df2, df3, .id = "Month") %>% 
  ggplot(aes(x, y, fill = COL)) + 
  geom_tile(color="black") +
  theme_classic() +
  scale_fill_distiller('pr',palette='Spectral') +
  facet_wrap(vars(Month), ncol = 4, nrow = 3) +
  theme(legend.position = "bottom")


