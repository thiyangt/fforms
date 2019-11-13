library(here)
library(tidyverse)
library(reshape2)

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
                           is.na(yearly.rank) == FALSE & is.na(monthly.rank) == FALSE & is.na(hourly.rank) == TRUE ~ "YM",
                           is.na(yearly.rank) == FALSE & is.na(monthly.rank) == TRUE & is.na(hourly.rank) == FALSE ~ "YH",
                           is.na(yearly.rank) == TRUE & is.na(monthly.rank) == FALSE & is.na(hourly.rank) == FALSE ~ "MH",
                           is.na(yearly.rank) == FALSE & is.na(monthly.rank) == FALSE & is.na(hourly.rank) == FALSE ~ "ALL"))

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


viwide <- reshape(vi.fforms, idvar = "feature", timevar = "class", direction = "wide")
rmt <- rowSums(!is.na(viwide))
#sort_rmt <- sort(rmt, decreasing = TRUE)
sort_rmt <- sort(rmt)
index <- as.numeric(names(sort_rmt))
featureOrder <- viwide$feature[index]
vi.fforms$feature <- factor(vi.fforms$feature, levels = featureOrder)
vi.fforms$ranks <- factor(vi.fforms$ranks, levels = c("ALL", "MH", "YH", "YM", "H", "M", "Y"))
vi.fforms$class <- factor(vi.fforms$class, levels = 
                            c("wn","rwd", "rw", "nn", "theta", "ARMA.AR.MA","ARIMA",  "ETS.notrendnoseasonal", "ETS.dampedtrend",
                              "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal",
                              "ETS.seasonal", "SARIMA",  
                              "stlar", "tbats","snaive","mstlarima", "mstlets"))
ggplot(vi.fforms, aes(y = feature, x = class, fill= ranks, label = ranks)) + 
  geom_tile(colour="grey20") +scale_fill_discrete(na.value="white", name = "Frequency \ncombinations")+ 
  geom_text(col = "black", size=8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=25))



