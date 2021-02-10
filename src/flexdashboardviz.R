## ---- packages
library(tidyverse)
library(iheatmapr)
library(here)
library(webshot)
library(plotly)
library(patchwork)

## ---- yearlydb
features_M4Y <-
  readRDS(here( "dashboard_data", "yearly", "features_M4Y.rds"))
load(here( "dashboard_data", "yearly", "yearlym4_votes.rda"))
set.seed(10011)# 1, 10011
yheatmap <- iheatmapr::main_heatmap(yearlym4_votes, name="Vote probability") %>%
  add_row_clustering(method="kmeans", k = 5) %>%
  add_col_clustering(method="kmeans", k=4) %>%
  add_col_labels(size=0.1, textangle = -90) %>%
  add_row_title("Time series (each row corresponds to a single time series)") %>%
  add_col_title("Class", side="bottom", buffer=0.2)  
yheatmap

## ---- classdisyearly
features_M4Y <- readRDS(here( "dashboard_data", "yearly", "features_M4Y.rds"))
load(here( "dashboard_data", "yearly", "yearlym4_votes.rda"))
load(here("dashboard_data", "yearly", "ym4_true_classlabels.rda"))
set.seed(10011)# 1, 10011
yheatmap <- iheatmapr::main_heatmap(yearlym4_votes, name="Vote probability") %>%
  add_row_clustering(method="kmeans", k = 5) %>%
  add_col_clustering(method="kmeans", k=4) %>%
  add_col_labels(size=0.1, textangle = -90) %>%
  add_row_title("Time series (each row corresponds to a single time series)") %>%
  add_col_title("Class", side="bottom", buffer=0.2)  

row_cluster_yearly <- yheatmap@plots@listData$`Row<br>Clusters`@data 
yearly_m4_clus <- data.frame(cluster=row_cluster_yearly)

yearly_m4_clus$trueLabel <- factor(ym4_true_classlabels,
                                   levels=c("nn", "rw", "theta", "ARMA/AR/MA","ETS-dampedtrend", "ETS-notrendnoseasonal",
                                            "wn", "rwd", "ARIMA", "ETS-trend"))

yearly_m4_clus$cluster <- factor(yearly_m4_clus$cluster,
                                 c(5, 4, 3, 2, 1))

pyclus <- ggplot(yearly_m4_clus, 
                 aes(trueLabel, fill=cluster)) + 
  geom_bar() + 
  facet_wrap(~cluster, ncol=1)+
  scale_fill_manual(values=rev(c("#1b9e77","#d95f02",
                                 "#7570b3", "#e7298a", "#66a61e"))) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("Count")+ 
  ggtitle("Composition of clusters by true class labels") + theme(axis.title.x = element_text(hjust=15)) + labs( 
    caption = "X-axis: true class lables, Panels: Row clusters") + ggtitle("A")

fclusy <- data.frame(trend = features_M4Y$trend,
                     diff1y_acf1 = features_M4Y$diff1y_acf1,
                     diff2y_acf1 = features_M4Y$diff2y_acf1,
                     entropy = features_M4Y$entropy,
                     cluster = yearly_m4_clus$cluster)
fclusy <- pivot_longer(fclusy, 1:4)

fclusplot <- ggplot(fclusy, aes(x=cluster, y=value, fill=cluster)) + 
  geom_violin() +
  geom_boxplot(width=0.1, color="grey", alpha=0.2, outlier.colour = NA)+
  facet_wrap(~name, ncol=1, scales = "free") + 
  scale_fill_manual(values=rev(c("#1b9e77","#d95f02","#7570b3", "#e7298a", "#66a61e"))) + ggtitle("B")
pyclus|fclusplot
