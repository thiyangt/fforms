## Packages
library(iheatmapr)
library(tidyverse)
library(plotly)

## Yearly
load("~/PhD_journey/fforms/data/HPCfiles/yearlym4_votes.rda")
load("data/ym4_true_classlabels.rda")
set.seed(10011)# 1, 10011
yheatmap <- iheatmapr::main_heatmap(yearlym4_votes, name="Vote probability") %>%
  add_row_clustering(method="kmeans", k = 5) %>%
  add_col_clustering(method="kmeans", k=4) %>%
  add_col_labels(size=0.05) %>%
  add_row_title("Time series (each row corresponds to a single time series)") %>%
  add_col_title("Class", side="top") 
yheatmap

row_cluster_yearly <- yheatmap@plots@listData$`Row<br>Clusters`@data 
yearly_m4_clus <- data.frame(cluster=row_cluster_yearly, trueLabel=ym4_true_classlabels)
head(yearly_m4_clus)

yearly_m4_clus$trueLabel <- factor(yearly_m4_clus$trueLabel,
                                      c("nn", "rw", "theta", "ARMA/AR/MA",
                                        "ETS-dampedtrend", "ETS-notrendnoseasonal",
                                        "wn", "rwd", "ARIMA", "ETS-trend"))

yearly_m4_clus$cluster <- factor(yearly_m4_clus$cluster,
                                   c(5, 4, 3, 2, 1))

py <- ggplot(yearly_m4_clus, 
       aes(trueLabel, fill=cluster)) + 
  geom_bar() + 
  facet_wrap(~cluster, ncol=1)+
 scale_fill_manual(values=rev(c("#a6cee3", "#1f78b4",
    "#b2df8a", "#33a02c", "#fb9a99"))) + 
  theme(axis.text.x = element_text(angle = 90)) + xlab("Class label (Best forecast model)") + 
  ggtitle("Composition of clusters by class labels (best forecast model)")

ggplotly(py)


## PCA
#head(df)
#df$class <- yearly_m4_clus$cluster 

#plot_ly(x=df$PC1, y=df$PC2, z=df$PC3, type="scatter3d", 
#        mode="markers", color=df$class, opacity=0.7, size=18,
#        colors    = ~c("#1b9e77","#d95f02",
#                       "#7570b3", "#e7298a", "#66a61e"))
