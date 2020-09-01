## Computation with iheatmapr
## https://docs.ropensci.org/iheatmapr/articles/full_vignettes/iheatmapr.html

## ---- package
library(iheatmapr)
library(viridis)

## ---- yearlyiheatmapr
load("~/PhD_journey/fforms/data/HPCfiles/yearlym4_votes.rda")

## Method 1
iheatmapr::main_heatmap(yearlym4_votes) %>%
  add_col_clustering(method="kmeans", k=4) %>%
  add_col_clustering() %>%
  add_row_clustering(method="kmeans", k = 5)

# good
## nn, rw, theta/ ARMA, ETS-dampedtrend, ETS-notrendnoseasonal, wn/ rwd/ ARIMA, ETS-trend
## cluster: rwd/ rwd, ETS-t, ARIMA/ rwd, ETS-trend/ rwd/ETS-trend, ARIMA

set.seed(7) #7, 700
Classes <- colnames(yearlym4_votes)
iheatmapr::main_heatmap(yearlym4_votes) %>%
  add_col_clustering(method="kmeans", k=4) %>%
  add_row_clustering(method="kmeans", k = 5) %>%
  add_col_labels()

yheatmap <- iheatmapr::main_heatmap(yearlym4_votes, name="Vote probability") %>%
  add_col_clustering(method="kmeans", k=4) %>%
  add_row_clustering(method="kmeans", k = 5) %>%
  add_col_labels() %>%
  add_col_clustering() %>%
  add_row_title("Time series (each row corresponds to a single time series)") %>%
  add_col_title("Class", side="top") 


yheatmap@yaxes@listData$y@order # tail of this is hown as the first row in the visualisation
yheatmap@xaxes@listData$x@order 
colnames(yearlym4_votes)
 # add_col_annotation(annotation) 

yheatmap@plots@listData$`Row<br>Clusters`@data # row clusters

features_M4Y <- readRDS("~/PhD_journey/fforms/data/HPCfiles/features_M4Y.rds")
features_M4Y <- as.matrix(features_M4Y)
iheatmapr::main_heatmap(yearlym4_votes) %>%
  add_col_clustering(method="kmeans", k=4) %>%
  add_row_clustering(method="kmeans", k = 5)  

## Method 2

iheatmap(yearlym4_votes, row_k=5, col_k=4,
         cluster_rows = "kmeans", cluster_cols = "kmeans") %>% 
  add_col_clustering(method="kmeans", k=4) %>%
  add_row_clustering(method="kmeans", k = 5)

## good
iheatmap(yearlym4_votes) %>% 
  add_col_clustering(method="kmeans", k=4) %>%
  add_row_clustering(method="kmeans", k = 5)

iheatmap(yearlym4_votes, cluster_cols = "kmean", col_k=2, row_k=5)

iheatmapr::iheatmap(yearlym4_votes, cluster_cols = "kmean", col_k=3)

iheatmapr::iheatmap(yearlym4_votes) %>%
  add_col_clustering("kmeans", k=4)

iheatmapr::iheatmap(yearlym4_votes, cluster_cols = "kmean", col_k=4) %>%
   add_col_clustering() 

iheatmapr::iheatmap(yearlym4_votes, cluster_cols = "kmean",
                    cluster_rows = "hclust", col_k=2, row_k=5) 

## ---- quarterlyiheatmapr
load("~/PhD_journey/fforms/data/HPCfiles/quarterlym4_votes.rda")
clonamesq <- colnames(quarterlym4_votes)
iheatmap(quarterlym4_votes) %>% 
  add_col_clustering(method = "kmeans", k = 4) %>%
  add_row_clustering(method = "kmeans", k = 5)




## ---- monthlyiheatmapr
monthlym4_votes <- readRDS("~/PhD_journey/fforms/data/HPCfiles/monthlym4_votes.rds")
## Comment on the column cluster
iheatmap(monthlym4_votes) %>% 
  add_col_clustering(method="kmeans", k=4) %>%
  add_row_clustering(method="kmeans", k = 5)

## ---- weeklyiheatmapr

## ---- dailyiheatmapr

## ---- hourlyiheatmapr