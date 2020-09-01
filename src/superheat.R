# superheat
library(superheat)

# Data
load("~/PhD_journey/fforms/data/HPCfiles/yearlym4_votes.rda")


# Code - yearly
superheat(yearlym4_votes,
          pretty.order.rows = TRUE,
          pretty.order.cols = TRUE,
          col.dendrogram = TRUE,
          row.dendrogram = TRUE)


# Data - quarterly
load("~/PhD_journey/fforms/data/HPCfiles/quarterlym4_votes.rda")
superheat(quarterlym4_votes,
          pretty.order.rows = TRUE,
          pretty.order.cols = TRUE,
          col.dendrogram = TRUE)

# Data - hourly

load("data/hourly/train_votes.rda")
superheat(hourlym4_votes,
          pretty.order.rows = TRUE,
          pretty.order.cols = TRUE,
          col.dendrogram = TRUE,
          n.clusters.rows =5)


