# superheat
library(superheat)

# Data
load("~/PhD_journey/fforms/data/HPCfiles/yearlym4_votes.rda")

# Code - yearly
superheat(yearlym4_votes,
          pretty.order.rows = TRUE,
          pretty.order.cols = TRUE,
          col.dendrogram = TRUE)
