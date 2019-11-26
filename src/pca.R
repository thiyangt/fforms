library(seer)
load("~/PhD_journey/fforms/data/HPCfiles/yearlym4_votes.rda")
models.weights <- fforms_ensemble(yearlym4_votes, threshold = 0.6)
a <- lapply(models.weights, function(temp){length(temp)})
aunlist <- unlist(a)
table(aunlist)
which(aunlist==1)

features_M4Y <- readRDS("~/PhD_journey/fforms/data/HPCfiles/features_M4Y.rds")

calculate_pca <- function(feature_dataset){
  pcaY_cal <- prcomp(feature_dataset, center = TRUE, scale = TRUE)
  PCAresults <- data.frame(PC1 = pcaY_cal$x[, 1], 
                           PC2 = pcaY_cal$x[, 2], 
                           PC3 = pcaY_cal$x[, 3])
  return(list(prcomp_out =pcaY_cal,pca_components = PCAresults))
}

pca_projection <- function(prcomp_out, data_to_project){
  
  PCA <- scale(data_to_project, prcomp_out$center, prcomp_out$scale) %*% prcomp_out$rotation
  pca_projected <- data.frame(PC1=PCA[,1], PC2=PCA[,2], PC3=PCA[,3]) 
  return(pca_projected)
  
}

pca_ref_calc <- calculate_pca(features_M4Y)
df <- pca_ref_calc$pca_components
highlight <- which(aunlist==1)
df2 <- cbind(df, features_M4Y)
df2$count <- as.factor(aunlist)
df3 <- data.frame(name = as.factor(unlist(lapply(models.weights[highlight], function(temp){names(temp)}))),
                  highlight = highlight,
                  PC1=df$PC1[highlight],
                  PC2=df$PC1[highlight])

ggplot(df,aes(x=PC1,y=PC2)) + 
  geom_point(colour="grey") +
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2, color=df3$name))




library(ggplot2)
ggplot(df2,aes(x=PC1,y=PC2, color=count)) + 
  geom_point(shape=1) 


library(ggplot2)
ggplot(df,aes(x=PC1,y=PC2)) + 
  geom_point(colour="grey") +
  geom_point(data=df3[highlight, ], aes(x=PC1, y=PC2), colour="red")

ggplot(df2,aes(x=PC1,y=PC2, color=y_pacf5)) + 
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red")

ggplot(df2,aes(x=PC1,y=PC2, color=diff1y_acf1)) + 
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red")

ggplot(df2,aes(x=PC1,y=PC2, color=diff1y_acf5)) + 
  geom_point()+
  geom_point(data=df[highlight, ], aes(x=PC1, y=PC2), colour="red")

## Monthly