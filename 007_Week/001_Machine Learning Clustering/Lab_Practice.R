
cereals = read.csv("/Users/suresh/Desktop/Insofe/007_Week/001_Machine Learning Clustering/Cereals.csv",header=T)
str(cereals)


table(cereals$shelf)

#cereals$shelf = as.factor(cereals$shelf)

apply(cereals,2,FUN=function(x){sum(is.na(x))})

library(DMwR)

cereals = knnImputation(cereals,5)

library(vegan)


cereals[,sapply(cereals,is.numeric)] = decostand(cereals[,sapply(cereals,is.numeric)],'standardize')

fit<-kmeans(cereals[,sapply(cereals,is.numeric)],centers=5)

summary(fit)

fit$withinss
fit$centers
fit$cluster



wss <- 0 
for (i in 1:15)
{ 
  set.seed(1234) 
  wss[i] <- sum(kmeans(cereals[,sapply(cereals,is.numeric)],centers=i)$withinss) 
}


plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Total within-cluster sum of squares")

set.seed(1234)
fit <- kmeans(cereals[,sapply(cereals,is.numeric)], centers = 6) 
#With-in sum of squares in each cluster 
fit$withinss 
sum(fit$withinss)
fit$cluster
cereals$Cluster = fit$cluster
table(cereals$Cluster)
library(stats)
cereals[order(cereals$Cluster),c('name','Cluster')]



