
#Hierarchical Clustering

data = mtcars
d<-dist(data,method='euclidean')
d
distances = as.matrix(d)


fit<-hclust(d,method='single')
plot(fit)
fit<-hclust(d,method='ward.D')
plot(fit)

groups <- cutree(fit,k=5)
groups


rect.hclust(fit,k=4,border='blue')


#K-medoids Clustering


#Spectral Clustering

library(kernlab)

x <- rbind(cbind(rnorm(10), rnorm(10,0,0.5)), cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
specclu = specc(x, centers=2)
plot(x, col=specclu)
##Manual Calculation of Spectral clustering
distances = as.matrix(dist(x))
W = exp(-distances^2)
G = diag(rowSums(W))
L = G - W
eig = eigen(L)
km2 = kmeans(eig$vectors[,29],
             centers=2)
plot(x,xlab="",ylab="",
     col=c("red","black","blue")[km2$cluster],
     main="spectral clustering")


#Soft Clustering

library(cluster)
set.seed(123)
# Load the data
data("USArrests")
# Subset of USArrests
ss <- sample(1:50, 20)
df <- scale(USArrests)
# Compute fuzzy clustering
res.fanny <- fanny(df, 3,metric='manhattan')
# Cluster plot using fviz_cluster()
# You can use also : clusplot(res.fanny)
library(factoextra)
fviz_cluster(res.fanny, frame.type = "norm",
             frame.level = 0.68)
fviz_silhouette(res.fanny, label = TRUE)

library(e1071)
res.cmeans = cmeans(df, 4)
fviz_cluster(list(data = df, cluster=res.cmeans$cluster), ellipse.type = "norm",ellipse.level = 0.50)
