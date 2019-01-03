### Clustering Analyses

### Principal Components Analysis
### use the "USArrests" built-in R dataset

dimnames(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2,var)

### From this, we see that "Assault" has a much larger variance than the other variables.
### This would dominate the principal components, so we ought to standardize the variables when we perform PCA.

pca.out<-prcomp(USArrests,scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out,scale=0)


### K-Means Clustering
### K-means works in any dimension, but it is simple to demonstrate in two, so we can make plots easily.

### First, make data in clusters by shifting the means of the points.

set.seed(101)
x<-matrix(rnorm(100*2),100,2)
xmean<-matrix(rnorm(8,sd=4),4,2)
which<-sample(1:4,100,replace=TRUE)
x<-x+mean[which,]
plot(x,col=which,pch=19)

km.out<-kmeans(x,4,nstart=15)
km.out
plot(x,col=km.out$cluster,cex=3,pch-1,lwd=2)
points(x,col=which,pch=1
points(x,col=c(4,3,2,1)[which],pch=19)


### Hierarchichal clustering
### Now we will use the same data as above but with the hierarchical clustering method.
### method refers to differences in linkage rule, distance is euclidean.

hc.complete<-hclust(dist(x),method="complete")
plot(hc.complete)
hc.single<-hclust(dist(x),method="single")
plot(hc.single)
hc.average<-hclust(dist(x),method="average")
plot(hc.average)

### We can then compare the hierarchical clustering method with the actual clusters in the data.
### The function "cutree" can be used to cut the tree at level four.
### This returns a vector of numbers from 1 to 4, saying which branch each observation belongs to.
### The "table" function can tell us how well they match.

hc.cut<-cutree(hc.complete,4)
table(hc.cut,which)
table(hc.cut,km.cluster$cluster)

plot(hc.complete,labels=which)








