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







