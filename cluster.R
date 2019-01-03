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








