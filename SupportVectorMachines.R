### This code will demonstrate the support vector machine (SVM).
### We begin in low dimensional space, in order to view the data.
### The Linear SVM classifier with simulated data.


set.seed(10111)
x<-matrix(rnorm(40),20,2)
y<-rep(c(-1,1),c(10,10))
x[y==1,]<-x[y==1,]+1
plot(x,col=y+3,pch=19)

### Now load the "e1071" package, which contains the "SVM"function. We use this function to compute the fit and compute a cost parameter. The cost parameter is a tuning parameter.

library(e1071)
dat<-data.frame(x,y=as.factor(y))
svmfit<-svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)
print(svmfit)
plot(svmfit,dat)


### The plot function here is crude, and plots X2 on the horizontal axis (unlike what R normally does for a matrix). 
### To make our own plot, we need to make a function to provide a grid of values for X1 and X2.
### We use the function expand.grid, and produce the coordinates of n*n points on a lattice covering the domain of "x".
### Having made the lattice, we then make a prediction at each point of the lattice.
### We then plot the lattice, color-coded according to the classification and visualize the decision boundary.

### The support points, (i.e. points on the margin, or on the wrong side of the margin) are indexed in the "$index" component of the fit.

make.grid<-function(x,n=75){
	grange<-apply(x,2,range)
	x1<-seq(from=grange[1,1],to=grange[2,1],length=n)
	x2<-seq(from=grange[1,2],to=grange[2,2],length=n)
	expand.grid(X1=x1,X2=x2)
}

