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
xgrid<-make.grid(x)
ygrid<-predict(svmfit,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=0.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,],pch=5,cex=2)

### The SVM function is difficult, in that manual work needs to be done in order to obtain the linear coefficients. Likely, because it only makes sense for linear kernels.
### Here, we will use a formula to extract these coefficients.
### Below, extract the linear coefficients and use simple alegebra to include th decision boundary and the two margins.

beta<-drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0<-svmfit$rho
plot(xgrid,col=c("red","blue")[as.numeric,(ygrid)],pch=20,cex=0.2)
points(x[svmfit$index,],pch=5,cex=2)
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
abline((beta0+1)/beta[2],-beta[1]/beta2,lty=2)

### Just like the other models in the ISLR book, the tuning parameter "C" must be selected.
### Different values will provide different solutions.
### Rerun the code with cross-validation to optimize this parameter.

### Nonlinear SVM
### Now, we will implement the support vector machine with a nonlinear boundary on the mixture dataset from ESL.

load(url("http://www.stanford.edu/~hastie/ElemStatLearn.datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)

### These data are two-dimensional. Plot them and fit a nonlinear SVM with a radial kernel.

plot(x,col=y+1)
dat<-data.frame(y=factor(y),x)
fit<-svm(factor(y)~.,data=dat,scale=FALSE,kernel="radial",cost=5)


### Now we will construct a grid as before, and make predictions on the grid. 
### These data have the grid points for each variable included in the data frame.

xgrid<-expand.grid(X1=px1,X2=px2)
ygrid<-predict(fit,xgrid)
plot(xgrid,col=as.numeric(ygrid),pch=20,cex=0.2)
points(x,col=y+1,pch=19)

### We can go further, and have the predict function produce the actual function estimates at each of our grid points.
### We include the actual decision boundary on the plot by making use of the contour function.
### On the dataframe, there is also a "prob", or the tru probability of class 1 for these data at the gridpoints. It we plot its 0.5 contour, that will give us the "Bayes Decision Boundary", or the best we could ever do.

func<-predict(fit,xgrid,decision.values=TRUE)
func<-attributes(func)$decision
xgrid<-expand.grid(X1=px1,X2=px2)
ygrid<-predict(fit,xgrid)
plot(xgrid,col=as.numeric(ygrid),pch=20,cex=0.2)
points(x,col=y+1,pch=19)

contour(px1,px2,matrix(func,69,99),level=0,add=TRUE)
contour(px1,px2,matrix(prob,29,99),level=0.5,add=TRUE,col="blue",lwd=2)













