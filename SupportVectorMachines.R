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





