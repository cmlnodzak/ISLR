library(MASS)
library(ISLR)

#### Perform simple linear regression 

names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1<-lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confit(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")


### Perform multiple linear regression (no interactions)
fit2<-lm(medv~lstat+age, data=Boston)
summary(fit2)

### Perform multiple linear regression (all predictors)
fit3<-lm(medv~., data=Boston)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)

fit4<-update(fit3,~.-age-indus)
summary(fit4)

### Multiple linear regression with nonlinear terms and interactions

fit5<-lm(medv~lstat*age,data=Boston)
summary(fit5)

fit6<-lm(medv~lstat + I(lstat^2),data=Boston)
summary(fit6)

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
fit7<-lm(medv~ploy(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)

### Qualitative predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1<-lm(Sales~.,+Income:Advertising+Age:Price,data=Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

### Define an R function
regplot=function(x,y){
	fit<-lm(x~y)
	plot(x,y)
	abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Seats)

### Redefine the R function to take optional args
regplot=function(x,y,...){
        fit<-lm(x~y)
        plot(x,y,...)
        abline(fit,col="red")
}

regplot(Price,Sales,xlab="Price",ylab="Sales",col=blue,pch=20)







