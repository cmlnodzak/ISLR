require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower,data=Auto)

### Leave one out cross validation (LOOCV)
glm.fit<-glm(map~horsepower,data=Auto)
cv.glm(Auto,glm.fit$delta) # slow, does not use formula 5.2

### function to make use of LOOCV formula to speed up cv

loocv<-function(fit){
	h=lm.influence(fit)$h
	mean((residuals(fit)/(1-h))^2)
}

loocv(glm.fit)
cv.error<-rep(0,5)
degree<-1:5
for(d in degree){
	glm.fit<-glm(mpg~poly(horsepower,d),data=Auto)
	cv.error[d]<-loocv(glm.fit)
}
plot(degree,cv.error,type="b")

### 10-fold CV

cv.error10<-rep(0,5)
for(d in degree){
	glm.fit<-glm(mpg~poly(horsepower,d),data=Auto)
	cv.error10[d]<-cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")











