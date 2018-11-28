### The following code attempts to make classfications on stock market data using various methods.

require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket

pairs(Smarket,col=Smarket$Direction)

### Use glm to perform logistic regression
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)

summary(glm.fit)
glm.probs<-predict(glm.fit,type="response")
glm.probs[1:5]
glm.pred<-ifelse(glm.probs>0.5, "Up","Down")

attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)

### Make a training and test set
train<-Year<2005
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs<-predict(glm.fit,newdata=Smarket[!train,],year="response")
glm.pred<-ifelse(glm.probs > 0.5, "Up","Down")
Direction.2005<-Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)


### Fit a smaller model
glm.fit<-glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs<-predict(glm.fit,newdata=Smarket[!train,],type="resopnse")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(76+106)

### Perform Linear Discriminant Analysis
require(MASS)
lda.fit<-lda(Direction
lda.fit
plot(lda.fit)
Smarket.2005<-subset(Smarket,Year==2005)
lda.pred<-predict(lda.fit,Smarket.2005)
lda.pred[1:5]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

### Perfrom K-Nearest Neighbors
library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred<-knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])








