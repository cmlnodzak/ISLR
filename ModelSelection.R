library(ISLR)
summary(Hitters)

### remove missing values
Hitters<-na.omit(Hitters)
with(Hitters,sum(is.na(Salary)))


### Best subset regression model selection with leaps library.
library(leaps)
regfit.full<-regsubsets(Salary~., data=Hitters)
summary(regfit.full)


regfit.full<-regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary<-summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)

points(10,reg.summary$cp[10],pch=20,col="red")

### regsubsets objects have plot method defined

plot(regfit.full,scale="Cp")
coef(regfit.full,10)

### Forward stepwise selection with regsubsets objects

regfit.fwd<-regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
plot(regfit.fwd,scale="Cp")


### Model selection with a validation set
### need to make a training and test set to choose a subset model.
dim(Hitters)
set.seed(1)
train<-sample(seq(263),180,replace=FALSE)
train
regfit.fwd<-regsubsets(Salary~.,data=Hitters[train,],nvmax=19,method="forward")

### Now we have 19 models, we can make predictions on the test set.
### construct empty vectors to record errors.
### Since no predict method exists for regsubsets, we will define one.

val.errors<-rep(NA,19)
x.test<-model.matrix(Salary~.,data=Hitters[-train,])
for(i in 1:19){
	coefi<-coef(regfit.fwd,id=i)
	pred<-x.test[,names(coefi)]%*%coefi
	val.errors[i]<-mean((Hitters$Salary[-train]-pred)^2)
}
plot(sqrt(val.errors),ylab="Root MSE",ylim=c(300,400),pch=19,type="b")
legend("topright",legend=c("Training","Validation"),col=c("blue","black"),pch=19)

predict.regsubsets<-function(object,newdata,i,...){
	form<-as.formula(object$call[[2]])
	mat<-model.matrix(form,newdata)
	coefi<-coef(object,id=i)
	mat[,names(coef)]%*%coefi
}

### The training error goes down monotonically as the model gets bigger, but not so for the validation error.


### Model Selection by Cross Validation
set.seed(11)
folds=sample(rep(1:10,length=nrow(Hitters)))
table(folds)
cv.errors=matrix(NA,10,19)
for(k in 1:10){
	best.fit=regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forwards")
	for(i in 1:19){
	pred=predict(best.fit,Hitters[folds==k,],id=i)
	cv.errors[k,i]=mean((Hitters$Salary[folds==k]-pred)^2)
	}
}

rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")






