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




