require(ISLR)
attach(Wage)


### Polynomials with a single predictor

fit=lm(wage~poly(age,4),data=Wage)
summary(fit)

### the poly() function generates a basis of orthogonal polynomials. 

agelims<-range(age)
age.grid<-seq(from=agelime[1],to=agelims[2])
preds<-predict(fit,newdata=list)
se.bands<-cbind(preds$fit + 2 * preds$se,preds$fit -2 * preds$se)
plot(age,wage,col="darkgrey")
lines(age.grid,preds$fit,lwd-2,col="blue")
matlines(age.grid,se.bands,col="blue",lty=2)



