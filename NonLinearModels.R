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

### This can be done in a more direct way...

fita<-lm(age~I(age^3)+I(age^4),data=Wage)
summary(fita)

### I() is a wrapper function, which is necessary to allow the correct operation to be interpreted by the lm function.
### The coefficients are different from the first method, yet the fits are identical.

### The orthogonal polynomials from the simple method show us that we can seperately test for each coefficient.
### The summary of the fit shows that the linear, quadratic, and cubic terms are significant, but not the quartic.

### This only works for linear regression, and if there is a single predictor, use anova().

fita<-lm(wage~education,data=Wage)
fitb<-lm(wage~education+age,data=Wage)
fitc<-lm(wage~education+poly(age,2),data=Wage)
fitd<-lm(wage~education+poly(age,3),data=Wage)
anova(fita,fitb,fitc,fitd)

### Polynomial logistic regression.
### Now, we fit a logistic regression model to a binary response variable, constructed from 'wage'. We encode big earners (>250k) as 1, else 0.





