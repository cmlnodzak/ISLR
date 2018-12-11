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

fit<-glm(I(wage>250)~poly(age,3),data=Wage,family=binomial)
summary(fit)
preds<-predict(fit,list(age=age.grid)se=T)
se.bands=preds$fit + cbind(fit=0,lower=-2*preds$se,upper=2*preds$se)
se.bands[1:5,]

### Now we have the computations on the logit scale. To transform we need to apply the inverse logit mapping.

### p = [ e^(eta) / (1 + e^(eta) ]

### We can apply this transformation simultaneously to all three of the columns in se.bands.

prob.bands<-exp(se.bands)/(1+exp(se.bands))
maplot(age.grid,prob.bands,col="blue",lwd=c(2,1,1),lty=c(1,2,2),type="l",ylim=c(0,1))
points(jitter(age),I(wage>250)/10,pch="|",cex=0.5)

### Splines are more flexible than polynomials, but the idea is rather similar. Here we explore cubic splines.

require(splines)
fit<-lm(wage~bs(age,knots=c(25,40,60)),date=Wage)
plot(age,wage,col="darkgrey")
lines(age.grid,predict(fit,list(age=age.grid)),col="darkgreen",lwd=2)
abline(v=c(25,40,60),lty=2,col="darkgreen")

### The smoothing splines does not require knot selection, but it does have a smoothing parameter, which can be specified by the effective degrees of freedom.

fit<-smooth.spline(age,wage,df=16)
lines(fit,col="red",lwd=2)

### We can use Leave-one-out cross validation to select the smoothing parameter automatically.
fit<-smooth.spline


