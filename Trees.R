### Decision Trees
### This section uses the Carseats dataset and the "tree" R package. 
### We begin by creating a binary response variable "High", and include it in the same dataframe.

require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
High<-ifelse(Sales<=8,"No","Yes")
Carseats<-data.frame(Carseats,High)

### Now we can fit a tree to the data, summarize it and plot it.
### Notice that we have to exclude "Sales" from the right hand side of the formula, as the response variable is derived from it.

tree.carseats<-tree(High~.-Sales,data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

tree.carseats

### Now we create a traning and test set with (250:150) split of 400 observations, grow the tree on the training set and evaluate its performance on the test set.

set.seed(1011)
train<-sample(1:nrow(Carseats),250)
tree.carseats<-tree(High~.-Sales,Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.pred<-predict(tree.carseats,Carseats[-train,]type="class")
with(Carseats[-train,],table(tree.pred,High))
(72+33)/150

### The tree was created and grown to full depth here, which makes it vulnerable to overfitting. To combat this we can prune it with cross validation.

cv.carseats<-cv.tree(tree.carseats,FUN=prune.misclass)
cv.carseats
plot(cv.carseats)
prune.carseats<-prine.misclass(tree.carseats,best=13)
plot(prune.carseats)
text(prune.carseats,pretty=0)

### Now we can evaluate the pruned tree on the test set.

tree.pred<-predict(prune.carseats,Carseats[-train,]type="class")
with(Carseats[-train,]table(tree.pred,High))

### The pruned tree did about as well as the unpruned model. This can be interpreted as the pruned tree did not hurt the clasifcation with respect to the misclassification errors, but it did provide a simpler tree.



### Random Forests and Boosting
### These methods use trees as building blocks to build more complex models.
### Here we will use the Bosting housing data to explore random forests and boosting.
### The data is from the "MASS" package and provides 1970 housig and census data for the 506 suburbs of Boston.

### Random forests build lots of bushy trees, and the average them to reduce their variance.

require(randomForest)
require(MASS)
set.seed(101)
dim(Boston)
train<-sample(1:nrow(Boston),300)
?Boston

### Now we will fit a random forest and see how well it performs. We use the response variable of "medv", or median housing value.

rf.boston<-randomForest(medv~.,data=Boston,subset=train)
rf.boston


### The MSR and % variance explained are based on the out of bag estimates (OOB).
### OOB is a device in random forests to get honest error estimates. 
### The model reports that "mtry=4", which is the number of variables randomly chosen at each split.
### Since p=13, we could try all possible 13 values of "mtry" and plot below. 

oob.err<-double(13)
test.err<-double(13)
for(mtry in 1:13){
	fit<-randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
	oob.err[mtry]<-fit$mse[400]
	pred<-predict(fit,Boston[-train])
	test.err[mtry]<-with(Boston[-train],mead((medv-pred)^2))
	cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err)pch=19,col="red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red,blue"))

### Fairly simple to implement. The test-curve dropped below the OOB curve, because they are estimates based on the availbale data, each with their own standard errors.
### Notice that when mtry=13, this corresponds to bagging.

### Boosting builds a lot of small tree. Unlike random forests, each new tree in boosting tried to patch up the deficiencies of the current ensemble.

require(gbm)
boost.boston<-gbm(medv~.,data=Boston[train,],distribution="gaussian",n.tree=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.boston)
plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")

### Now we can attempt to make a prediction on the test set.
### With Boosting, the number of trees is a tuning parameter, where too many trees can lead to overfitting.
### To overcome this, we use cross validation to select the number of trees.
### First, we compute the test error as a function of the number of trees and plot the result.

ntrees<-seq(from=100,to=10000,by=100)
predmat<-predict(boost.boston,newdata=Boston[-train,],n.trees=n.trees)
dim(predmat)
berr<-with(Boston[-train,],apply((predmat-medv)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error",xlab="Number Trees",main="Boosting Test Error")
abline(h=min(test.err),col="red")







