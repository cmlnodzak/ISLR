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


