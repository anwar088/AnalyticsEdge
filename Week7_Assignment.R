#Week7 Assignment

setwd("~/GitHub/MITx_15-071x-AnalyticsEdge")

library("caTools", lib.loc="C:/Users/Tom_Anichini/Documents/R/win-library/2.15")
library("rpart", lib.loc="C:/Program Files/R/R-2.15.1/library")
library("rpart.plot", lib.loc="C:/Users/Tom_Anichini/Documents/R/win-library/2.15")
library("randomForest", lib.loc="C:/Users/Tom_Anichini/Documents/R/win-library/2.15")
library(flexclust)

require(ISLR)
require(tree)

# Install and load mice package
install.packages("mice")
library(mice)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
colnames(train)[1:10]
test.Happy <- rep(NA,1980)
test <- cbind(test[1:7],test.Happy,test[8:109])
colnames(test) <- c(colnames(test[1:7]),"Happy",colnames(test[9:110]))
imputedTrain <- complete(mice(train))



str(train)
merged <- rbind(train,test)
merged[4615:4620,1:10]
summary(merged$YOB)
merged$YOB[merged$YOB==2039] <- NA
set.seed(144)
imputed <- complete(mice(merged))
summary(imputed)
train <- imputedTrain

table(train$HouseholdStatus)
mergedrows <- nrow(merged)
merged$kids <- rep(0,mergedrows)
kidsStatus <- c("Domestic Partners (w/kids)","Married (w/kids)","Single (w/kids)")
merged$kids[merged$HouseholdStatus == "Domestic Partners (w/kids)"]=1
merged$kids[merged$HouseholdStatus == "Married (w/kids)"]=1
merged$kids[merged$HouseholdStatus == "Single (w/kids)"]=1
table(merged$kids)
merged$relation <- rep(1,mergedrows)
relStatus <- c("Domestic Partners (no kids)","Married (no kids)","Single (no kids)")
merged$relation[merged$HouseholdStatus == "Domestic Partners (no kids)"]=0
merged$relation[merged$HouseholdStatus == "Married (no kids)"]=0
merged$relation[merged$HouseholdStatus == "Single (no kids)"]=0
table(merged$relation)
colnames(merged)[110:112]
train <- merged[1:4619,]
test <- merged[4620:6599,]

colnames(train)[1:10]
mean(train$Happy)
sd(train$Happy)

attach(train)
tree.happy=tree(Happy~. ,data=train)
plot(tree.happy)
summary(tree.happy)
text(tree.happy,pretty=0)
tree.happy
test <- cbind(test[,1:7],test[,9:112])
test.pred=predict(tree.happy,newdata=test)
output <- cbind(test$UserID,test.pred)
head(output)
write.csv(output,"submission72187.csv")


set.seed(1011)

3220*.7
traintrain=sample(1:nrow(train),2254)
train.happy=train[traintrain,]
test.happy=train[-traintrain,]
train.tree=tree(Happy~.,data=train.happy)
plot(train.tree);text(train.tree,pretty=0)
test.pred=predict(train.tree,newdata=test.happy)
with(test.happy,table(1*(test.pred>.5),Happy))

(600+886)/2365

plot(rownames(table(train$YOB,train$Happy)),table(train$YOB,train$Happy)[,1]/(table(train$YOB,train$Happy)[,1]+table(train$YOB,train$Happy)[,2]))
baseline <- sum(test.happy$Happy)/966
baseline

table(train.happy$YOB)


str(train$YOB)

Random Forests
--------------
  Random forests build lots of bushy trees, and then average them to reduce the variance.

```{r}
require(randomForest)
require(MASS)
set.seed(101)
dim(train)
summary(train)
train.sample=sample(1:nrow(train),2254)
train.train=train[train.sample,]
```
Lets fit a random forest and see how well it performs. We will use the response `medv`, the median housing value (in \$1K dollars)

```{r}
rf.traintrain=randomForest(Happy~.,data=train[,2:112],subset=train.sample)
rf.traintrain
sum(rf.traintrain$importance>20)
```

The MSR and % variance explained are based on OOB  or _out-of-bag_ estimates, a very clever device in random forests to get honest error estimates. The model reports that `mtry=4`, which is the number of variables randomly chosen at each split. Since $p=13$ here, we could try all 13 possible values of `mtry`. We will do so, record the results, and make a plot.

```{r}
oob.err=double(7)
test.err=double(7)
for(mtry in 1:7){
  fit=randomForest(Happy~.,data=train[2:110],subset=train.sample,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,train[-train.sample,])
  test.err[mtry]=with(train[-train.sample,],mean((Happy-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))
```

Not too difficult! Although the test-error curve drops below the OOB curve, these are estimates based on data, and so have their own standard errors (which are typically quite large). Notice that the points at the end with `mtry=13` correspond to bagging.

Boosting
--------
  Boosting builds lots of smaller trees. Unlike random forests, each new tree in boosting tries to patch up the deficiencies of the current ensemble.
```{r}
require(gbm)
boost.happy=gbm(Happy~.,data=train[train.sample,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.happy)
plot(boost.happy,i="lstat")
plot(boost.boston,i="rm")
```
Lets make a prediction on the test set. With boosting, the number of trees is a tuning parameter, and if we have too many we can overfit. So we should use cross-validation to select the number of trees. We will leave this as an exercise. Instead, we will compute the test error as a function of the number of trees, and make a plot.

```{r}
n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.happy,newdata=train[-train.sample,],n.trees=n.trees)
dim(predmat)
berr=with(train[-train.sample,],apply( (predmat-Happy)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")
abline(h=min(test.err),col="red")
```

