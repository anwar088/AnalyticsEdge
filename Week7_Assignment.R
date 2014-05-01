#Week7 Assignment

setwd("~/GitHub/AnalyticsEdge")

library("caTools" lib.loc="C:/Users/Tom_Anichini/Documents/R/win-library/2.15")
library("rpart", lib.loc="C:/Program Files/R/R-2.15.1/library")
library("rpart.plot", lib.loc="C:/Users/Tom_Anichini/Documents/R/win-library/2.15")
library(flexclust)

require(ISLR)
require(tree)

# Install and load mice package
#install.packages("mice")
library(mice)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
test.Happy <- rep(NA,1980)
test <- cbind(test[1:7],test.Happy,test[8:109])
colnames(test)[1:10]
colnames(test) <- c(colnames(test[1:7]),"Happy",colnames(test[9:110]))
colnames(test)[1:10]
merged <- rbind(train,test)
colnames(merged)[1:10]
merged$YOB[merged$YOB==2039] <- NA
set.seed(144)
imputedMerged <- complete(mice(merged))

imputedMergedrows <- nrow(imputedMerged)
imputedMerged$Kids <- rep(0,imputedMergedrows)
kidsStatus <- c("Domestic Partners (w/kids)","Married (w/kids)","Single (w/kids)")
imputedMerged$Kids[imputedMerged$HouseholdStatus == "Domestic Partners (w/kids)"]=1
imputedMerged$Kids[imputedMerged$HouseholdStatus == "Married (w/kids)"]=1
imputedMerged$Kids[imputedMerged$HouseholdStatus == "Single (w/kids)"]=1
table(imputedMerged$Kids)
imputedMerged$Relation <- rep(1,imputedMergedrows)
relStatus <- c("Domestic Partners (no kids)","Married (no kids)","Single (no kids)")
imputedMerged$Relation[imputedMerged$HouseholdStatus == "Domestic Partners (no kids)"]=0
imputedMerged$Relation[imputedMerged$HouseholdStatus == "Married (no kids)"]=0
imputedMerged$Relation[imputedMerged$HouseholdStatus == "Single (no kids)"]=0
table(imputedMerged$Relation)
colnames(imputedMerged[,111:112]) <- c("Kids","Relation")
colnames(imputedMerged[,1:11])
predHappy2 <- cbind(imputedMerged$UserID[4620:6599],1*(imputedMerged$Happy[4620:6599]))
head(predHappy2)
write.csv(predHappy2,"predHappyBin.csv")

newColumnOrder <- c(1,8,110:112,2:4,6:7,9:109) # removing Household Status, redundant w/ Kids and Relation
newData <- imputedMerged[,newColumnOrder]

colnames(newData)[1:11]
summary(newData[,1:11])
attach(newData)
typeof(newData[,1])
typeof(newData[,2])
typeof(newData[,3])

typeof(newData[,4])
typeof(newData[,5])
typeof(newData[,6])

typeof(newData[,7])
typeof(newData[,8])
typeof(newData[,9])

typeof(newData[,10])
typeof(newData[,11])

summary(newData[,3])
colnames(newData[,1:20])


train <- newData[1:4619,]
test <- newData[4620:6599,]

attach(train)
tree.happy=tree(Happy~. ,data=train)
plot(tree.happy)
summary(tree.happy)
text(tree.happy,pretty=0)
tree.happy
test$Happy <- newData$Happy[4620:6599]

test.pred=1*(predict(tree.happy,newdata=test)>.5)
testTree <- predict(tree.happy,newdata=test)
table(test.pred,test$Happy)

(234+978)/(234+978+686+82)


trainCART = rpart(Happy~., data=train, method="class")
testCART = predict(trainCART,newdata=test)
testCARTpred <- testCART[,2]
head(testCART)
table(1*(testCART[,2]>.5),test$Happy)
(388+859)/(388+859+532+201)
plot(testCART[,2],test.pred)
?predict


?rpart
trainLOG <- glm(Happy~., data=train)
summary(trainLOG)
testLOG <- predict.glm(trainLOG,newdata=test)
testLOG[testLOG>=1] <- 0.9999
testLOG[testLOG<=0] <- 0.0001
table(1*(testLOG>.5),test$Happy)
(565+855)/(565+855+355+205)
glmCSV <- cbind(test$UserID,testLOG)
colnames(glmCSV) <- c("UserID","Probability1")
head(glmCSV)
write.csv(glmCSV,"glm.csv",row.names=F)
?write.csv

masterPred <- apply(cbind(testCARTpred,testLOG),1,mean)
masterPred <- cbind(test$UserID,masterPred)
table(1*(masterPred[,2]>.5),test$Happy)
(472+876)/1980


colnames(masterPred) <- c("UserID","Probability1")
write.csv(masterPred,"master.csv",row.names=F)

output <- cbind(test$UserID,test.pred)
head(output)
write.csv(output,"tree.csv")




head(output)

set.seed(1011)

traintrain=sample(1:nrow(train),2254)
train.happy=train[traintrain,]
test.happy=train[-traintrain,]
train.tree=tree(Happy~.,data=train.happy)
plot(train.tree);text(train.tree,pretty=0)
test.pred=predict(train.tree,newdata=test.happy)
with(test.happy,table(1*(test.pred>.5),Happy))

(331+1129)/(331+1129+175+730)

plot(rownames(table(train$YOB,train$Happy)),table(train$YOB,train$Happy)[,1]/(table(train$YOB,train$Happy)[,1]+table(train$YOB,train$Happy)[,2]))
baseline <- sum(test.happy$Happy)/2365
baseline



Boosting
--------
  Boosting builds lots of smaller trees. Unlike random forests, each new tree in boosting tries to patch up the deficiencies of the current ensemble.
```{r}
require(gbm)
boost.happy=gbm(Happy~.,data=train[train.sample,],distribution="bernoulli",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.happy)
plot(boost.happy)

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

summary(train)
