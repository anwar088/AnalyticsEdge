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
#write.csv(predHappy2,"predHappyBin.csv")

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
(425+823)/1980
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
#write.csv(glmCSV,"glm.csv",row.names=F)
?write.csv

masterPred <- apply(cbind(testCARTpred,testLOG),1,mean)
masterPred <- cbind(test$UserID,masterPred)
table(1*(masterPred[,2]>.5),test$Happy)
(472+876)/1980

masterPred2 <- apply(cbind(test$Happy,testLOG,testLOG),1,mean)
masterPred2 <- cbind(test$UserID,masterPred2)
table(1*(masterPred2[,2]>.5),test$Happy)
colnames(masterPred2) <- c("UserID","Probability1")
#write.csv(masterPred2,"master2.csv",row.names=F)
getwd()

colnames(masterPred) <- c("UserID","Probability1")
#write.csv(masterPred,"master.csv",row.names=F)

output <- cbind(test$UserID,test.pred)
head(output)
#write.csv(output,"tree.csv")




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

summary(train)

library(languageR) 
library(designGG) 
library(party) 
data.controls <- cforest_unbiased(ntree=100, mtry=30) 
set.seed(47)
system.time(data.cforest10 <- cforest(Happy ~ ., data = train, controls=cforest_unbiased(ntree=10, mtry=5)) )
system.time(data.cforest100 <- cforest(Happy ~ ., data = train, controls=cforest_unbiased(ntree=100, mtry=5)) )
system.time(data.cforest500 <- cforest(Happy ~ ., data = train, controls=cforest_unbiased(ntree=500, mtry=5)) )
system.time(data.cforest1000 <- cforest(Happy ~ ., data = train, controls=cforest_unbiased(ntree=1000, mtry=5)) )
system.time(data.cforest1000 <- cforest(Happy ~ ., data = train, controls=cforest_unbiased(ntree=1000, mtry=15)) )
system.time(data.cforest100_30 <- cforest(Happy ~ ., data = train, controls=data.controls) )
system.time(data.cforest500_30 <- cforest(Happy ~ ., data = train, controls=cforest_unbiased(ntree=500, mtry=30)) )
system.time(data.cforest1000_30 <- cforest(Happy ~ ., data = train, controls=cforest_unbiased(ntree=1000, mtry=30)) )

pred.rf10 <- predict(data.cforest10,newdata=test,type="prob")
table(1*(pred.rf10>.5),test$Happy)
(861+445)/1980

pred.rf <- predict(data.cforest,newdata=test,type="prob")
table(1*(pred.rf>.5),test$Happy)
(927+383)/1980
table(1*(pred.rf>.5),1*(testLOG>.5))
(1171+477)/1980

pred.rf1000 <- predict(data.cforest1000,newdata=test,type="prob")
table(1*(pred.rf1000>.5),test$Happy)
table(1*(pred.rf1000>.5),1*(testLOG>.5))

pred.rf100_30 <- predict(data.cforest100_30,newdata=test,type="prob")
table(1*(pred.rf100_30>.5),test$Happy)
(484+859)/1980
table(1*(pred.rf100_30>.5),1*(testLOG>.5))
(578+1103)/1980
predrf100_30 <- unlist(pred.rf100_30)
predrf100_30 <- cbind(test$UserID,predrf100_30)
colnames(predrf100_30) <- c("UserID","Probability1")
head(predrf100_30)
write.csv(predrf100_30,"predrf100_30.csv",row.names=F)

pred.rf500_30 <- predict(data.cforest500_30,newdata=test,type="prob")
table(1*(pred.rf500_30>.5),test$Happy)
(484+859)/1980
table(1*(pred.rf500_30>.5),1*(testLOG>.5))
(578+1103)/1980
predrf500_30 <- unlist(pred.rf500_30)
predrf500_30 <- cbind(test$UserID,predrf500_30)
colnames(predrf500_30) <- c("UserID","Probability1")
head(predrf500_30)
write.csv(predrf500_30,"predrf500_30.csv",row.names=F)

str(pred.rf1000)
pred.rf1000[1:10]
typeof(pred.rf1000)
predrf1k <- unlist(pred.rf1000)
predrf1k <- cbind(test$UserID,predrf1k)
colnames(predrf1k) <- c("UserID","Probability1")
head(predrf1k)
write.csv(predrf1k,"predrf1k.csv",row.names=F)
getwd()
