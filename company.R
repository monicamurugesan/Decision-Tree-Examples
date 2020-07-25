library(caret)
library(ggplot2)
library(gmodels)
library(party)
library(C50)
library(xgboost)
library(randomForest)
library(MASS)
setwd("H://RStudio//assignment//")
cdata <-read.csv(file.choose())
View(cdata)
attach(cdata)
cdata1<-as.factor(ifelse(Sales<10,"NO","Yes"))
cdata["Urban"]<-as.integer(Urban=="Yes")
cdata["US"]<-as.integer(US=="Yes")

View(cdata1)
str(cdata1)
compdata<-cbind(cdata[,-1],cdata1)
View(compdata)
str(compdata)

inTrainingcomp <-createDataPartition(compdata$cdata1,p=0.75,list=F)
cdtrain<-compdata[inTrainingcomp,]
cdtest<-compdata[-inTrainingcomp,]
attach(cdtrain)
#cdtest<-compdata[201:400,]
comp_tree<-C5.0(cdata1~Advertising+Age+CompPrice+Education+Income+Population+Price+ShelveLoc+Urban+US,data=cdtrain)
plot(comp_tree)
?C5.0
summary(comp_tree)
pred<-predict.C5.0(comp_tree,cdtest)
tablecomp <-table(cdtest$cdata1,pred)
tablecomp
plot(comp_tree,ignore=warning)
confusionMatrix(cdtest$cdata1,pred)
CrossTable(cdtest$cdata1,pred)
mean(cdtest$cdata1==pred)
?prune
library(rpart)
##pruning
rpartcomp <-rpart(cdata1~.,data=cdtrain,method="class", control = rpart.control(cp = 0))
summary(rpartcomp)
printcp(rpartcomp)
plotcp(rpartcomp)
prunecomp<-prune(rpartcomp,cp=0.0084)
base_test1<-predict(rpartcomp,cdata1)
summary(prunecomp)
test$pred <- predict(prunecomp, cdtest$cdata1, type = "class")
###boosting
mod1 <-train(cdata1~.,data=cdtrain,method="ada",trControl=trainControl("repeatedcv",number=2))
mod1
prada <-predict(mod1,cdtest)
mean(prada==cdtest$cdata1)
a<-table(cdtrain$cdata1,prada)
confusionMatrix(cdtest$cdata1,prada)
CrossTable(cdtest$cdata1)

