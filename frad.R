library(caret)
library(ggplot2)
library(gmodels)
library(party)
library(C50)
library(xgboost)
library(randomForest)
library(MASS)
setwd("H://RStudio//assignment")
fraud <-read.csv(file.choose())
View(fraud)
fraud1 <-as.factor(ifelse(fraud$Taxable.Income<30000,"RISKY","GOOD"))
View(fraud1)
fraud3<-as.factor(ifelse(fraud$Undergrad=="YES",0,1))
fraud4<-as.factor(ifelse(fraud$Urban=="YES",0,1))
fraudbind <-data.frame(fraud[,-c(1,2,5)],fraud1,fraud3,fraud4)
attach(fraudbind)
View(fraudbind)
str(fraudbind)

inTrainingf <-createDataPartition(fraud1,p=0.75,list=F)
fraudtrain<-fraudbind[inTrainingf,]
fraudtest<-fraudbind[-inTrainingf,]
fraud_model<-C5.0(fraud1~.,data=fraudtrain)
summary(fraud_model)
plot(fraud_model)
fraud_pred<-predict(fraud_model,fraud1)
tablefraud <-table(fraud1,fraud_pred)
tablefraud
confusionMatrix(fraud1,fraud_pred)
CrossTable(fraud1,fraud_pred)

library(rpart)
##pruning
rpartf <-rpart(fraud1~.,data=fraudtrain,method="class", control = rpart.control(cp = 0))
summary(rpartf)
printcp(rpartf)
plotcp(rpartf)
prunef<-prune(rpartf,cp=0.0084)
base_test1<-predict(rpartf,fraud1)
summary(prunef)
pred <- predict(prunef, fraud1, type = "class")
pred



