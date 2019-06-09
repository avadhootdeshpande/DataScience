rm(list = ls())

UniBank=read.csv("C:/Users/Avadhoot/Desktop/Data science with R/Decision tree/UniversalBank.csv", header=TRUE)
View(UniBank)
library(lattice)
library(ggplot2)
library(caret)

sum(UniBank$Personal.Loan)

?nearZeroVar

#We will check whether there are any variables with unique values
#(which can cause the model to crash or fit to be unstable) 
#using nearZeroVar function

summary(UniBank)

nzv_out=nearZeroVar(UniBank, freqCut=95/5, saveMetrics=TRUE)
nzv_out

UniBank1=UniBank[,-c(1,5,9)]
summary(UniBank1)
#now we will create classes for age and income 
#to make them easy to interprete

summary(UniBank1)

?cut
#Used to create age groups of with break points and covnert the field into factor
UniBank1$AgeClass=cut(UniBank1$Age, c(20,30,40,50,60,max(UniBank1$Age)),
                      labels=c("21-30","31-40","41-50","51-60",">60"))
# threshold points c(20,30,40,50,60,max(UniBank1$Age))
summary(UniBank1)

UniBank1$IncomeClass=cut(UniBank1$Income, c(0,30,50,80,110,150,max(UniBank1$Income)),
                         labels=c("0-30","31-50","51-80","81-110","111-150",">150"))

summary(UniBank1)
UniBank1=UniBank1[,-c(1,3)]

#convert variables as factors
UniBank1$Personal.Loan=as.factor(UniBank1$Personal.Loan)
UniBank1$Securities.Account=as.factor(UniBank1$Securities.Account)
UniBank1$CD.Account=as.factor(UniBank1$CD.Account)
UniBank1$Online=as.factor(UniBank1$Online)
UniBank1$CreditCard=as.factor(UniBank1$CreditCard)
UniBank1$Education=as.factor(UniBank1$Education)
UniBank1$Family=as.factor(UniBank1$Family)

View(UniBank1)

sum(UniBank$Personal.Loan)

set.seed(2017)
inTrain=createDataPartition(UniBank1$Personal.Loan, p=0.7,list=FALSE)
training=UniBank1[inTrain,]
testing=UniBank1[-inTrain,]
#class(training)
# sum((training$Personal.Loan))
# sum(as.numeric(training$Personal.Loan))
# 3836
# 3500+336

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

?rpart

# fit1=rpart(Personal.Loan ~ ., method="class", data=training,
#         control=rpart.control(minsplit=20,cp=0.001))

fit=rpart(Personal.Loan ~ ., method="class", data=training)

# fit1=rpart(Personal.Loan ~ IncomeClass+AgeClass, method="class", data=training)
# rpart.plot(fit1,extra=1,cex=0.8)

?rpart.plot
rpart.plot(fit)#gives probability
rpart.plot(fit,extra=1,cex=0.8)
#rpart.plot(fit,extra=1,cex=0.5)
#Cex=0.5 to change the font size

45/2750

names(fit)
summary(fit)

#cp is complexity parameter
#balance between number of nodes and accuracy

?which.min
names(fit)
fit$cptable[,"xerror"]
which.min(fit$cptable[,"xerror"])

bestcp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
bestcp

#bestcp=0.01

?prune
tree.pruned=prune(fit, cp=bestcp)

fit1=rpart(Personal.Loan ~ ., method="class", data=training,
        control=rpart.control(minsplit=300))
# remove all the nodes associated even if we have
#one node having less number of count in min split
?rpart.plot
rpart.plot(fit1,extra=1, cex=0.6)


#alternate

fit1=rpart(Personal.Loan ~ ., method="class", data=training,
        control=rpart.control(minsplit=10))

rpart.plot(fit1,extra=1, cex=0.8)

summary(testing)

View(testing)
Pred=predict(tree.pruned, newdata=testing[,-5],type="class")

caret::confusionMatrix(table(testing$Personal.Loan, Pred, dnn=list("actual","predicted")))

#Calculate accuracy using test data
#GermanCredit data to build a classification tree class is the field
#regression tree on mtcars data with mpg as dependent



##Randomforest on Germancredit
data("GermanCredit")
a=GermanCredit

a$Class=ifelse(a$Class=="Bad",0,1)
a$Class[1:5]
abc=createDataPartition(a$Class,p=0.7,list = FALSE)

test=a[-abc,]
train=a[abc,]

sum(a$Class[abc])

?randomForest
library(randomForest)
F1=randomForest(as.factor(Class)~., data = train)
#,ntree=100)

aa=predict(F1,train,type = "vote")
View(aa)
names(aa)
aa1=ifelse(aa[,2]>0.5,1,0)
aa1=ifelse(aa1==1,"Good",'Bad')
table(aa1,train$Class)

caret::confusionMatrix(table(aa1,train$Class))
