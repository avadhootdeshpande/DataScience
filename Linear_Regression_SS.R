#superstore data - sales is dependent variable
ASSTRData= read.csv(file.choose())
summary(ASSTRData)


# set.seed(100)
# library(caret)
# ?createDataPartition
# testdata=createDataPartition(ASSTRData,p=0.7)
#was not able to create data partition on the given dataset, please guide

#converting non numeric to numeric

head(ASSTRData)
ASSTRData=ASSTRData[,c(-1,-2,-3,-4,-6,-7)]
names(ASSTRData)

plot(ASSTRData)
pairs(ASSTRData)
names(ASSTRData)

ASSTRData$Ship.Mode=as.numeric(ASSTRData$Ship.Mode)
ASSTRData$Segment=as.numeric(ASSTRData$Segment)
ASSTRData$Country=as.numeric(ASSTRData$Country)
ASSTRData$City=as.numeric(ASSTRData$City)
ASSTRData$State=as.numeric(ASSTRData$State)
ASSTRData$Postal.Code=as.numeric(ASSTRData$Postal.Code)
ASSTRData$Region=as.numeric(ASSTRData$Region)
ASSTRData$Product.ID=as.numeric(ASSTRData$Product.ID)
ASSTRData$Category=as.numeric(ASSTRData$Category)
ASSTRData$Sub.Category=as.numeric(ASSTRData$Sub.Category)
ASSTRData$Product.Name=as.numeric(ASSTRData$Product.Name)
ASSTRData$Sales=as.numeric(ASSTRData$Sales)
ASSTRData$Quantity=as.numeric(ASSTRData$Quantity)
ASSTRData$Discount=as.numeric(ASSTRData$Discount)
ASSTRData$Profit=as.numeric(ASSTRData$Profit)

cor(ASSTRData)
names(ASSTRData)

ASSTRData=ASSTRData[,(-3)]
cor(ASSTRData)

plot(ASSTRData)
pairs(ASSTRData)

#Model 1
library(usdm)
?vifstep
vifstep(ASSTRData,th=5)

vif(ASSTRData)
names(ASSTRData)
vif(ASSTRData[,-7])
method1_ss1=lm(Sales~.-Product.ID,data = ASSTRData)
summary(method1_ss1)#drop Category 

method1_ss2=lm(Sales~.-Product.ID-Category,data = ASSTRData)
summary(method1_ss2)#drop Ship.Mode

method1_ss3=lm(Sales~.-Product.ID-Category-Ship.Mode,data = ASSTRData)
summary(method1_ss3)#drop State

method1_ss4=lm(Sales~.-Product.ID-Category-Ship.Mode-State,data = ASSTRData)
summary(method1_ss4)#drop Segment

method1_ss5=lm(Sales~.-Product.ID-Category-Ship.Mode-State-Segment,data = ASSTRData)
summary(method1_ss5)#drop City

method1_ss6=lm(Sales~.-Product.ID-Category-Ship.Mode-State-Segment-City,data = ASSTRData)
summary(method1_ss6)

#R^2 0.2739

#method2

method2_ss1=lm(Sales~.,data=ASSTRData)
summary(method2_ss1)
method2_ss2=step(method2_ss1)
summary(method2_ss2)
#R^2 0.2769


#method3

method3_ss1=lm(Sales~.,data=ASSTRData)
vif(ASSTRData)
names(ASSTRData)

method3_ss2=lm(Sales~. -Product.ID,data=ASSTRData)
summary(method3_ss2)
vif(ASSTRData[,-7])


method3_ss3=lm(Sales~. -Product.ID-Ship.Mode,data=ASSTRData)
summary(method3_ss3)
vif(ASSTRData[,c(-7,-1)])

method3_ss4=lm(Sales~. -Product.ID-Ship.Mode-State,data=ASSTRData)
summary(method3_ss4)
vif(ASSTRData[,c(-7,-1,-4)])

method3_ss5=lm(Sales~. -Product.ID-Ship.Mode-State-Segment,data=ASSTRData)
summary(method3_ss5)
names(ASSTRData)
vif(ASSTRData[,c(-7,-1,-4,-2)])


method3_ss6=lm(Sales~. -Product.ID-Ship.Mode-State-Segment-City,data=ASSTRData)
summary(method3_ss6)
names(ASSTRData)
vif(ASSTRData[,c(-7,-1,-4,-2,-3)])

method3_ss7=lm(Sales~. -Product.ID-Ship.Mode-State-Segment-City-Category,data=ASSTRData)
summary(method3_ss7)
names(ASSTRData)
vif(ASSTRData[,c(-7,-1,-4,-2,-3,-8)])
#R^2 value 0.2739 




plot(method1_ss6)
plot(method2_ss2)
plot(method3_ss7)
