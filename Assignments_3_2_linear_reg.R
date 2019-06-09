#add first column and build the models

Assignment_House_Data= read.csv("C:/Users/Avadhoot/Desktop/Data science with R/linear regression/HousingData_data.csv",header = T)

View(Assignment_House_Data)
summary(Assignment_House_Data)

plot(Assignment_House_Data)
pairs(Assignment_House_Data)
class(Assignment_House_Data)
as.numeric(Assignment_House_Data$SegmentofCity)
Assignment_House_Data$NewSegmentofcity=(c(Assignment_House_Data$.,as.numeric(Assignment_House_Data$SegmentofCity)))

Assignment_House_Data=Assignment_House_Data[,-1]
cor(Assignment_House_Data)

#method 1 checking vif
vifstep(Assignment_House_Data,th=5)

vif(Assignment_House_Data[,-2])
names(Assignment_House_Data)
vif(Assignment_House_Data[,c(-2,-4)])
vif(Assignment_House_Data[,c(-2,-4,-1)])

mod1=lm(SellingPrice000s~NumberofBathrms+GarageSize+NewSegmentofcity,data = Assignment_House_Data)
summary(mod1)

orig=lm(SellingPrice000s~NumberofBathrms+GarageSize+NewSegmentofcity,data = Assignment_House_Data)
summary(orig)
names(orig)

mean(abs(orig$residuals/Assignment_House_Data$SellingPrice000s))
#MApe value is  0.1633832


#method 2


m1=lm(SellingPrice000s~.,data = Assignment_House_Data)
summary(m1)
m2=step(m1)

m3=lm(SellingPrice000s~NumberofBedrms+HouseSize00Sqft+NewSegmentofcity,data = Assignment_House_Data)
summary(m3)
mean(abs(m3$residuals/Assignment_House_Data$SellingPrice000s))
#MApe value 0.1438358

# method 3
library(sp)
library(raster)
library(usdm)
method3_alldata= lm(SellingPrice000s~.,data = Assignment_House_Data)
View(Assignment_House_Data)
summary(method3_alldata)
vif(Assignment_House_Data)
method3_drop1=lm(SellingPrice000s~NumberofBathrms+NumberofBedrms+NewSegmentofcity+NumberofBedrms,data = Assignment_House_Data)
summary(method3_drop1)
names(Assignment_House_Data)
vif(Assignment_House_Data[,c(-1,-2,-5)])

mean(abs(method3_drop1$residuals/Assignment_House_Data$SellingPrice000s))
#MApe value 0.1376307

plot(orig)
plot(m3)
plot(method3_drop1)


#mtcars - use mpg as dependent and rest all as independent variables
View(mtcars)
head(mtcars,10)
Assignment_mtcars=mtcars
head(Assignment_mtcars)
cor(Assignment_mtcars)

#method 1
vifstep(Assignment_mtcars)
names(Assignment_mtcars)

vif(Assignment_mtcars[,c(-2,-3)])

method1_drop1=lm(mpg~hp+drat+wt+qsec+ vs+am+gear+carb, data = Assignment_mtcars)
summary(method1_drop1)#drop vs
method1_drop2=lm(mpg~hp+drat+wt+qsec+ am+gear+carb, data = Assignment_mtcars)
summary(method1_drop2)#drop gear
method1_drop3=lm(mpg~hp+drat+wt+qsec+ am+carb, data = Assignment_mtcars)
summary(method1_drop3)#drop hp
method1_drop4=lm(mpg~drat+wt+qsec+ am+carb, data = Assignment_mtcars)
summary(method1_drop4)#drop drat
method1_drop5=lm(mpg~wt+qsec+ am+carb, data = Assignment_mtcars)
summary(method1_drop5)#carb
method1_drop6=lm(mpg~wt+qsec+ am, data = Assignment_mtcars)
summary(method1_drop6)
names(method1_drop6)
#R^2 value  0.8336 
mtcars_all=lm(mpg~.,data = Assignment_mtcars)
summary(mtcars_all)
mean(abs(method1_drop6$residuals/mtcars_all$mpg)) # not getting generated please help

#method 2
method2_fit1=lm(mpg~.,data = Assignment_mtcars)
summary(method2_fit1)
method2_fit2=step(method2_fit1)
summary(method2_fit2)
#R^2 value 0.8336
mean(abs(method2_fit2$residuals/mtcars_all$mpg)) # not getting generated please help

#method 3
method3_var1=lm(mpg~.,data = Assignment_mtcars)
vif(Assignment_mtcars)
summary(method3_var1)
method3_var2=lm(mpg~.-disp,data = Assignment_mtcars)
vif(Assignment_mtcars[,c(-3)])

method3_var3=lm(mpg~.-disp -cyl,data = Assignment_mtcars)
summary(method3_var3)
names(Assignment_mtcars)
vif(Assignment_mtcars[,c(-3,-2)])

method3_var4=lm(mpg~.-disp -cyl-wt,data = Assignment_mtcars)
summary(method3_var4)
names(Assignment_mtcars)
vif(Assignment_mtcars[,c(-3,-2,-6)])

method3_var5=lm(mpg~.-disp -cyl-wt-hp,data = Assignment_mtcars)
summary(method3_var5)
names(Assignment_mtcars)
vif(Assignment_mtcars[,c(-3,-2,-6,-4)])

method3_var6=lm(mpg~.-disp -cyl-wt-hp-gear,data = Assignment_mtcars)
summary(method3_var6)
names(Assignment_mtcars)
vif(Assignment_mtcars[,c(-3,-2,-6,-4,-10)])

method3_var7=lm(mpg~.-disp -cyl-wt-hp-gear-qsec,data = Assignment_mtcars)
summary(method3_var7)
names(Assignment_mtcars)
vif(Assignment_mtcars[,c(-3,-2,-6,-4,-10,-7)])

method3_var8=lm(mpg~.-disp -cyl-wt-hp-gear-qsec-drat,data = Assignment_mtcars)
summary(method3_var8)
names(Assignment_mtcars)
vif(Assignment_mtcars[,c(-3,-2,-6,-4,-10,-7,-5)])
#R^2 value 0.7585 
mean(abs(method3_var8$residuals/mtcars_all$mpg)) # not getting generated please help

plot(method1_drop6)
plot(method2_fit2)
plot(method3_var8)
