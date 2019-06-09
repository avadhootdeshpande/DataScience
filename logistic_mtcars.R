#Predict if a vehicle is automatic or manual - mtcars

logmtcars=mtcars
head(logmtcars)
library(caret)

summary(logmtcars)

set.seed(111)
traininput=createDataPartition(logmtcars$am,p = 0.8,list = FALSE)

Trainmtcars=logmtcars[traininput,]
Testmtcars=logmtcars[-traininput,]

sum(Trainmtcars$am)
sum(Testmtcars$am)


#method1
library(usdm)

names(logmtcars)

vif(logmtcars[,-9])
vifstep(logmtcars[,-9],th = 5)
# 
# ---------- VIFs of the remained variables -------- 
#   Variables      VIF
# 1       mpg 4.722618
# 2      drat 2.881643
# 3      qsec 3.434482
# 4        vs 3.840130
# 5      gear 3.691968
# 6      carb 3.545845
#----building model on above var

m1=glm(am ~ mpg+drat+qsec+vs+gear+carb,data=Trainmtcars,family=binomial(link="logit"),maxit=100)
# Warning message:
#   glm.fit: algorithm did not converge
#   glm.fit: fitted probabilities numerically 0 or 1 occurred 
# was getting above two warnings but keeping maxit=100 removed 1st still unable to resolve second one.
summary(m1)
# and getting incorrect p value 

alldata=glm(am~. ,data=Trainmtcars,family=binomial(link="logit"))
summary(m1)
#method 2
library(MASS)
m2=stepAIC(alldata,direction = "both")

m2_1=glm(formula = am ~  vs + carb+disp+ wt+mpg+qsec+cyl+drat+ hp+gear+ cyl, family = binomial(link = "logit"), 
         data = Trainmtcars)
summary(m2_1)


