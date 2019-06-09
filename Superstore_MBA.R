SSData=read.csv("C:/Users/Avadhoot/Desktop/Data science with R/Excel reporting-29-12/given data/Sample - Superstore_US.csv")

summary(SSData)
names(SSData)

SSData=SSData[,c(2,16)]
names(SSData)
class(SSData)


??itemFrequencyPlot

library(datasets)
library(Matrix)
library(arules)
library(grid)
library(arulesViz)

itemFrequencyPlot(SSData$Sub.Category, topN=20, type="absolute") 
# not able to generate pls help
# Error in (function (classes, fdef, mtable)  : 
# unable to find an inherited method for function 'itemFrequencyPlot' for signature '"factor"'
itemFrequencyPlot(SSData$Sub.Category)
library(arules)


rules=apriori(SSData, parameter=list(supp=0.0003,conf=0.8))

inspect(rules)
plot(rules)


summary(SSData)
#Binders 1523 finding rules on it


AnaRule1=apriori(data=SSData, parameter=list(supp=0.0003,conf=0.8), 
               appearance=list(default="lhs",rhs=c("Binders")))

#unable to get the result from this getting following error
#Error in asMethod(object) : Binders is an unknown item label
