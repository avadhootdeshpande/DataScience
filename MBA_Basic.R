rm(list=ls())

#install.packages("datasets","arules","arulesViz")

library(datasets)
library(Matrix)
library(arules)
library(grid)
library(arulesViz)

data(Groceries)
Groceries
summary(Groceries)
#check split function
#?split
?itemFrequencyPlot
?apriori
View(Groceries)
itemFrequencyPlot(Groceries, topN=20, type="relative") #gives support value
#type="absolute"

rules=apriori(Groceries, parameter=list(supp=0.003,conf=0.8))
rules
rules=apriori(Groceries, parameter=list(supp=0.002, conf=0.8))
rules
rules=apriori(Groceries, parameter=list(supp=0.001, conf=0.8))
rules


inspect(rules)
inspect(rules[1:5])

plot(rules)

# Show top 5 rules but 2 digits
?options
options(digits=2)

rules=sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

?plot

plot(rules)
plot(rules,engine="interactive")
View(Groceries)
class(Groceries)
rules2=apriori(data=Groceries, parameter=list(supp=0.0005,conf=0.7), 
            appearance=list(default="lhs",rhs=c("butter milk")))

rules2=sort(rules2, by="confidence", decreasing=TRUE)
inspect(rules2[1:5])

rules21=apriori(data=Groceries, parameter=list(supp=0.0005,conf=0.7), 
               appearance=list(default="lhs",rhs=c("yogurt")))

rules21=sort(rules21, by="confidence", decreasing=TRUE)
inspect(rules21[1:5])

#rerun the algorithm, but this time reduce our parameters for support and confidence
#and save the result set into a different object

install.packages("plotly")
library(ggplot2)
library(plotly)

plot(rules2)

#rules2=sort(rules2, decreasing=FALSE,by="confidence")
rules2=sort(rules2, decreasing=FALSE,by="lift")

inspect(rules2[1:5])

plot(rules2[1:5], method="graph",interactive=T)

plot(rules2[1:5], method="graph",interactive=T, shading=NA)

inspect(rules2[1:5])
?apriori

rules3=apriori(data=Groceries, parameter=list(supp=0.001,conf=0.08), 
              appearance=list(default="lhs",rhs="yogurt"), 
              control=list(verbose=F))
inspect(rules3[1:4])
rules3=apriori(data=Groceries, parameter=list(supp=0.001,conf=0.08), 
               appearance=list(default="lhs",rhs=c("yogurt","butter milk")))

rules3
rules3=sort(rules3, decreasing=FALSE,by="confidence")
inspect(rules3[1:5])

plot(rules3[1:5], method="graph",interactive=T, shading=NA)

write(rules2, file="C:/Users/Avadhoot/Desktop/Data science with R/MArket basket analysis/rules2.csv")

#If data is not in required format

setwd("C:/Users/punkmule/Box Sync/Kapil/Personal/Analytics/Ethans/Tableau")
data=read.csv("Sample - Superstore Sales (Excel).csv")

View(data)
names(data)

as.
?split
i = split(data$Product.Name, data$Order.ID)
#check if we need to use any other field instead
head(i,3)

txn = as(i, "transactions")

#Now we convert the data into a "Transaction" object optimized for running the arules algorithm
# ?as
# ?matrix
# a=data.frame(c(1,2,3,4,5,6),2,3)
# b=as(a,"matrix")

# txn = as(i, "transactions")
# summary(txn)
# txn

summary(txn)
rules = apriori(txn, parameter = list(sup = 0.0002, conf = 0.6, target="rules"))
summary(rules)
inspect(rules)
names(data)

#Build rules on superstore data using category & sub - categories

i = split(data$Product.Sub.Category, data$Order.ID)

txn = as(i, "transactions")
summary(txn)
?itemFrequencyPlot
itemFrequencyPlot(txn,topN=15, type="absolute")
itemFrequencyPlot(txn,support=0.01, type="absolute")
#In case of error use below
dev.off()
