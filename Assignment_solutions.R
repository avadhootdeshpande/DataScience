#Use arthritis data
#Calculate average age of male & female patients

library(vcd)
View(Arthritis)
Adata=Arthritis
Adata
names(Adata)
MaleAge=ifelse(Adata$Sex=='Male',Adata$Age,NA)
MaleAge
AvgAgeMale=mean(na.omit(MaleAge))
AvgAgeMale
#1325/25--> 53

FeMaleAge=ifelse(Adata$Sex!='Male',Adata$Age,NA)
FeMaleAge
AvgAgeFeMale=mean(na.omit(FeMaleAge))
AvgAgeFeMale
anothertry=mean(na.omit(ifelse(Adata$Sex!='Male',Adata$Age,NA)))
anothertry

#Create a field to define patients into 6 categories
# A-Treated & improved
# B-Treated & some improvement
# C-Treated & No improvement
# D-Placebo & improved
# E-placebo & some improvement
# F-Placebo & no improvement
#calcualte mix for each category
names(Adata)
unique((Adata$Improved))
unique((Adata$Treatment))
CatA=Adata[ifelse(Adata$Treatment=='Treated' & Adata$Improved=='Marked',T,F),]
CatA
CatB=Adata[ifelse(Adata$Treatment=='Treated' & Adata$Improved=='Some',T,F),]
CatB
CatC=Adata[ifelse(Adata$Treatment=='Treated' & Adata$Improved=='None',T,F),]
CatC
CatD=Adata[ifelse(Adata$Treatment=='Placebo' & Adata$Improved=='Marked',T,F),]
CatD
CatE=Adata[ifelse(Adata$Treatment=='Placebo' & Adata$Improved=='Some',T,F),]
CatE
CatF=Adata[ifelse(Adata$Treatment=='Placebo' & Adata$Improved=='None',T,F),]
CatF
# Use for loop to print below o/p(changed to 6 levels output do it)
# 0,0,0
# 0,0,1
# 0,1,0
# 0,1,1
# 1,1,1
# 1,1,0
# 1,0,0
# 1,0,1
# ..... 
for(A in 0:1)
{
  for(B in 0:1)
  {
    for(C in 0:1)
    {
      for(D in 0:1)
      {
        for(E in 0:1)
        {
          for(F1 in 0:1)
          {
            level6=paste(A,B,C,D,E,F1,sep='')
            print(level6)
          }
        }
      }
    }
  }
}
#remove missing values (NAs) from full join o/p


DataFrame1=data.frame(ID=c(1:5),Name=c('a','b','c','d','e'))
DataFrame2=data.frame(ID=c(1,2,6),Surname=c('p','q','r'))
?join
JoinRes=join(DataFrame1,DataFrame2,by='ID',type='full')
na.omit(JoinRes)
#alternative
JoinRes1=na.omit(join(DataFrame1,DataFrame2,by='ID',type='full'))
print(JoinRes1)


#Use createDataPartition function to split arthritis data into 2 groups
#group 1 with 60% of data and rest in other group
#need package 'caret'

library(caret)
View(Arthritis)
?createDataPartition

group1=createDataPartition(Arthritis$Age,p=0.6)
group1
group2=createDataPartition(-group1$Resample1)
group2

-------
  group11=createDataPartition(Arthritis$Age,p=0.06)
group11
group12=createDataPartition(-group11$Resample1)
group12  
  
#Create a custom function to print summary of a dataset use superstore data
#like mean, median, sum (for numeric fields)
SuperstoreData=read.csv('C:/Users/Avadhoot/Desktop/Data science with R/Excel reporting-29-12/given data/Sample - Superstore_US.csv')
?head
head(SuperstoreData)

meanSummery=function(sales,discount,quantity,profit)
{
 salesmean= mean(sales)
 discountmean= mean(discount)
 quantitymean= mean(quantity)
 profitmean= mean(profit)
 print(salesmean)
      print(discountmean) 
    print(quantitymean)
    print(profitmean)
}
meanSummery(SuperstoreData$Sales,SuperstoreData$Discount,SuperstoreData$Quantity,SuperstoreData$Profit)

medianSummery=function(sales,discount,quantity,profit)
{
  salesmedian= median(sales)
  discountmedian= median(discount)
  quantitymedian= median(quantity)
  profitmedian= median(profit)
  print(salesmedian)
  print(discountmedian) 
  print(quantitymedian)
  print(profitmedian)
}
medianSummery(SuperstoreData$Sales,SuperstoreData$Discount,SuperstoreData$Quantity,SuperstoreData$Profit)



sumSummery=function(sales,discount,quantity,profit)
{
  salessum= sum(sales)
  discountsum= sum(discount)
  quantitysum= sum(quantity)
  profitsum= sum(profit)
  print(salessum)
  print(discountsum) 
  print(quantitysum)
  print(profitsum)
}
sumSummery(SuperstoreData$Sales,SuperstoreData$Discount,SuperstoreData$Quantity,SuperstoreData$Profit)

#Build all charts that we looked at, for any field in mtcars data (other than mpg and cyl)

head(mtcars)

displacement=table(mtcars$disp)
displacement
#barplot

barplot(displacement,main = 'bar plot')
plot(displacement)
hist(displacement)
boxplot(disp~hp,data = mtcars)
#Use read.csv("....path") to read any csv file and using basic function learnt, draw some 
#insights

superstoreData1=read.csv('C:/Users/Avadhoot/Desktop/Data science with R/Excel reporting-29-12/given data/Sample - Superstore_US.csv')
View(superstoreData1)

totalsales=sum(superstoreData1$Sales)
p1=sprintf("total sales of the superstore is %f",totalsales)
p1


totalprofit=sum(superstoreData1$Profit)
p2=sprintf("total profit of the superstore is %f",totalprofit)
p2

totaldisc=sum(superstoreData1$Discount)
p3=sprintf("total discount offered by the superstore is %f",totaldisc)
p3

totalquant=sum(superstoreData1$Quantity)
p4=sprintf("total quantities sold by the superstore are %d",totalquant)
p4

unique(superstoreData1$Region)
#how many times region is occured 
regions=table(superstoreData1$Region)
regions
summary(superstoreData1)

#maximum sales
max(superstoreData1$Sales)

#maximum profit
max(superstoreData1$Profit)

#maximum loss
min(superstoreData1$Profit)

#customers from state
custstate=table(superstoreData1$State)
custstate
custname=superstoreData1$Customer.Name[custstate]
custname
