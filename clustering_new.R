rm(list=ls())

data(mtcars)
dimnames(mtcars)

?scale
mtcars.scale=scale(mtcars)
View(mtcars.scale)
summary(mtcars.scale)
head(mtcars.scale)

L=mean(mtcars$mpg)-2*sd(mtcars$mpg)
U=mean(mtcars$mpg)+2*sd(mtcars$mpg)

#if there are values less than L or greater than U

?dist

dist.mtcars=dist(mtcars.scale, method='euclidean')
length(dist.mtcars)
head(dist.mtcars)

#remember n*(n-1)/2 --- 
(31*32)/2

#Hierarchical clustering results
?hclust
hc=hclust(dist.mtcars, method="average")
hc
summary(hc)
head(hc)
names(hc)
plot(hc)
plot(hc,hang=-1)


hc$merge
hc$method
hc$height
hc$order
hc$labels

mtcars[20,]

hc1=as.data.frame(hc$labels)
View(hc1)

hc2=as.data.frame(hc$order)
View(hc2)

cbind(hc1,hc2)
#Use joins to undestand who sits where and how the groups are formed
#explore labling options

# Visualization of hclust
  plot(hc,hang=-1)

# Add rectagle around 4 groups
rect.hclust(hc, k=2, border=2:6)
plot(hc,hang=-1)

hc_6=hclust(dist.mtcars, method="average")
plot(hc_6,hang=-1)
# Add rectagle around 4 groups
rect.hclust(hc_6, k=4, border=2:4)

# Kmeans
Vall=1000
V1+V2=600

#variance - (x-mean)^2/N

?kmeans
a=kmeans(mtcars.scale, centers=1)
names(a)
wss=a$withinss
wss
sum(a$withinss)

kmeans(mtcars.scale, centers=2)
kmeans(mtcars.scale, centers=2)$withinss
kmeans(mtcars.scale, centers=2)$tot.withinss
sum(kmeans(mtcars.scale, centers=2)$withinss)

for(i in 2:15)
{wss[i]=sum(kmeans(mtcars.scale, centers=i)$withinss)}
#,iter.max = 25 use it if needed.

for(i in 2:15)
{wss[i]=(kmeans(mtcars.scale, centers=i,iter.max = 25)$tot.withinss)}


wss

set.seed(20)
#scree plot
plot(wss)
plot(1:15,wss, type="b", xlab="No. of Clusters", ylab="Within Group Sum of Squares ")
#type = b is both 
# Analysing cluster results
#max 6 or 4 clusters are good to have

set.seed(20)
mtcarsCluster= kmeans(mtcars.scale, 5)
m1=kmeans(mtcars.scale, 6)


mtcarsCluster$cluster
mtcarsCluster$centers

m1$cluster
m1$centers

table(mtcarsCluster$cluster)

Results= cbind.data.frame(mtcars,cluster=mtcarsCluster$cluster)
Final=Results[order(Results$cluster),]

Results1= cbind.data.frame(mtcars,cluster=m1$cluster)
Final1=Results1[order(Results1$cluster),]



View(Results)
View(Final)

plot(Final$cluster)

View(Results1)
View(Final1)

plot(Final1$cluster)
#Study the final utput and check characteristics of a cluster

# Create clusters using superstore data
# create cluster of customers using Unibank data


library(caret)
data(GermanCredit)
a=GermanCredit
names(a)
a$Class=ifelse(a$Class=="Good",1,0)
summary(a)
a=a[,c(-27,-45)]
a.scale=scale(a)
summary(a.scale)


gkmeans=kmeans(a.scale,centers = 1)
gcwss=gkmeans$tot.withinss
summary(gkmeans)

for(i in 2:200)
{gcwss[i]=(kmeans(a.scale, centers=i)$tot.withinss)}

rm(gcwss)
gcwss

plot(1:200,gcwss, type="b", xlab="No. of Clusters", ylab="Within Group Sum of Squares ")

Results1= cbind.data.frame(mtcars,cluster=m1$cluster)
Final1=Results1[order(Results1$cluster),]
