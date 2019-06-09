
#TTR, forecast, tseries
library(TTR)
library(forecast)
library(tseries)


rm(list = ls())

par(mfrow=c(1,1))


setwd("C:/Users/Avadhoot/Desktop/Data science with R/Excel reporting-29-12/given data/")
kings=read.csv("Kings.csv")
kings
View(kings)
plot(kings)
plot(kings,type="l")

?ts
?plot.ts
plot.ts(kings) #need chart intimeseries format
kings_ts=ts(kings) #first step to do 
View(kings_ts)
plot.ts(kings_ts)
library(TTR)
?SMA
a=SMA(kings_ts,n=3)
a=SMA(kings_ts)# n is lags, default value for n is 10
b=forecast(a,h=12)
b$fitted
b


a1=SMA(kings_ts,n=3)
b1=forecast(a1,h=12)
b1$fitted

a2=SMA(kings_ts,n=4)
b2=forecast(a2,h=12)
b2$fitted

a3=SMA(kings_ts,n=5)
b3=forecast(a3,h=12)
b3$fitted


a4=SMA(kings_ts,n=8)
b4=forecast(a4,h=12)
b4$fitted

a5=SMA(kings_ts,n=12)
b5=forecast(a5,h=12)
b5$fitted

a6=SMA(kings_ts,n=15)
b6=forecast(a6,h=12)
b6$fitted


names(b)
b$fitted
b$x #actual values
b$residuals
b$mean
b

c=forecast(kings_ts,h=12) # expoential smoothing
c$mean
c

births=read.csv("births.csv")
View(births)

births1=ts(births)
plot.ts(births1)
?ts
births_ts=ts(births, frequency=12, start=c(1946, 3))# frequency =1 then yearly value
#12 as MOnthly value start=c(1946, 3) data starting from 1946 march
births_ts
plot.ts(births_ts)

births_ts=ts(births, frequency=4, start=c(1946, 1)) #Qtrly
births_ts

births_ts=ts(births, frequency=52, start=c(1946, 1)) #weekly
births_ts

births_ts=ts(births, frequency=12, start=c(1946, 2))
births_ts

plot.ts(births_ts)

birthsDecomp=decompose(births_ts)
names(birthsDecomp)
plot(birthsDecomp)

birthsDecomp$trend
library(forecast)

a2=forecast(births_ts,h=36)
a2$mean
names(a2)
summary(a2)
plot.ts(a2$mean)
plot(a2)

-----

#Simple Moving average
  
  install.packages("TTR")
  library(TTR)
?SMA
kings_tsSMA3=SMA(kings_ts,n=3)
par(mfrow=c(1,2))

plot.ts(kings_ts)
plot.ts(kings_tsSMA3)

kings_tsSMA8=SMA(kings_ts,n=8)
plot.ts(kings_tsSMA8)

install.packages("forecast")
library(forecast)
?forecast

SMA_F=forecast(kings_ts,h=3)

SMA_F
plot(SMA_F)

a=forecast(kings_tsSMA8,h=3)
a
names(a)
names(SMA_F)
SMA_F$model
SMA_F$mean
SMA_F$fitted #forecast
SMA_F$x #Actual
SMA_F$residuals

a$fitted

SMA_F$x - SMA_F$fitted #Error
(SMA_F$x - SMA_F$fitted)/SMA_F$x #PE
abs(SMA_F$x - SMA_F$fitted)/SMA_F$x #APE
mean(abs(SMA_F$x - SMA_F$fitted)/SMA_F$x) #MAPE

a$x - a$fitted #Error
(a$x - a$fitted)/SMA_F$x #PE
abs(a$x - a$fitted)/SMA_F$x #APE
mean(abs(a$x - a$fitted)/SMA_F$x) #MAPE

MAPE=0
for (i in 2:15)
{
  a1=SMA(kings_ts,n=i)
  a=forecast(a1,h=3)
  MAPE[i]=mean(abs(a$x - a$fitted)/a$x) #MAPE actual - fitted by actual
}
MAPE

mapebirth=0
for(i in 2:15)
{
  bir=SMA(births_ts,n=i)
  bi=forecast(bir,h=3)
  mapebirth[i]=mean(abs(bi$x-bi$fitted)/bi$x)
}

# (bi$x-bi$fitted) -- DEVIATION
# (bi$x-bi$fitted)/bi$x) -- PERCENT DEVIATION

mapebirth

plot(mapebirth, type="l")

plot(MAPE, type="l")

?append
kings_F=append(kings_ts,SMA_F$mean,after=length(kings_ts))


plot.ts(kings_F)
plot(SMA_F)
plot(SMA_F$fitted)

#Exponential smoothing

rain=read.csv("rain.csv")
View(rain)
rainseries=ts(rain, start=1813,frequency=12)
plot.ts(rainseries)
?ts
#alpha = level, beta = slope/trend, gamma=season
#values between 0 to 1

frequency(rainseries)

Mr=forecast(rainseries)
Mr$mean
summary(Mr)
?HoltWinters

#OP1 - All components (1859)
rainseriesforecasts=HoltWinters(rainseries)
names(rainseriesforecasts)
rainseriesforecasts
rainseriesforecasts$SSE

#OP2 - Level component only (1828)
rainseriesforecasts=HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
names(rainseriesforecasts)
rainseriesforecasts$SSE

#OP3 - level & trend (1784)
rainseriesforecasts=HoltWinters(rainseries, gamma =FALSE)
rainseriesforecasts

rainseriesforecasts$SSE

#OP4 - level & season (1784)
rainseriesforecasts=HoltWinters(rainseries, beta=FALSE)
rainseriesforecasts

rainseriesforecasts$SSE

plot(rainseriesforecasts$fitted)

rainseriesforecasts2=forecast(rainseriesforecasts,h=8)
rainseriesforecasts2
names(rainseriesforecasts2)
plot(rainseriesforecasts2)

#calculate mape and forecast

souvenir=read.csv("souvenir.csv")

souvenir_ts=ts(souvenir, frequency=12, start=c(1987,1))
souvenir_ts

plot.ts(souvenir_ts)
library(tseries)

adf.test(souvenir_ts)
?adf.test

ndiffs(souvenir_ts)

ndiffs(logsouvenir)

logsouvenir=log(souvenir_ts)
plot.ts(logsouvenir)

adf.test(logsouvenir)

souvenir_forecasts = HoltWinters(logsouvenir)
souvenir_forecasts

plot(souvenir_forecasts)

ndiffs(souvenir_ts)
ndiffs(logsouvenir)

library(forecast)
souvenir_forecasts2 = forecast(souvenir_forecasts, h=48)
plot(souvenir_forecasts2)

souvenir_forecasts2

# ---ARIMA--
  #checking stationarity, install pacakge tseries
  #methods adf.test and kpss.test
  
  
  install.packages("tseries")
library(tseries)

adf.test(kings_ts)
#Null hypothesis - data is not stationary, need low p value

kpss.test(kings_ts)
#Null hypothesis - data is stationary, need high p value
  
adf.test(souvenir_ts)
adf.test(logsouvenir)
?ndiffs
ndiffs(souvenir_ts)
?arima
?auto.arima

arima(souvenir_ts)

auto.arima(souvenir_ts)

a1=auto.arima(souvenir_ts)

a2=auto.arima(logsouvenir)
#AIC low to be selected

summary(a1)
summary(a2)
f1=forecast(a1,h=48)
f1
plot(f1)
acf(souvenir_ts) # auto correlation factor
pacf(souvenir_ts) #partial autocorrelation factor

acf(logsouvenir)
pacf(logsouvenir)

?arima

000
100
101
110
010
011
?arima

arima(souvenir_ts, order=c(1,1,1))
arima(souvenir_ts, order=c(1,1,1),seasonal=c(0,1,1))

AR1=arima(souvenir_ts, order=c(1,1,1),seasonal=c(0,1,1))

ARL1=arima(logsouvenir, order=c(2,0,0),seasonal=c(1,1,0))
names(ARL1)

ARL1$aic
#--Looping--

x=c(1,2,4,5,10,80,192)
    
  #for values of iterator it will perform operation mentioned in curly brackets
  #
  for (itr in 1:10)
  {
    print(x[itr]^2)
    
  }


##looping for arima

pdq=c(000)
AIC=c(10000)
out=data.frame(pdq,AIC)
View(out)

fit=arima(souvenir_ts,c(0,1,0))
fit$aic


for (p in 0:2)
{
  for(q in 0:2)
  {
    fit=arima(souvenir_ts,c(p,1,q))
    AIC= fit$aic
    new=data.frame(pdq=paste(p,1,q),AIC=AIC)
    out=rbind(out,new)
  }
}



View(out)
out[out$AIC==min(out$AIC),]
#----------------------

so=read.csv("souvenir.csv")

aarima=auto.arima(so)
summary(aarima)
# ARIMA(1,1,1)
# AIC=1813.14
pdq1=c(000)
AIC1=c(10000)
out1=data.frame(pdq1,AIC1)
View(out1)

for (p in 0:1) {
  for (d in 0:1) {
    for (q in 0:1) {
      mod=arima(so,c(p,d,q))
      AIC= mod$aic
      new1=data.frame(pdq1=paste(p,d,q),AIC1=AIC)
      out1=rbind(out1,new1)
    }
    
  }
  
}

View(out1)
out[out1$AIC==min(out1$AIC),]

pdq2=c(000000)
AIC2=c(10000)
out2=data.frame(pdq2,AIC2)
View(out2)


for (p in 0:1) {
  for (d in 0:1) {
    for (q in 0:1) {
      for (P in 0:1) {
        for (D in 0:1) {
          for (Q in 0:1) {
            mod1=arima(so,order= c(p,d,q),seasonal = c(P,D,Q))
            AIC= mod1$aic
            new2=data.frame(pdq2=paste(p,d,q,P,D,Q),AIC2=AIC)
            out2=rbind(out2,new2)
          }
        }
       }
      }
    }
}

View(out2)
out2[out2$AIC2==min(out2$AIC2),]

names(fit)
forecast(fit)

out=out[-1,]
View(out)

#holt winters : souvenir_forecasts2
Sov_Train=souvenir_ts[1:63]
Sov_Train=ts(Sov_Train,  frequency=12, start=c(1987,1))

Sov_Train
Sov_test=souvenir_ts[64:84]
Sov_test=ts(Sov_test,   frequency=12, start=c(1992,4))
Sov_test

#Build models using simple moving average
?SMA
MA1=SMA(Sov_Train,n=1)
MA2=SMA(Sov_Train,n=2)
MA3=SMA(Sov_Train,n=3)
MA4=SMA(Sov_Train,n=4)
MA5=SMA(Sov_Train,n=5)

FMA1=forecast(MA1,h=21)
FMA2=forecast(MA2,h=21)
FMA3=forecast(MA3,h=21)
FMA4=forecast(MA4,h=21)
FMA5=forecast(MA5,h=21)

#holt winters : souvenir_forecasts2
souvenir_forecasts = HoltWinters(Sov_Train)
HWF=forecast(souvenir_forecasts,h=21)

#Build ARIMA models

autoarima=auto.arima(Sov_Train)
summary(autoarima)
AR100 = arima(Sov_Train,order=c(1,0,0))
FAR100=forecast(AR100,h=21)

AR100_110=arima(Sov_Train,order=c(1,0,0),seasonal=c(1,1,0))
FAR100_110=forecast(AR100_110,h=21)
ndiffs(Sov_Train)
a=c(0)
b=c(10000)
out=data.frame(a,b)

for (p in 0:2)
{
  for(q in 0:2)
  {
    fit=arima(Sov_Train,c(p,1,q))
    AIC= fit$aic
    new=data.frame(a=paste(p,1,q),b=AIC)
    out=rbind(out,new)
  }
}

out$a[3]

min(out$b)

AR011 = arima(Sov_Train,order=c(0,1,1))
FAR011=forecast(AR011,h=21)
names(FAR011)
FAR011$mean
Sov_test

mean(abs((FAR011$mean-Sov_test)/Sov_test))


ex=read.csv("souvenir_ex.csv")
ex_Train=ex[1:63,c(2,3)]
ex_Train=ts(ex_Train)

ex_test=ex[64:84,c(2,3)]
ex_test=ts(ex_test)
Sov_test

?arima
AR_x=arima(Sov_Train,order=c(1,1,0),seasonal=c(1,1,0),xreg = ex_Train)
AR_x$aic

ARF_x=forecast(AR_x,xreg = ex_test,h=21)
ARF_x

AR_x1=arima(Sov_Train,order=c(1,1,0),seasonal=c(1,1,0),xreg = ex_Train[,1])
AR_x1$aic

ARF_x1=forecast(AR_x1,xreg = ex_test[,1],h=21)
ARF_x1

AR_x2=arima(Sov_Train,order=c(1,1,0),seasonal=c(1,1,0),xreg = ex_Train[,2])
AR_x2$aic

ARF_x2=forecast(AR_x2,xreg = ex_test[,2],h=21)
ARF_x2

MAPE=function(a,b)
{
mean(abs((a-b)/a))
}

MAPE(Sov_test,ARF_x$mean)
