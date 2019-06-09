souvenir=read.csv("souvenir.csv")

souvenir_ts=ts(souvenir, frequency=12, start=c(1987,1))

adf.test(souvenir_ts)
?adf.test

ndiffs(souvenir_ts)

Sov_Train=souvenir_ts[1:72]
Sov_Train=ts(Sov_Train,  frequency=12, start=c(1987,1))
Sov_Train

Sov_test=souvenir_ts[73:84]
Sov_test=ts(Sov_test,   frequency=12, start=c(1993,1))
Sov_test

S2=SMA(Sov_Train,n=2)
S3=SMA(Sov_Train,n=3)
S4=SMA(Sov_Train,n=4)
S5=SMA(Sov_Train,n=5)
S6=SMA(Sov_Train,n=6)

F1=forecast(S2,h=12)
F2=forecast(S3,h=12)
F3=forecast(S4,h=12)
F4=forecast(S5,h=12)
F5=forecast(S6,h=12)

M=as.list(1)

for (i in 2:5) {
    M[[i]]=SMA(Sov_Train,n=i)
}

M[[2]]

MAPE=function(x,y)
{
  mean(abs((x-y)/x))
}
F1
names(F1)

MT_F1=MAPE(F1$x,F1$fitted)
MT_F2=MAPE(F2$x,F2$fitted)
MT_F3=MAPE(F3$x,F3$fitted)
MT_F4=MAPE(F4$x,F4$fitted)

Output=data.frame(Model_Details="a",MAPE_Train=0,MAPE_Test=0)

for (i in 2:10) {
Model_Details=paste("SMA",i)
fit_Train=SMA(Sov_Train,n=i)  
F1=forecast(fit_Train,h=12)
MAPE_Train=MAPE(F1$x,F1$fitted)

MAPE_Test=MAPE(Sov_test,F1$mean)

out=data.frame(Model_Details,MAPE_Train,MAPE_Test)
Output=rbind(Output,out)

}

#Exponential Smoothing

ES_All=HoltWinters(Sov_Train)
ES_L=HoltWinters(Sov_Train,beta = F,gamma = F)
ES_LS=HoltWinters(Sov_Train,beta = F)

F6=forecast(ES_All,h=12)
F7=forecast(ES_L,h=12)
F8=forecast(ES_LS,h=12)

plot(F6)

[length(Sov_Train)-length(na.omit(F6$fitted)):length(Sov_Train)]


library(randomForest)
na.roughfix(F6$fitted)

L=is.na(F6$fitted)
MAPE(F6$x[!L],F6$fitted[!L])

#ETS_Train_MAPE
out1=data.frame(Model_Details="ETS_ALL",
                MAPE_Train=MAPE(F6$x[(length(Sov_Train)-length(na.omit(F6$fitted)))+1:length(na.omit(F6$fitted))],
                                F6$fitted[(length(Sov_Train)-length(na.omit(F6$fitted)))+1:length(na.omit(F6$fitted))]),
                MAPE_Test=MAPE(Sov_test,F6$mean)
                )
out2=data.frame(Model_Details="ETS_L",MAPE_Train=MAPE(F7$x,F7$fitted),MAPE_Test=MAPE(Sov_test,F7$mean))
out3=data.frame(Model_Details="ETS_LS",MAPE_Train=MAPE(F8$x,F8$fitted),MAPE_Test=MAPE(Sov_test,F8$mean))

Output=rbind(Output,out1,out2,out3)       

#Test MAPE
MV_F1=MAPE(Sov_test,F1$mean)
MV_F2=MAPE(Sov_test,F2$mean)
MV_F3=MAPE(Sov_test,F3$mean)
MV_F4=MAPE(Sov_test,F4$mean)
MV_F5=MAPE(Sov_test,F5$mean)
MV_F6=MAPE(Sov_test,F6$mean)
MV_F7=MAPE(Sov_test,F7$mean)
MV_F8=MAPE(Sov_test,F8$mean)

Test_MAPE=c(MV_F1,MV_F2,MV_F3,MV_F4,MV_F5,MV_F6,MV_F7,MV_F8)
Train_MAPE=c(MT_F1,MT_F2,MT_F3,MT_F4,MT_F5,MT_F6,MT_F7,MT_F8)
Description=c("SM2","SM3",
              "SM4",
              "SM5",
              "SM6",
              "ES All",
              "ES Level",
              "ES Level & Season")
Model=paste("F",1:8,sep="_")
Output=data.frame(Model,Description,Train_MAPE,Test_MAPE)

#Arima
auto.arima(Sov_Train)
