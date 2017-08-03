# Packages
library()
t=read.csv("C:\\Users\\user\\Desktop\\tRAINING\\Tiser.csv",header=T)
t=t[,2:3]
head(t)
dim(t)

tg=ts(t$Demand,frequency=12)
tg
plot.ts(tg)
plot.ts(log(tg))
#Stationarity test
adf.test(tg)
plot(log(tg))

#ACF and PACF plot
acf(tg)
pacf(tg)
pacf(log(tg))


#Decomposing Timeseries data
dec=SMA(tg,n=2)
plot.ts(dec)

dr=decompose(tg,type="multiplicative")
plot(dr)
dr$trend
dr$seasonal
dr$random
l=c(dr$random,na.rm=T)
l=na.omit(dr$random)
l
adf.test(l)
plot.ts(l)
#Non transformed
y1=Arima(tg,order=c(1,1,2),method="CSS")
y2=Arima(tg,order=c(1,0,1))
y3=Arima(tg,order=c(1,2,1))
y4=Arima(tg,order=c(1,2,3))
y5=Arima(tg,order=c(1,2,2))

y6=Arima(tg,order=c(4,1,3))
y7=arima(tg,order=c(2,2,2))
summary(Arima(tg,order=c(4,1,3)))
accuracy(y1)
#MAE And MAPE values
summary(y1)
summary(y2)
summary(y3)
summary(y4)
summary(y5)
summary(y6)
accuracy(y6)
arima
y6$fitted
#Predicted value
f1=y1$fitted
f2=y2$fitted
f3=y3$fitted
f4=y4$fitted
f5=y5$fitted
f6=y6$fitted

#BIAS
bias1=tg-f1
bias2=tg-f2
bias3=tg-f3
bias4=tg-f4
bias5=tg-f5
bias6=tg-f6


#Mean absolute deviation
mad1=mean(abs(f1-tg))
mad1
mad2=mean(abs(f2-tg))
mad2
mad3=mean(abs(f3-tg))
mad3
mad4=mean(abs(f4-tg))
mad4
mad5=mean(abs(f5-tg))
mad5
mad6=mean(abs(f6-tg))
mad6

x=rbind(mad1,mad2,mad3,mad4,mad5,mad6)
x
library(forecast)
auto.arima(tg)
fc1=forecast(y1,h=12)
fc2=forecast(y2,h=12)
fc3=forecast(y3,h=12)
fc4=forecast(y4,h=12)
fc5=forecast(y5,h=12)
fc6=forecast(y6,h=12)
plot(fc1)
plot(fc2)
plot(fc3)
plot(fc4)
plot(fc5)
plot(fc6)
rep
tg
ramp=c(-4,-3,-2,-1,rep(0,53),-1,-2,-3)
ramp


tse=data.frame(tg,ramp)
tse
tsr=ts(tse$tg,freq=12)
tsr=data.frame(cbind(tsr,ramp))

plot.ts(tsr,ramp)
reg= arimax(tg,order=c(4,1,3),xreg=tsr$ramp)
summary(reg)
accuracy(reg)
r=fitted(reg)
x=data.frame(r)
x
mape=mean(abs(x-tg)/tg)*100
mape
forc=forecast(reg,h=12,xreg=x$r)
forecast
reg$fitted

data(airmiles)
plot(log(airmiles),ylab='Log(airmiles)',xlab='Year', main='')
acf(diff(diff(window(log(airmiles),end=c(2001,8)),12)),lag.max=48,main='')
air.m1=arimax(log(airmiles),order=c(0,1,1),seasonal=list(order=c(0,1,1),
                                                         period=12),xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                                                                       I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(1,0)),xreg=data.frame(Dec96=1*(seq(airmiles)==12),
                                                           Jan97=1*(seq(airmiles)==13),Dec02=1*(seq(airmiles)==84)),method='ML')


trend = ma(y, order = 12, centre = T)
