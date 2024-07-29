sink()
#loading required library
library(quantmod)
library(lmtest)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(xts)
library(tidyverse)
library("feasts") 
library("fable") 
library("lubridate")
library("gridExtra")
library(tseries)
library(forecast)
library(rugarch)
library(tseries)
library(rugarch)
library(xts)
library(readr)
library(ggpubr)

#import data
data<- read.csv("C:\\Users\\Omowunmi\\Documents\\Laptop 1\\My project\\projectdata.csv",header=T)
View(data)
summary(data)

#converting data to xts
str(data)
data$Date <- as.Date(data$Date, format="%d %m Y")
tail(data)
data <- read.csv(file = "C:\\Users\\Omowunmi\\Documents\\Laptop 1\\My project\\projectdata.csv")
data$Date <- as.Date(data$Date, format="%d-%m-%Y")
tail(data)
str(data)

#closing price graph
DATA <- xts(data[,-1], order.by = data$Date)
str(DATA)
head(DATA)
chartSeries(DATA, type="candlesticks")

#stationary test for closing price
adf.test(DATA$Price, k=0)

#calculating returns
return=CalculateReturns(DATA$Price,method = 'log')
return = return[-c(1),]
View(return)
summary (return)
skewness(return)
kurtosis(return)
sd(return)
a= jarque.bera.test(return)

#Stationarity test for returns
adf.test(return$Price,k=0)

#plot time series of returns to show volatility
chartSeries(return)

#plot the histogram of returns to show normality
chart.Histogram(return,method=c('add.density','add.normal'),
                colorset = c('blue','red','black'))
legend("topright",legend = c("return","kernel","normal dist"),
       fill=c('blue','red','black'))
skewness(return); kurtosis(return)


#calculate annualized volatility returns
Return.annualized(return)
StdDev.annualized(return)
chart.RollingPerformance(R = return, width = 12, FUN = "Return.annualized",main="Nigeria Annualized Returns")
chart.RollingPerformance(R = return, width = 12, FUN = "StdDev.annualized",main="Nigeria Annualized Returns")

#model fitting symmetric garch model
#Garch(1,1)
model1 <- ugarchspec(variance.model = list(model="sGARCH",
                                           garchOrder=c(1,1)),mean.model =list(armaOrder=c(0,0))
                     ,distribution.model = "sstd")
Garch1 <- ugarchfit(spec = model1,data=return)
Garch1
plot(Garch1, which='all')
#Garch(2,1)
model2 <- ugarchspec(variance.model = list(model="sGARCH",
                                           garchOrder=c(1,2)),mean.model =list(armaOrder=c(0,0))
                     ,distribution.model = "sstd")
Garch2 <- ugarchfit(spec = model2,data=return)
Garch2
plot(Garch2, which='all')
#Garch(1,2)
model3 <- ugarchspec(variance.model = list(model="sGARCH",
                                           garchOrder=c(2,1)),mean.model =list(armaOrder=c(0,0))
                     ,distribution.model = "sstd")
Garch3 <- ugarchfit(spec = model3,data=return)
Garch3
plot(Garch3, which='all')
#Garch(2,2)
model4 <- ugarchspec(variance.model = list(model="sGARCH",
                                           garchOrder=c(2,2)),mean.model =list(armaOrder=c(0,0))
                     ,distribution.model = "sstd")
Garch4 <- ugarchfit(spec = model4,data=return)
Garch4
plot(Garch4, which= 'all')
###Garch in mean 
model5 <- ugarchspec(mean.model = list(armaorder=c(0,0)),
                     variance.model = list(model='sGARCH'),distribution.model = "sstd")
GarchM <- ugarchfit(spec = model5,data=return)
GarchM
plot(GarchM, which='all')

#TGARCH(1,1) GJR
model6 <- ugarchspec(variance.model = list(model="gjrGARCH",
                                           garchOrder=c(1,1)),mean.model =list(armaOrder=c(0,0))
                     ,distribution.model = "sstd")
TGarch <- ugarchfit(spec = model6,data=return)
TGarch
plot(TGarch, which='all')
#EGARCH(1,1)
model7 <- ugarchspec(variance.model = list(model="eGARCH",
                                           garchOrder=c(1,1)),mean.model =list(armaOrder=c(0,0))
                     ,distribution.model = "sstd")
EGarch <- ugarchfit(spec = model7,data=return)
EGarch
plot(EGarch, which='all')
#PGARCH

model8 <- ugarchspec(variance.model = list(model="apARCH",
                                           garchOrder=c(1,1)),mean.model =list(armaOrder=c(0,0))
                     ,distribution.model = "sstd")
PGarch <- ugarchfit(spec = model8,data=return)
PGarch
plot(PGarch, which='all')
#Forecasting

#forecasting garch(1,1)
for.model1 <- ugarchforecast(fitORspec = Garch1, n.ahead = 90)
print(for.model1)
#rolling forecast
fit_roll1 <- ugarchfit(Garch1, data=return,out.sample =500)
fore_roll1 <- ugarchforecast(fit_roll1, n.ahead=20, n.roll=50)
fore_roll1

foreroll1 <- ugarchroll(model1, return, n.ahead = 1, forecast.length = 500)
foreroll1
accuracy(foreroll1)
           
acc.model1 <- accuracy(Garch1,for.model1,test.ts)
accuracy(Garch1,foreroll1)
#forecasting garch(1,2)
for.model2 <- ugarchforecast(fitORspec = Garch2, n.ahead = 90)
print(for.model2)
#forecasting garch(2,1)
for.model3 <- ugarchforecast(fitORspec = Garch3, n.ahead = 90)
print(for.model3)
#forecasting garch(2,2)
for.model4 <- ugarchforecast(fitORspec = Garch4, n.ahead = 90)
print(for.model4)
#forecasting garch in mean
for.model5 <- ugarchforecast(fitORspec = GarchM, n.ahead = 90)
print(for.model5)
#forecasting Tgarch
for.model6 <- ugarchforecast(fitORspec = TGarch, n.ahead = 90)
print(for.model6)
#forecasting Egarch
for.model7 <- ugarchforecast(fitORspec = EGarch, n.ahead = 90)
print(for.model7)
#forecasting Pgarch
for.model8 <- ugarchforecast(fitORspec = PGarch, n.ahead = 90)
print(for.model8)
