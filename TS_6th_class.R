library(ggplot2)
library(fpp2)
oildata<-window(oil, start=1996)
autoplot(oildata) +ylab("Oil (millions of tonnes)")+ xlab("Year")
#The Simplest of the exponential smoothing methods is called simple exponential
#smoothing method. This method is used for forecasting data when no clear trend or seasonality pattern
#is found.
#Simple expomential smoothing has a flat forecast equation or function ie. all forecast take the same value equal to the last
#level component.
oildata<-window(oil, start=1996)
fc<-ses(oildata, h=5)
fc

round(accuracy(fc),2)
autoplot(fc) + autolayer(fitted(fc), series = "Fitted")+ylab("Oil (millions of tons)") +xlab("Year")
summary(fc)
#The very small value of Beta says that the slope hardly changes over time.
air<-window(ausair, start = 1990)
fc<- holt(air, h=15)
fc2<-holt(air, damped = TRUE, phi = 0.9, h=15)
autoplot(air) + autolayer(fc, series = "Holt's method", PI= FALSE)+autolayer(fc2, series = "Damped Holt's method", PI= FALSE)+ ylab("Air Passengers in Australia(Millions)") +xlab("Year")+guides(colour=guide_legend(title = "Forecast"))
fc2<-holt(air, damped=TRUE) 
summary(fc2)
aust<-window(austourists, start=2005)
fit1<-hw(aust, seasonal="additive") #By default it is additive
fit2<-hw(aust,seasonal = "multiplicative")
summary(fit1)
summary(fit2)
#By checking MAPE, fit2 has less, so multiplicative will be preffered
autoplot(fit1)+autolayer(fit2)
autoplot(aust) + autolayer(fit1, series = "Additive", PI= FALSE)+autolayer(fit2, series = "Multiplicative", PI= FALSE)+ ylab("Australian Tourists") +xlab("Year")+guides(colour=guide_legend(title = "Forecast"))
fit1<-hw(aust, seasonal="additive", damped = TRUE, phi = 0.9, h=15)
fit2<-hw(aust,seasonal = "multiplicative", damped = TRUE, phi = 0.9, h=15 )
summary(fit1)
fit1<-hw(aust, seasonal="additive")
fit2<-hw(aust,seasonal = "multiplicative")
fit<-ets(aust)
summary(fit)
autoplot(fit)
