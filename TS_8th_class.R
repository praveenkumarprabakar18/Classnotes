library(fpp2)
library(ggplot2)
autoplot(uschange[,"Consumption"]) + xlab("Year")+ylab("Quarterly Percentage change")

fit<-auto.arima(uschange[,"Consumption"], seasonal=FALSE, trace=TRUE)
fit
fit %>% forecast(h=10) %>% autoplot(include=80)
ggAcf(uschange[,"Consumption"])
ggPacf(uschange[,"Consumption"])
#If the data are from ARIMA(p,d,0) or ARIMA(0,d,q) then the ACF and PACF plots
#can be helpful in determining the value of p or q
#If p and q are both positive, then the plots do not help in finding suitable
#values of p and q
#The data may follow a ARIMA(p,d,0) if ACF and PACF plots of the 
#Differenced data show the following pattern
#1. ACF is exponentially decaying or sinusoidal
#2. There is a significant spike at lag p in the PACF but none beyond lag P
#The data may follow an ARIMA(0,d,q) if the ACF and PACF plots of the 
#Differenced data show the following patterns
#1. The PACF is exponentially decaying
#2. There is a significant spike in the lag q in the ACF but none beyond lag q
fit2<-arima(uschange[,"Consumption"], order=c(3,0,0))
fit2 %>% forecast(h=10) %>% autoplot(include=80)
fit2
fit3<-auto.arima(uschange[,"Consumption"], seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
fit3

elecequip
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(elecequip)
ggAcf(eeadj)
#Trend is still there, that is why ACF plot is linerarly decaying
ggPacf(eeadj)
eeadj %>% diff() %>% ggtsdisplay(main=" ")
fit4<-arima(eeadj, order=c(3,1,1))
fit4
#Diagnostic checking to be done if model is good, need to check residuals for that
checkresiduals(fit4)
#In ACF, all are falling inside, so it is white noise process
#The stationarity condition for the model ARIMA(p,q) is that p complex root of phi(B) lie outside
#the unit circle and the invertibility condition are that the q omplex roots of Teta(B)
#lies outside the unit circle
autoplot(fit4)
auto.arima(eeadj)
