library(fpp2)
library(ggplot2)
plot(goog200)
acf(goog200)
Box.test(goog200)
#p-val<0.05, reject H0, there is autocorrelation
#Make data stationary by Differencing
goog200DIFF1<-diff(goog200)
plot(goog200DIFF1)
acf(goog200DIFF1)
Box.test(goog200DIFF1)
#A seasonal difference is the difference between an observation and previous observation
#from the same season
plot(a10)
library(urca)
cbind("Sales($million)"= a10, "Monthly log sales"=log(a10), "Annual change in log sales"=diff(log(a10), 12) %>% autoplot(facets=TRUE)+xlab("Year")+ylab("")+ggtitle("Antidiabetic drug sales"))
loga10=log(a10)
plot(loga10)
test1=ur.kpss(loga10)
summary(test1)
test2=ur.kpss(diff(loga10))
summary(test2)
kings<-scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip = 3)
kings
plot(kings)
kings_ts<-ts(kings)
plot(kings_ts)
test3<-ur.kpss(kings_ts)
summary(test3)
#The process of using sequence of kpss test to determine the appropriate
#number of 1st difference is carried out by the function ndiffs
#A similar function for determining whether a seasonal differencing is required is nsdiffs
ndiffs(kings_ts) #For trend
nsdiffs(kings_ts) #For seasonality
diff_kings<-diff(kings_ts)
test4<-ur.kpss(diff_kings)
summary(test4)
acf(diff_kings) #To know the order of MA (only 2nd falls outside after 1st, so order =1)
pacf(diff_kings) # 3 lines fall outside, order 3
auto.arima(kings_ts) #Suggests a model with AICC values minimum
