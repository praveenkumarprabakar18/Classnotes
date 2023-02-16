#SimpleForecastingmethods
library(forecast)
library(ggplot2)
library(fpp2)
beer2<-window(ausbeer, start=1992, end=c(2007,4))
beer2
y<-meanf(beer2, h=10) #Forecasting using Mean Average method
y
autoplot(beer2)+ autolayer(meanf(beer2, h=11), series="Mean", PI=FALSE) + autolayer(naive(beer2, h=11), series = "Naive", PI=FALSE)+ autolayer(snaive(beer2, h=11), series="Seasonal Naive", PI=FALSE) + ggtitle("Forecasts for Quarterly beer production")+xlab("Year") + ylab("Megalitres")+guides(colour=guide_legend(title="Forecast"))

autoplot(goog200)+ autolayer(meanf(goog200, h=11), series="Mean", PI=FALSE) + autolayer(naive(goog200, h=11), series = "Naive", PI=FALSE)+ autolayer(snaive(goog200, h=10), series="Seasonal Naive", PI=FALSE) + autolayer(rwf(goog200, h=11))+  ggtitle("Forecasts for Quarterly beer production")+xlab("Year") + ylab("Megalitres")+guides(colour=guide_legend(title="Forecast"))

autoplot(milk)
dframe<-cbind(Monthly = milk, DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE)+ xlab("Years") +ylab("Pounds") +ggtitle("Milk production per cow")

lambda<- BoxCox.lambda(elec)
lambda
autoplot(elec)
autoplot(BoxCox(elec, lambda))

autoplot(goog200)+xlab("Day") + ylab("Closing Price (US$)")+ggtitle("Google Stock (daily ending & December 2013)")
res<-residuals(naive(goog200))
res
plot(res)
max(res)
gghistogram(res)
ggAcf(res)

#These graphs show that the naive method produces forecast that appear to account for all the available information
#The mean of the residuals is close to zero and there is no significant correlation in the residual series.
#The Time plot of the residuals shows that th variation of the residuals stays much the same across the historical data
#apart from the one outlier and therefore the resdual variance can be treated as constant. The histogram suggests
#that the residuals may not be normal. The right tail seems a little too long. Consequently forecast from
#this method will probably be quite good. But prediction intervals that are computed assuming normal distribution may be inaccurate.

#Portmanteau test for autocorrelation 
q1<-Box.test(res, lag=10, fitdf=0)
q2<-Box.test(res, lag=10, fitdf=0, type="L")
#For both q1 and q2, the results are not significant. Thus we can conclude that the residuals are not distinguishable.
#from a white noise series

checkresiduals(naive(goog200))
