library(ggfortify)
library(zoo)
library(tseries)
library(astsa)
library(forecast)
library(ggplot2)
library(fpp2)
data("AirPassengers")  
AirPassengers
plot(AirPassengers, main="Air Passengers Dataset") #plot to know the nature, main is for title
decomposed_air_passengers<-decompose(AirPassengers, type='multiplicative')
autoplot(decomposed_air_passengers)
ggseasonplot(AirPassengers)
decomposed_air_passengers$figure #gives seasonality index
ggseasonplot(decomposed_air_passengers$seasonal)
ggseasonplot(AirPassengers, polar = TRUE)
ggsubseriesplot(AirPassengers, main="Seasonal Subseries for Air passengers")
#######
births<-scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births
plot(births, main="Birth Data")
birthtimeseries<-ts(births, frequency = 12, start = c(1946,1))
birthtimeseries
plot(birthtimeseries)
decomposed_birth<-decompose(birthtimeseries)
autoplot(decomposed_birth)
birthtsseasonaladjusted<-birthtimeseries-(decomposed_birth$seasonal)
birthtsseasonaladjusted
plot(birthtsseasonaladjusted)
