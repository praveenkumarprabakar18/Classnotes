library(fpp2)
library(forecast)
library(ggplot2)
library(corrplot)
elecdemand
autoplot(elecdemand[, c("Demand", "Temperature")],facets=TRUE) + xlab("Year: 2014") +ylab("")+ggtitle("Half-hourly electricity demand: Victoria, Australia")
qplot(Temperature, Demand, data=as.data.frame(elecdemand)) + ylab("Demand (GW)") + xlab("Tempeaure(celcius")
cor(elecdemand)
visnights
?visnights
summary(visnights)
autoplot(visnights[,1:5], facets = TRUE)+ ylab("Number of visitor nights each quarter (millions)")
GGally::ggpairs(as.data.frame(visnights[,1:5]))
#From the figure, we can observe that the 2nd column of plot shows there is a strong positive relationship
#between visitors to the NSWNthCo and NSWSthCo but no deductable relationship between visitors to the NSWNthCO and
#visitors to the NSWSthIn
#Since there was olympics in 2001, there shows a correlation between NSWNorthCO, NSWSouthCo and NSWMetro that is why
#there is a 2nd peak. If it is removed, the correlation will become less
ausbeer
beer2<-window(ausbeer, start=1992)
gglagplot(beer2)
#Here the colors indicate the quarter of the variable on the vertical axis. The relationship is strongly
#positive at lag 4 and lag 8 reflecting the strong seasonality in the data. The negative correlation is also seen
#in lag2 and lag6 because peaks are plotted against truffs.
ggAcf(beer2) #corrologram

#R4 is higher than other lags This is due to the seasonal pattern in the data
#R2 is more negative than for the other lags because troughs tend to be 2 Quarters behind peaks
aelec<-window(elec, start=1980)
autoplot(aelec)+xlab("Year") +ylab("Gwh")
ggAcf(aelec, lag=48)
#A slow decrease ACF as the lags increases is due to the trend while the scalloped is due to the seasonality.
set.seed(30)
y<-ts(rnorm(50))
autoplot(y) +ggtitle("White Noise")
ggAcf(y)
#If in a timeseries data, there is no autocorrelation, it is called white noise.