#As the name suggests ARIMA supports both AR and MA elements. Integrated elements refers to differencing 
#allowing the method to support Time Series data with trend.

#A problem with ARIMA is that it doesn't support seasonal data that is a time series
#with repeating data

#In case of SARIMA, it adds 3 new hyper parameters to specify autoregression AR, differencing I and 
#moving average MA for the seasonal component of the time series


library(astsa)
library(fpp2)
library(ggplot2)
plot(prodn)
acf2(prodn)
acf2(diff(prodn), 48)
acf2(diff(diff(prodn), 12), 48)
#Model SARIMA (2,1,0)(0,1,3)
sarima(prodn,20, 2,1,0, 0,1,3,12)
auto.arima(prodn)
autoplot(euretail) + ylab("Retail Index") +xlab("Year")
nsdiffs(euretail) #For seasonality
ndiffs(euretail) #For trend
#auto.arima(euretail)
euretail %>% diff(lag=4) %>%ggtsdisplay()
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()
euretail %>% arima(order=c(0,1,1), seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
euretail %>% arima(order=c(0,1,2), seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
euretail %>% arima(order=c(0,1,3), seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
#Now it becomes white noise
auto.arima(euretail)
