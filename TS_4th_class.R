#Evaluating forecast accuracy
#The size of the test set is typically above 20% of the total sample. Allthough this value
#depends on how long the sample is and how far ahead you want to forecast. The test set should 
#ideally be atleast as large as the maximum forecast horizon required.
#(Training data for validation and testing data for forecast).
#The Following point should be noted.
#A perfect fit can always be obtained by using a model with enough parameters. A model which
#fits the training data well will not necessairily forecast well.
#Forecast error = e T+h= y T+h - y^ T+h/T
#Evaluating Forecast Accuracy with different methods. Please refer notebook.
library(ggplot2)
library(forecast)
library(fpp2)
beer2<-window(ausbeer, start=1992, end=c(2007,4))  
beerfit1<-meanf(beer2, h=10)
beer3<-window(ausbeer, start=2008)
accuracy(beerfit1,beer3)
beerfit2<-rwf(beer2, h=10)
accuracy(beerfit2, beer3)
beerfit3<-snaive(beer2, h=10)
accuracy(beerfit3, beer3)
#On comparing Error terms for test set for various models, it is found that snaive is the best as it gives 
#the least error.

#FORECASTING USING REGRESSION
uschange
autoplot(uschange[,c("Consumption", "Income")]) +ylab("% change") +xlab("Year")
tslm(Consumption ~ Income, data = uschange)
uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

fit.consMR<-tslm(Consumption ~ Income + Production + Savings + Unemployment, data=uschange)
summary(fit.consMR)
#For forecasting purpose, the final 2 columns are of limited interest. That is pvalue
#and tvalue. This is useful when studying the effect of each predictor but is not particularly
#useful for forecasting
checkresiduals(fit.consMR)
#The figure shows that a timeplot, the ACF and the histogram of the residuals from the
#multiple regression model fitted to the US quarterly consumption data. As well as 
#biyush godflay test for jointly testing upto the 8th order of autocorrelation. The timeplot
#shows some changing variations over time but is otherwise relatively small.This hetroscedasity will
#potential make the prediction interval coverage inaccurate. The histogram shows that the
#residuals seem to be slightly skewed which may also affect the coverage probability of 
#the prediction intervals. The autocorrelation plot shows that a significant spike at lag 7.But it is not
#quiet enogh for biyush godflay to be significant at the 5% level. In any case the autocorrelation
#is not particularly large and at lag 7 it is unlikely to have any noticeable impact on the forecast
#or on the prediction interval.