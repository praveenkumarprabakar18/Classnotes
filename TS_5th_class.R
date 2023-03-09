library(forecast)
library(fpp2)
library(ggplot2)
beer2<-window(ausbeer, start=1992)
autoplot(beer2)+xlab("Year")+ylab("Megalitres")
beer2
fit.beer<-tslm(beer2 ~ trend+season) #Use tslm when indep variables have categorical
summary(fit.beer)

#From the result you can conclude that there is a minus downward trend of -0.34 litres per quarter.
#On Average the second quarter has production of 34.7 megalitres lower than the 1st quarter.
#Similarly the 4th quarter has the production of 72.79 megalitres higher than the 1st quarter.

autoplot(beer2, series = "Data")+autolayer(fitted(fit.beer), series="Fitted")+xlab("Year") +ylab("Megalitres")+ ggtitle("Quarterly Beer Production")

#When there are many possible predictors, we need some strategy to use in the regression model
#A common approach which is invalid is to do Multilpe linear regression on all predictors and disregard all variables
#whose p-value is greater than 0.05. To start with statistical significance does not always indicate predictive
#value even if forecasting is not the goal, this is not the good strategy because the p-value
#can be misleading when 2 or more predictors are correlated with each other, instead we use measure of 
#predictive accuracy

#Measure of Predictive Accuracy methods -> CV, AIC, AICc, BIC, Adjusted R square

#The timeseries cross validation, is a general tool for determining the predictive ability of a model
#It is a 3 step process
#1. Remove observation t from the dataset and fit the model using remaining data, then compute the error for the omitted observation.
#2. Repeat step 1 for t=1,2...T
#3. Compute the MSE from e1*, e2*,...eT* and we will call this the cross validation.

#Although this looks like a time consuming method, there are fast methods for calcualting CV, so that
#it takes no longer than fitting 1 model to the full dataset.

#AIC = Tlog(SSE/T) + 2(k+2)
#BIC = Tlog(SSE/T) + (k+2)log(T)
#Adj R square -> 1-((SSE/DFE)/(SST/DFT))

#In ML models, people go for CV to reduce biasness and BIC as it encourages least number of indep variables

fit.consMR = tslm(Consumption ~ Income + Production + Savings + Unemployment, data=uschange)
summary(fit.consMR)
CV(fit.consMR)

fit.consMR2 = tslm(Consumption ~ Income + Savings + Unemployment, data=uschange)
summary(fit.consMR2)
CV(fit.consMR2)

#Like this remove each of the independent variables and try combination (16) and find the model with least CV

#EX - ante forecast are those that are made using only the information that is available in advance.
#For Example - Percentage change in US consumption for quarters following the end of the sample
#EX- Post forecast are those that are made using the later information on the predictors. It is backward looking and 
#it looks the result after they have already occurred.
#For example in investment companies, analysts can use historical return to forecast the probability of making profit or loss
#on an investment

#Suppose a US policy maker is interested in comparing a predicted change in consumption when there is a growth of 1%
#and 0.5% respectively for income and savings with no change in employment rate versus respective decline
#of 1% and 0.5% for each of the 4 quarters following the end of the sample.

#Income = [1,1,1,1]
#Savings = [0.5,0.5,0.5,0.5]
#Unemployment = [0,0,0,0]

fit.consBest <- tslm(Consumption ~ Income + Savings + Unemployment, data= uschange)
h<-4
uschange
newdata<-data.frame(Income = c(1,1,1,1), Savings = c(0.5,0.5,0.5,0.5), Unemployment = c(0,0,0,0))
fcast.up <-forecast(fit.consBest, newdata = newdata)

newdata<-data.frame(Income =rep(-1,h), Savings = rep(-0.5,h), Unemployment = rep(0,h))
fcast.down <-forecast(fit.consBest, newdata = newdata)

autoplot(uschange[,1]) +ylab("%change in US consumption")+autolayer(fcast.down, PI = TRUE, series= "decrease") + autolayer(fcast.up, PI = TRUE, series= "increase")+ guides(colour = guide_legend(title = "Scenario"))
