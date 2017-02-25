library (forecast)
library (zoo)
library (fBasics)
library (tseries)
library (lmtest)
library (stats)
library (dplyr)
library (ggplot2)
library (openxlsx)
library (fUnitRoots)

#reading the datafile 
df = read.csv("rawcar.csv")
#Creating a time series for automobile sales
sales_ts = ts(df$TOTALNSA, start=c(1976, 1), freq = 12)

################### Exploratory Analysis ###########################
basicStats(sales_ts)
#Plot the time series
plot(sales_ts, ylab = 'No. of Automobile sold', xlab = 'Time (in Years)', main = 'Time Series - Automobiles sold')
# Perform Jarque-Bera normality test.
normalTest(sales_ts,method=c("jb")) 
#Histogram 
hist(sales_ts, main = "Histogram of Automobile Sales", xlab = 'Number of Automobiles')
#QQ Plot
qqnorm(sales_ts)
qqline(sales_ts, col = 'red')
# Dickey Fuller
adfTest(sales_ts, lags =8, type =c("c"))
adfTest(sales_ts, lags =10, type =c("c"))


hist(sales_ts, main = "Histogram of Automobile Sales", xlab = 'Number of Automobiles')
#QQ Plot
qqnorm(log(sales_ts))
qqline(log(sales_ts), col = 'red')

'''
library(dygraphs)
dygraph(sales_ts,ylab="Number of Automobile Sold (In Thousands)",        ) %>%
  dyOptions(colors = "red") %>%
  dyRangeSelector()
'''

#Acf plot
#Acf indicates highly correlated data
acf(coredata(sales_ts), main = 'ACF - Automobile Sales', lag = 60)
#to look for seasonality
sales.stl = stl(sales_ts, s.window = 'per')
plot(sales.stl)

#Differencing of the ts
dif_sales = diff(sales_ts)
#plotting the time series
#Plot shows that the series is not stationary, no seasonality
plot(dif_sales, main = 'Time Series - First Differencing', xlab = 'Time (in Years)', ylab = 'Differencing of Automobiles')
#Acf & PACF with first differencing to analyze seasonality, no seasonality indicated
acf(coredata(dif_sales), main = 'ACF - First Differencing')
pacf(coredata(dif_sales), main = 'PACF - First Differencing')

# Dickey Fuller
adfTest(dif_sales, lags =6, type =c("ct"))
adfTest(sales_ts, lags =3, type =c("ct"))


############################ Dicky Fuller #############################


adfTest(sales_ts,lag=3,type='ct')
adfTest(sales_ts,lag=5,type='ct')
#We fail to reject the null hypothesis of unit root non stationary 

########################### Model Creation ########################
sample_fit = auto.arima(sales_ts, trace = T, max.p = 8, max.q = 8, stationary = FALSE, seasonal = T,
                ic = c('bic'))

 #First model based on Auto Arima function (0,1,1)(2,0,0)
model_1 = Arima(sales_ts, order=c(0,1,1),seasonal=list(order=c(2,0,0),period=12))
coeftest(model_1)

#ACF for Residuals
acf(coredata(model_1$residuals), main = 'ACF - Residuals')
pacf(coredata(model_1$residuals), main = 'ACF - Residuals')
#Ljung Box Test
Box.test(model_1$residuals, type = "Ljung-Box", fitdf = 3, lag = 9)
Box.test(model_1$residuals, type = "Ljung-Box", fitdf = 3, lag = 12)

###################### Second Attempt 
model_2 = Arima(sales_ts, order=c(0,1,2),seasonal=list(order=c(2,0,0),period=12))
coeftest(model_2)

#ACF for Residuals
acf(coredata(model_2$residuals), main = 'ACF - Residuals')
pacf(coredata(model_2$residuals), main = 'ACF - Residuals')
#Ljung Box Test
Box.test(model_2$residuals, type = "Ljung-Box", fitdf = 4, lag = 12)
Box.test(model_2$residuals, type = "Ljung-Box", fitdf = 4, lag = 13)


###################### Third Attempt #############################
model_3 = Arima(sales_ts, order=c(0,1,1),seasonal=list(order=c(2,0,1),period=12))
coeftest(model_3)

#ACF for Residuals
acf(coredata(model_3$residuals), main = 'ACF - Residuals')
pacf(coredata(model_3$residuals), main = 'ACF - Residuals')
#Ljung Box Test
Box.test(model_3$residuals, type = "Ljung-Box", fitdf = 4, lag = 7)
Box.test(model_3$residuals, type = "Ljung-Box", fitdf = 4, lag = 8)

###################### Fourth Attempt ###############################

model_4 = Arima(sales_ts, order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
coeftest(model_4)

#ACF for Residuals
acf(coredata(model_4$residuals), main = 'ACF - Residuals')
pacf(coredata(model_4$residuals), main = 'ACF - Residuals')
#Ljung Box Test
Box.test(model_4$residuals, type = "Ljung-Box", fitdf = 3, lag = 9)
Box.test(model_4$residuals, type = "Ljung-Box", fitdf = 3, lag = 12)


######################## Fifth Attempt ###########################

model_5 = Arima(sales_ts, order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
coeftest(model_5)

#ACF for Residuals
acf(coredata(model_5$residuals), main = 'ACF - Residuals')
pacf(coredata(model_5$residuals), main = 'ACF - Residuals')
#Ljung Box Test
Box.test(model_5$residuals, type = "Ljung-Box", fitdf = 3, lag = 8)
Box.test(model_5$residuals, type = "Ljung-Box", fitdf = 3, lag = 6)
#QQplot
qqnorm(model_5$residuals)
qqline(model_5$residuals)

############################ Prediction #######################

#Compute Prediction
forecast.model5 = forecast.Arima(model_5, h=12)
#display the forecast
forecast.model5
#plot the forecast
plot(forecast.model5, include  = 100)
lines(ts(c(forecast.model5$fitted, forecast.model5$mean), frequency=12,start=c(1976,1)), col="blue")


source("F:/MSPA/CSC 425/Homework 4/backtest.R")
backtest(model_1, sales_ts, h =1, orig = length(df$TOTALNSA)* 0.95)
backtest(model_2, sales_ts, h =1, orig = length(df$TOTALNSA)* 0.95)
backtest(model_3, sales_ts, h =1, orig = length(df$TOTALNSA)* 0.95)
backtest(model_4, sales_ts, h =1, orig = length(df$TOTALNSA)* 0.95)
backtest(model_5, sales_ts, h =1, orig = length(df$TOTALNSA)* 0.85)


model_1
model_2
model_3
model_4

Predicted_TimeSeries <-new_ts 
Actual_TimeSeries <- sales_ts 

stocks <- cbind(Predicted_TimeSeries,Actual_TimeSeries)   
dygraph(stocks,ylab="Number of Automobile Sold", main="Predicted & Actual Time Series") 

library(dygraphs)
#plot the forecast
plot(x, include = 100)
new_ts <- ts(c(forecast.model5$fitted, forecast.model5$mean), frequency=12,start=c(1976,1))
plot(new_ts)
plot(x, include = 100)
