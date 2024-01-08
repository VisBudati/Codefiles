#INSTALL REQUIRED PACKAGES IF NEEDED
#ADD EXTRA PACKAGES TO LIST.OF.PACKAGES TO INSTALL MORE
list.of.packages <- c("ggplot2", "Rcpp","tseries","TSA","forecast","LSTS","zoo","stats","fGarch","timeSeries","rugarch")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Load necessary libraries and Import DATA
library(tseries)
library(TSA)
library(forecast)
library(LSTS)
library(zoo)
library(stats)
library(rugarch)
library(fGarch)
library(timeSeries)
# Monthly average bike data
obc <- read.csv("C:\\Users\\clpga\\Documents\\School Work\\Time Series Analysis 1 MA 641\\Group Project\\BIKE DATA MONTHLY.csv")
#BIKE_DATA_MONTHLY <- obc
# Raw bike data
abc <- read.csv("C:\\Users\\clpga\\Documents\\School Work\\Time Series Analysis 1 MA 641\\Group Project\\ottawa_bike_counters.csv")

# AAPL stock data
AAPL <- read.csv("C:\\Users\\clpga\\Documents\\School Work\\Time Series Analysis 1 MA 641\\Group Project\\AAPL.csv")

#APPLE STOCK PRICE SECTION

# AAPL
AAPL_close <- AAPL[,5]
plot(AAPL_close,type='l',xaxt='n',main='AAPL Monthly Closing Stock Price',
     ylab='Stock Price', xlab='Time')

axis(side=1,at=c(8,20,32,44,56,68,80,92),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023'))

# Dicky-Fuller test, AAPL
adf.test(AAPL_close)

# Fail to reject null hypothesis. Series is non-stationary

diff1 <- diff(AAPL_close)
plot(diff1,type='l',xaxt='n',xlab='Time',ylab=expression('Y'[t]~'-'~'Y'[t-1]),main='First Difference of AAPL Time Series')
axis(side=1,at=c(8,20,32,44,56,68,80,92),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023'))


adf.test(diff1)
# Reject null hypothesis. First difference is stationary.

acf(diff1)

pacf(diff1)

eacf(diff1)
# TRY TRANSFORMATION INSTEAD

# ln of data
lnAAPL <- log(AAPL_close)
plot(lnAAPL,type='l',xaxt='n',xlab='Time',ylab=expression('ln(Y'[t]~')'),main='Log of AAPL Time Series')
axis(side=1,at=c(8,20,32,44,56,68,80,92),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023'))

adf.test(lnAAPL)
# Fail to reject null hypothesis. Accept that series is not stationary

lndiff <- diff(lnAAPL)
plot(lndiff,type='l',xaxt='n',xlab='Time',ylab=expression('ln(Y'[t]~')'~'-'~'ln(Y'[t-1]~')'),main='Diff-Log Transformation of AAPL Time Series')
axis(side=1,at=c(8,20,32,44,56,68,80,92),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023'))

adf.test(lndiff)

acf(lndiff,main='ACF plot of diff-log transformation')
pacf(lndiff,main='PACF plot of diff-log transformation')
eacf(lndiff)

abslndiff <- abs(lndiff)
plot(abslndiff,type='l',xaxt='n',xlab='Time',ylab=expression('ln(Y'[t]~')'~'-'~'ln(Y'[t-1]~')'),main='Diff-Log Transformation of AAPL Time Series')
axis(side=1,at=c(8,20,32,44,56,68,80,92),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023'))


acf(abslndiff)

pacf(abslndiff)

eacf(abslndiff)




# Pick model
# BEST MODEL IS GARCH(1,1)

model2 <- Arima(abslndiff,order=c(0,1,1),method='ML')
model2

model3 <- Arima(abslndiff,order=c(1,1,1),method='ML')
model3

model4 <- Arima(abslndiff,order=c(0,0,0),method='ML')
model4

# Residuals
resid2 <- rstandard(model2)
resid4 <- rstandard(model4)

# Shapiro-Wilks test
shapiro.test(resid2)
# p value greater than 0.05. Fail to reject null hypothesis. Accept normality

shapiro.test(resid4)
# p value greater than 0.05. Fail to reject null hypothesis. Accept normality

# QQ plot
qqnorm(resid2)
qqline(resid2)

qqnorm(resid4)
qqline(resid4)

# histogram
hist(resid2, main='Residuals Histogram')
hist(resid4, main='Residuals Histogram')

#Ljung-Box test

tsdiag(model2)
tsdiag(model4)

Box.Ljung.Test(resid2)
Box.Ljung.Test(resid4)

# acf plot residuals
acf(resid2)
acf(resid4)
# Both look like white noise

# forecasting
future2 <- forecast(model2)

plot(future2, type='l',xaxt='n',xlab='Time',
     ylab=expression('ln(Y'[t]~')'~'-'~'ln(Y'[t-1]~')'),
     main="Transformed Series and Forecasted IMA(1, 1) Model")
axis(side=1,at=c(8,20,32,44,56,68,80,92, 104),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023', '2024'))

future4 <- forecast(model4)

plot(future4, type='l',xaxt='n',xlab='Time',
     ylab=expression('ln(Y'[t]~')'~'-'~'ln(Y'[t-1]~')'),
     main="Transformed Series and Forecasted White Noise")
axis(side=1,at=c(8,20,32,44,56,68,80,92, 104),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023', '2024'))

# These two lines are the fitted models. Don't need to run. Just for reference
#lines(future2[["fitted"]])
#lines(future4[["fitted"]])

# Garch model
g1=garch(abslndiff,order=c(1,3))
g2=garch(abslndiff,order=c(0,3))
g3=garch(abslndiff,order=c(2,3))



"g4=garch(abslndiff,order=c(2,0))
g4=garch(abslndiff,order=c(0,2))
g5=garch(abslndiff,order=c(2,2))
g8=garch(abslndiff,order=c(1,2))
g9=garch(abslndiff,order=c(2,1))
g10=garch(abslndiff,order=c(3,0))
g11=garch(abslndiff,order=c(0,3))
g10=garch(abslndiff,order=c(3,1))
g11=garch(abslndiff,order=c(1,3))
g10=garch(abslndiff,order=c(3,2))
g11=garch(abslndiff,order=c(2,3))
g12=garch(abslndiff,order=c(3,3))"

g1$n.likeli
g2$n.likeli
g3$n.likeli
"g4$n.likeli
g5$n.likeli
g8$n.likeli
g9$n.likeli
g10$n.likeli
g11$n.likeli
g12$n.likeli"


collection<- c(g1$n.likeli,
               g2$n.likeli,
               g3$n.likeli
               )

min(collection)

max(collection)

# Residuals
resid5 <- rstandard(model2)


spec <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,3),submodel = NULL,external.regressors = NULL,variance.targeting = FALSE),mean.model = list(armaOrder = c(0,1), external.regressors = NULL,distribution.model= "norm"))

garch1 <- ugarchfit(spec = spec, data = abslndiff,solver.control = list(trace=0))
garch1

spec1 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(0,3),submodel = NULL,external.regressors = NULL,variance.targeting = FALSE),mean.model = list(armaOrder = c(0,1), external.regressors = NULL,distribution.model= "norm"))

garch2 <- ugarchfit(spec = spec1, data = abslndiff,solver.control = list(trace=0))
garch2

spec2 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(2,3),submodel = NULL,external.regressors = NULL,variance.targeting = FALSE),mean.model = list(armaOrder = c(0,1), external.regressors = NULL,distribution.model= "norm"))

garch3 <- ugarchfit(spec = spec2, data = abslndiff,solver.control = list(trace=0))
garch3

x.g = garchFit(~garch(3,3),abslndiff, include.mean=F)
summary(x.g)



GARCHPLOT<-ugarchforecast(garch2, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0, 
               external.forecasts = list(mregfor = NULL, vregfor = NULL))

plot(GARCHPLOT,which=1)
   
#BIKE DATA
df<-data.frame(obc)
obc_data <- obc[,3]
#obc_data <- abc[1097:1835,5]
summary(obc_data)
plot(obc_data, type='l',xaxt='n',xlab='Time',
     ylab=expression('Number of Bike Riders'),
     main="Monthly Average of Bike Riders in Ottawa")
axis(side=1,at=c(1,13,25,37,49,61),
     labels=c('2013','2014','2015','2016','2017','2018'))



# Shapiro-Wilks Test for Normality
shapiro_test_result <- shapiro.test(obc_data)
print("Shapiro-Wilks Test for Normality:")
print(shapiro_test_result)
cat("p-value:", shapiro_test_result$p.value, "\n")


# QQ Plot for Normality
qqnorm(obc_data)
qqline(obc_data)


# Ljung-Box Test for Autocorrelation at Lag 1
Box.test(ts(obc_data), lag = 1, type = "Ljung") # Null hypothesis rejected, indicating significant autocorrelation at lag 1
# Ljung-Box Test for Autocorrelation at Lag 2
Box.test(ts(obc_data), lag = 2, type = "Ljung")



# Dicky-Fuller test
adf.test(obc_data)
# Reject null hypothesis. Accept alternate hypothesis. Series is stationary
# No further differencing or transformations needed

# ACF plot and PACF plot
acf(obc_data,lag.max=50,main='Monthly Average Bike Riders ACF')
pacf(obc_data,lag.max=50,main='Monthly Average Bike Riders PACF')
# ACF plot shows seasonality with lag 12


#Extra Test That Jones Made:

y=log(df$AVERAGE)
z = diff(y)
acf(ts(z),lag.max=60) 
pacf(ts(z),lag.max=60)




# Difference with lag 12
differenced_data = diff(obc_data, lag=12)
seasonal_data = ts(differenced_data)
plot(seasonal_data, ylab='Ottawa Bike Counters')

# Dicky-Fuller test for new series
adf.test(seasonal_data)
# Accept null hypothesis. Seasonal differenced data is not stationary

# Make seasonal data stationary through differencing
seasonal_data <- diff(seasonal_data)
plot(seasonal_data, ylab='Ottawa Bike Counters')
adf.test(seasonal_data)
# First difference is stationary now

# ACF plot of differenced data
acf(seasonal_data, main='ACF plot of First Difference and First Seasonal Difference with lag 12')
pacf(seasonal_data, main='PACF plot of First Difference and First Seasonal Difference with lag 12')
eacf(seasonal_data)
# ACF plot suggests MA(1) component, no MA seasonal components
# PACF plot suggests AR(2) component, no AR seasonal components
# EACF plot suggests an ARMA(1,1)

# Try SARIMA(0,1,1) with seasonal component (0,1,1) with lag 12
model_s1 <- Arima(obc_data,order=c(0,1,1),seasonal=list(order=c(0,1,0),period=12))
model_s1
# Try SARIMA(2,1,0) with seasonal component (2,1,0) with lag 12
model_s2 <- Arima(obc_data,order=c(2,1,0),seasonal=list(order=c(0,1,0),period=12))
model_s2
# Try SARIMA(1,1,1) with seasonal component (1,1,1) with lag 12
model_s3 <- Arima(obc_data,order=c(1,1,1),seasonal=list(order=c(0,1,0),period=12))
model_s3
# Try SARIMA(2,1,1) with seasonal component (1,1,0) with lag 12
model_s4 <- Arima(obc_data,order=c(2,1,1),seasonal=list(order=c(0,1,0),period=12))
model_s4


# MAYBE TRY DIFFERENT MODELS
# Continuing with model_s1. Best AIC value 

# Residuals
resid_s1 <- rstandard(model_s1)

# Shapiro-Wilks test
shapiro.test(resid_s1)
# p value is smaller than 0.05. Reject normality. May be due to sample size as
# shapiro wilks test is sensitive to small deviations from the normal. 

# QQ plot
qqnorm(resid_s1)
qqline(resid_s1)

# histogram
hist(resid_s1, main='Residuals Histogram')

#Ljung-Box test

tsdiag(model_s1)

Box.Ljung.Test(resid_s1)

# acf plot residuals
acf(resid_s1)
# Resembles white noise


# forecasting
future_s1 <- forecast(model_s1)

plot(future_s1, type='l',xaxt='n',xlab='Time',
     ylab=expression('Number of Bike Riders'),
     main=expression("Monthly Average of Bike Riders in Ottawa with Forecasted values from SARIMA(0, 1, 1) x (0, 1, 0)"[12]))
axis(side=1,at=c(1,13,25,37,49,61,73,85),
     labels=c('2013','2014','2015','2016','2017','2018','2019','2020'))



#BIKE DATA UPDATED
obc1 <- read.csv("BIKE DATA MONTHLY with 2018.csv")
obc_data1 <- obc1[,3]
#obc_data <- abc[1097:1835,5]
summary(obc_data)
plot(obc_data1, type='l',xaxt='n',xlab='Time',
     ylab=expression('Number of Bike Riders'),
     main="Monthly Average of Bike Riders in Ottawa")
axis(side=1,at=c(1,13,25,37,49,61),
     labels=c('2013','2014','2015','2016','2017','2018'))

# Dicky-Fuller test for stationarity
adf.test(obc_data1)
# Reject null hypothesis. Accept alternate hypothesis. Series is stationary
# No further differencing or transformations needed

# ACF plot and PACF plot
acf(obc_data1,lag.max=50,main='Monthly Average Bike Riders ACF')
pacf(obc_data1,lag.max=50,main='Monthly Average Bike Riders PACF')
# ACF plot shows seasonality with lag 12

# Difference with lag 12 to remove seasonality
differenced_data1 = diff(obc_data1, lag=12)
seasonal_data1 = ts(differenced_data1)
plot(seasonal_data1, ylab='Ottawa Bike Counters')

# CHANGE THIS SO THAT IT ISNT SEASONAL DIFFERENCE DATA
# ACF plot of differenced data
acf(seasonal_data1, main='ACF plot of First Seasonal Difference with lag 12')
pacf(seasonal_data1, main='PACF plot of First Seasonal Difference with lag 12')
eacf(seasonal_data1)
# ACF plot suggests SMA(1)x(1) with lag 12
# PACF plot suggests AR(1) component, no AR seasonal components
# EACF plot suggests an ARMA(3,1), ARMA(2,1), ARMA(1,1)

# Try SARIMA(0,0,1) with seasonal component (0,1,1) with lag 12
model_s1_1 <- Arima(obc_data1,order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
model_s1_1
# Try SARIMA(1,0,0) with seasonal component (0,1,0) with lag 12
model_s2 <- Arima(obc_data1,order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12))
model_s2_1
# Try SARIMA(3,0,1) with seasonal component (0,1,0) with lag 12
model_s3 <- Arima(obc_data1,order=c(3,0,1),seasonal=list(order=c(0,1,0),period=12))
model_s3_1
# Try SARIMA(2,0,1) with seasonal component (0,1,0) with lag 12
model_s4 <- Arima(obc_data1,order=c(2,0,1),seasonal=list(order=c(0,1,0),period=12))
model_s4_1
# Try SARIMA(1,0,1) with seasonal component (0,1,0) with lag 12
model_s5 <- Arima(obc_data1,order=c(1,0,1),seasonal=list(order=c(0,1,0),period=12))
model_s5_1

# Continuing with model_s1. Best AIC value 

# Residuals
resid_s1 <- rstandard(model_s1_1)

# Shapiro-Wilks test for normality of residuals
shapiro.test(resid_s1_1)
# p value is smaller than 0.05. Reject normality. May be due to sample size as
# shapiro wilks test is sensitive to small deviations from the normal. 

# QQ plot
qqnorm(resid_s1_1)
qqline(resid_s1_1)

# histogram
hist(resid_s1_1, main='Residuals Histogram')

#Ljung-Box test for dependency of residuals

tsdiag(model_s1_1)

Box.Ljung.Test(resid_s1_1)

# acf plot residuals
acf(resid_s1_1)
# Resembles white noise


# forecasting
future_s1_1 <- forecast(model_s1_1)

plot(future_s1_1, type='l',xaxt='n',xlab='Time',
     ylab=expression('Number of Bike Riders'),
     main=expression("Monthly Average of Bike Riders in Ottawa with Forecasted values from SARIMA(0, 0, 1) x (0, 1, 1)"[12]))
axis(side=1,at=c(1,13,25,37,49,61,73,85),
     labels=c('2013','2014','2015','2016','2017','2018','2019','2020'))

abline(0,0)


