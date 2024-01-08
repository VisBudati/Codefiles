# Read in data!!
library(tseries)
library(TSA)
library(forecast)
library(LSTS)
AAPL <- read.csv("C:\\Users\\mitch\\Documents\\MSDS Grad\\Time Series Analysis I MA 641\\Course Project\\AAPL.csv")

# AAPL

AAPL_close <- AAPL[,5]
plot(AAPL_close,type='l')

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


# Pick model
# ARI? IMA? ARIMA?
model1 <- Arima(lndiff, order=c(1,1,0),method='ML')
model2 <- Arima(lndiff,order=c(0,1,1),method='ML')
model3 <- Arima(lndiff,order=c(1,1,1),method='ML')

# Residuals
resid <- rstandard(model2)

# Shapiro-Wilks test
shapiro.test(resid)
# p value greater than 0.05. Fail to reject null hypothesis. Accept normality

# QQ plot
qqnorm(resid)
qqline(resid)

# histogram
hist(resid, main='Residuals Histogram')

#Ljung-Box test
Box.Ljung.Test(resid)

# forecasting
lndiff_fit <- cbind(lndiff,model2$fit)
matplot(1:length(lndiff), lndiff_fit,type='l',xaxt='n',xlab='Time',
        ylab=expression('ln(Y'[t]~')'~'-'~'ln(Y'[t-1]~')'),
        main="Diff-Log transformation and Fitted IMA(1, 1) Model")
axis(side=1,at=c(8,20,32,44,56,68,80,92),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023'))

future <- forecast(model2)

plot(future, type='l',xaxt='n',xlab='Time',
     ylab=expression('ln(Y'[t]~')'~'-'~'ln(Y'[t-1]~')'),
     main="Transformed Series and Fitted IMA(1, 1) Model with Forecasting")
axis(side=1,at=c(8,20,32,44,56,68,80,92, 104),
     labels=c('2016','2017','2018','2019','2020','2021','2022','2023', '2024'))

lines(future[["fitted"]],lty='55',col='red')

legend(-2,0.2,legend=c("Transformed Time Series","Fitted IMA(1, 1) Model"),
       col=c("black","red"),lty=1:2,cex=0.8)

# Garch model

