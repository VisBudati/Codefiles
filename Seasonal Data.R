library(tseries)
library(TSA)
library(forecast)
library(LSTS)
obc <- read.csv("C:\\Users\\jerem\\OneDrive\\Desktop\\ottawa_bike_counters - ottawa_bike_counters.csv")

obc_data <- obc[1097:1835,5]
summary(obc_data)
plot(obc_data, type = 'l')

adf.test(obc_data)

segment1 = obc_data[1:369]
segment2 = obc_data[370:738]

print(mean(segment1))
print(mean(segment2))
print(var(segment1))
print(var(segment2))

acf(obc_data,lag.max=50)
pacf(obc_data,lag.max=50)                    

differenced_data = diff(obc_data, lag=21)
seasonal_data = ts(differenced_data)
plot(seasonal_data, ylab='Ottawa Bike Counters')

adf.test(seasonal_data)


