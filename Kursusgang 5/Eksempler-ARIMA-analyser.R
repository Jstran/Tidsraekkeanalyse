# Simulation of an IMA(2,2) Series with θ1 = 1 and θ2 = −0.6

ima22 <- arima.sim(list(order = c(0,2,2), ma = c(1, -0.6)), n = 62)
ts.plot(ima22)
acf(ima22)

dima22 <- diff(ima22)
ts.plot(dima22)
acf(ima22)

ddima22 <- diff(dima22)
ts.plot(ddima22)
acf(ddima22)

# ARIMA eksempler

library(TSA)
data(oil.price)
plot(oil.price, ylab='Price per Barrel',type='l')

loil <- log(oil.price)

plot(diff(loil), ylab='Change in Log(Price)',type='l')

# Model diagnostics

acf(diff(loil))
pacf(diff(loil))

# Standardized Residuals from Log Oil Price IMA(1,1) Model
m1.oil <- arima(loil, order=c(0,1,1))
m1.oil
plot(rstandard(m1.oil), ylab='Standardized residuals')
abline(h=0, col = "red")

#Quantile-Quantile Plot: Residuals from IMA(1,1) Model for Oil
qqnorm(residuals(m1.oil),pch=3); qqline(residuals(m1.oil))

# Auto ARIMA med mere
library(forecast)
fit <- Arima(oil.price, order=c(3,1,1))
summary(fit)

modArima <- auto.arima(oil.price)
modArima
arimaorder(modArima)
