# Problem 3.33

library(astsa)
ts.plot(gtemp) # clearly not stationary
ts.plot(diff(gtemp)) # first differences look better
acf2(diff(gtemp)) # ARMA or AR(3)
mod1<-sarima(gtemp,1,1,1);mod1 # ARIMA(1,1,1) # no particlar autocorrelation in residuals,
# but may be heavy tails
mod2<-sarima(gtemp,3,1,0);mod2# AR(3), not so heavy tails
rbind(c(mod1$AIC,mod1$AICc,mod1$BIC),c(mod2$AIC,mod2$AICc,mod2$BIC))
# mod2 best
sarima(gtemp,2,1,1) # ARIMA(2,1,1) # no obvious amelioration
sarima(gtemp,1,1,2)# same here, c
#
#   forecast ARIMA(1,1,1)
sarima.for(gtemp,10,1,1,1) # upward trend, but wide intervals
sarima.for(gtemp,10,3,1,0)

# Compare with auto.arima from `forecast`
library(forecast)
automodel <- auto.arima(gtemp)
autoforecast <- forecast(automodel, h=10)
plot(autoforecast)
