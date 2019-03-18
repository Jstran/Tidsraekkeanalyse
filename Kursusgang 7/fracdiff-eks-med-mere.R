library(fracdiff)

source("Polyprocs.R")

maksimallag <- 50;

y <- fracdiff.sim(1000, ar = .6, ma = .25, d = .3)

y <- y$series

plot.ts(y,ylab="Simulated time series")

# Vi prøver så nogle forskellige modeller og ser hvilken der er bedst vha. fx AIC
AIC(fit20 <- fracdiff(y, nar = 2, nma = 0))
AIC(fit12 <- fracdiff(y, nar = 1, nma = 2))
AIC(fit11 <- fracdiff(y, nar = 1, nma = 1))
AIC(fit10 <- fracdiff(y, nar = 1, nma = 0))
AIC(fit02 <- fracdiff(y, nar = 0, nma = 2))
AIC(fit01 <- fracdiff(y, nar = 0, nma = 1))
AIC(fit00 <- fracdiff(y, nar = 0, nma = 0))

summary(fit11)
fit11$d

library(astsa)

y.fd <- diffseries(y, fit11$d)
fit11arima <- arima(y.fd, order=c(1,0,1))
sarima(y.fd,1,0,1)

acf2(residuals(fit11arima))

#### OPSUMMERING ############

ymodel <- fracdiff(x = y, nar = 1, nma = 1)
summary(ymodel)
ymodel$ma <- -ymodel$ma  # fortegnsskift
# Fordi fracdiff desværre bryder konventionen mht. fortegn

## Konfidensintervaller
confint(ymodel)

y.fd <- diffseries(y, d = ymodel$d)    
# (1-B)^d x_t. Følger en ARMA(1,1) ifølge det foregående

ts.plot(y.fd)
acf2(y.fd)   

ar_del <- polyinvers(y.fd, ymodel$ma, maxlag = maksimallag)
# 1/theta(B)*(1-B)^d x_t. Følger en AR(1) ifølge det foregående

plot(ar_del)
pacf(na.omit(ar_del))
arima(ar_del,order=c(1,0,0))

residualer <- polymult(ar_del,-ymodel$ar,0)[(maksimallag+2):length(y)]

# (1-phi*B)*1/theta(B)*(1-B)^d x_t = w_t

plot.ts(residualer)
acf(residualer)
pacf(residualer)
qqnorm(residualer)
qqline(residualer)
