library(astsa)
library(tseries)

set.seed(274135)
series <- arima.sim(n=36, list(ar=0.95))

#' ### spm (a)
plot(series, type='o')
acf2(series)

#' ### spm (b)
adf.test(series, k=0)

#' ### spm (c)
adf.test(series)
#' Konklusion: unit root accepteres
#' 
#' ### spm (d)
series2 <- arima.sim(n=100, list(ar=0.95))

plot(series2,type='o')
acf2(series2)

#' ### spm (b) igen
adf.test(series2, k=0)

#' ### spm (c) igen
adf.test(series2)
#' Konklusion: unit root accepteres
