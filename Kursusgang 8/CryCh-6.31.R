set.seed(15243) # Konistent simulering af samme tidrække
series <- arima.sim(n=60, list(order=c(0,1,1), ma=-0.8))[-1] # Startværdien 0 smides væk
library(tseries)

#' ### spm (a)
adf.test(series, k=0)

#' ### spm (b)
ar(diff(series)) # Finder at lag order = 3 er den bedste.
adf.test(series) # Bekræfter også lag order = 3
# Konklusion af ADF test: Der er en unit  root.

#' ### spm (c)
adf.test(diff(series),k=0)
#' Konklusion: Forkast (korrekt) hypotesen om unit root i 1. differensen.

adf.test(diff(series))
#' Konklusion: Forkast (korrekt) hypotesen om unit root i 1. differensen.


