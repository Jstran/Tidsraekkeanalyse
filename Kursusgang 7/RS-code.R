##### R/S metoden (Range-statistics) ###########
## Metode 1 fra Pfaff side 66-67. Jvf. R-Code 3.3 side 68
library(fracdiff)
set.seed(123456)
# ARFIMA(0.0,0.3,0.0)
y <- fracdiff.sim(n=1000,d=0.3)
# TEST:
summary(fracdiff(y$series, nar = 0, nma = 0))
# Get the dataseries, demean this if necessary
y.dm <- y$series
max.y <- max(cumsum(y.dm))
min.y <- min(cumsum(y.dm))
sd.y <- sd(y$series)
RS <- (max.y-min.y)/sd.y
H <- log(RS)/log(1000)
d <- H-0.5
d
