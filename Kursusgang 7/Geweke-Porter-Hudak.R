##### Geweke-Porter-Hudak ###########
## Metode 2 fra Pfaff side 69. Jvf. R-Code 3.4 side 69

library(fracdiff)
set.seed(123456)
y <- fracdiff.sim(n=1000, d=0.3)
y.spec <- spectrum(y$series,plot=FALSE)
lhs <- log(y.spec$spec)
rhs <- log(4*(sin(y.spec$freq/2))^2)
gph.reg <- lm(lhs~rhs)
gph.sum <- summary(gph.reg)
gph.sum
sqrt(gph.sum$cov.unscaled*pi/6)[2,2]
