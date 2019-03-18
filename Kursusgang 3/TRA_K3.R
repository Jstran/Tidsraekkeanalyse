library(astsa)
library(forecast)

myt <- ts(cmort)


regr = ar.ols(myt, order=2, demean=FALSE, intercept=TRUE)
fore = predict(regr, n.ahead=24)
ts.plot(myt, fore$pred, col=1:2, xlim=c(450,550), ylab="Mortality rate")
U = fore$pred+fore$se; L = fore$pred-fore$se
xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(fore$pred, type="p", col=2)
forecast(myt , level = 95)
