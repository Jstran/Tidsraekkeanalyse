#' # SKITSE til opgave ShSt 5.3

#' Bemærk: Med RStudio kompileres script til output med genvej Ctrl-Shift-K.

#' Data og relevante differenser og lags
library(astsa)
library(tseries)
plot(gtemp)
n <- length(gtemp)
trend <- 1:n
x <- gtemp
x1 <- c(NA, gtemp[1:(n-1)])  # Lag(x,1) med NA som første værdi
dx <- x-x1                   # Dif(x) med NA som første værdi
dx1 <- c(NA, dx[1:(n-1)])    # Lag(Dif(x)) med NA'er som første værdier
dx2 <- c(NA, dx1[1:(n-1)])   # Lag(Dif(x),2) med NA'er som første værdier
dx3 <- c(NA, dx2[1:(n-1)])   # Lag(Dif(x),3) med NA'er som første værdier
dx4 <- c(NA, dx3[1:(n-1)])   # Lag(Dif(x),4) med NA'er som første værdier
head(data.frame(x, x1, dx, dx1, dx2, dx3, dx4))

#' Alm. DF test med check af om residualer er autokorrelerede
adf.test(x, k=0)  # DF test
model <- lm(dx ~ trend + x1)
res <- residuals(model)
summary(model) # Bemærk t-værdi for `x1` er DF-teststatistik
acf(res)
pacf(res)

#' Augm. DF test med lag 1 og check af om residualer er autokorrelerede
adf.test(x, k=1)  # ADF test 1 lag
model <- lm(dx ~ trend + x1 + dx1)
res <- residuals(model)
summary(model) # Bemærk t-værdi for `x1` er DF-teststatistik
acf(res)
pacf(res)

#' Augm. DF test med lag 4 og check af om residualer er autokorrelerede
adf.test(x, k=4)  # ADF test 4 lags
model <- lm(dx ~ trend + x1 + dx1 + dx2 + dx3 + dx4)
res <- residuals(model)
summary(model) # Bemærk t-værdi for `x1` er DF-teststatistik
acf(res)
pacf(res)

#' Automatisk augm. DF test og PP test
adf.test(x)  # DF test med automatisk valg af k
pp.test(x)        # PP test
