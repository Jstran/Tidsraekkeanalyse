vartime = function(x, nmax = round(length(x) / 10)) {
  v = rep(NA, nmax);
  for (n in 1:nmax) {
    y = filter(x, rep(1/n, n), sides = 1);
    v[n] = var(y, na.rm = TRUE);
  }
  plot(log(1:nmax), log(v));
  lmv = lm(log(v) ~ log(1:nmax));
  abline(lmv);
  title(paste(deparse(substitute(x)), "; nmax = ", nmax));
  print(summary(lmv));
}

load("../tsa3.rda")


vartime(log(varve))
vartime(residuals(lm(log(varve) ~ time(log(varve)))))
