library(fracdiff)

polymult <- function(x,a,b)
  # Result is a(B)b(B) x_t
{
  if (length(b) == 1){polycoeff <- array(a)}
  else
  {alpha <- array(a)
  p <- dim(alpha)
  beta <- array(b)
  q <- dim(beta)
  # Assumed: p >= q
  polycoeff <- array(rep(0,p+q))
  for (k in 1:(p+q)) {
    for (j in max(0,k-q):min(k,p))
    {if (j == 0) {polycoeff[k] <- polycoeff[k] + beta[k]}
      else
      {if (j == k) {polycoeff[k] <- polycoeff[k] + alpha[k]}
        else {polycoeff[k] <- polycoeff[k] + alpha[j]*beta[k-j]}}
    }
  }
  }
  return(filter(x,c(1,polycoeff),sides=1, method="convolution"))
}

polyinvers <- function (x, a, maxlag = 20)
  # Result is (1/a(B)) x_t
{
  phi <- array(a)
  p <- dim(phi) # assumed >= 1
  polycoeff <- array(rep(0,maxlag))
  polycoeff[1] <- -phi[1]
  for (k in 2:maxlag) {
    for (j in (1:min(p,k))) {
      if (j == k) {polycoeff[k] <- polycoeff[k] - phi[k]}
      else
      {polycoeff[k] <- polycoeff[k] - phi[j]*polycoeff[k-j]}
    }
  }
  return(filter(x,c(1,polycoeff),sides=1, method="convolution"))
}

