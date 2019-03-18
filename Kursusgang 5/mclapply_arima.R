#' Example of apply functions in R:
square <- function(x) x^2
x <- 1:10
sapply(x, square)
lapply(x, square)

#' Fitting many ARIMA models:
o <- expand.grid(ar=0:2, d=0:1, ma=0:2)
o_list <- split(o, 1:nrow(o))
library(astsa)
f <- function(order, data = oil, folder = NULL){
  # Create folder if missing
  if(!is.null(folder) && !dir.exists(folder)){
    dir.create(folder)
  }
  # Fit
  fit <- arima(x = data, order = as.numeric(order))
  # Save with sensible file name
  if(!is.null(folder)){
    order_string <- paste0(c("ar", "d", "ma"), as.numeric(order), collapse = "_")
    filename <- paste0("model_", order_string, ".RData")
    save(fit, file = file.path(folder, filename))
  }
  return(fit)
}
library(parallel)
models <- mclapply(o_list, f, data = oil, folder = "test-folder", mc.cores = 8)
o$aic <- sapply(models, AIC)
head(o)
i_best <- which.min(o$aic)
o[i_best,]
finalmodel <- models[[i_best]]

