### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------
library(forecast)

### ¤¤ TS, ACF og PACF plot funktion ¤¤ ### ---------------------------------------------

tsplots <- function(dat , diffs = 0){
  
  if(diffs > 5)stop("diffs must be smaller than 6")
  
  par(mfrow = c(diffs + 1,3))
  
  plot(dat) ; acf(dat) ; pacf(dat)
  if(diffs > 0){
    for(i in 1:diffs ){
      ddat <- diff(dat , differences = i)
      plot(ddat) ; acf(ddat) ; pacf(ddat)
    
    }
  }
  par(mfrow = c(1,1))
}
### ¤¤ Simulerer ARIMA med d = 2. Plotter ACF og PACF ¤¤ ### ----------------------------

mts <- arima.sim(list(order = c(3,2,0), ar = c(.3 , .2 , .4)) , n = 1000 )

### Laver differenser
dmts  <- diff(mts)
ddmts <- diff(dmts)

### Plotter tidsraekken, ACF og PCF for op til differens 2 
par(mfcol = c(3,3))

plot(mts) ; plot(dmts) ; plot(ddmts)
acf(mts)  ; acf(dmts)  ; acf(ddmts)
pacf(mts) ; pacf(dmts) ; pacf(ddmts)

par(mfcol = c(1,1))

### ¤¤ Simulerer ARIMA med d = 5. Plotter ACF og PACF ¤¤ ### ----------------------------

mts <- arima.sim(list(order = c(4,5,3) ,
                      ar    = c(.4 , .2 , .2 , .1) ,
                      ma    = c(.6 , .8 , .3)) ,
                 n = 1000 )
mts <- list(d0 = mts ,
            d1 = diff(mts , differences = 1) ,
            d2 = diff(mts , differences = 2) ,
            d3 = diff(mts , differences = 3) ,
            d4 = diff(mts , differences = 4) ,
            d5 = diff(mts , differences = 5) )


### Plotter tidsraekken, ACF og PCF for op til differens 5 
par(mfcol = c(6,3))

plot(mts$d0) ; plot(mts$d1) ; plot(mts$d2) ; plot(mts$d3) ; plot(mts$d4) ; plot(mts$d5)
acf(mts$d0)  ; acf(mts$d1)  ; acf(mts$d2)  ; acf(mts$d3)  ; acf(mts$d4)  ; acf(mts$d5)
pacf(mts$d0) ; pacf(mts$d1) ; pacf(mts$d2) ; pacf(mts$d3) ; pacf(mts$d4) ; pacf(mts$d5)

par(mfcol = c(1,1))

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------

# Her er ingenting