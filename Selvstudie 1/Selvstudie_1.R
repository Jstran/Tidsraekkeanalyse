# Selvstudieopgavesæt 1
# Nedenstående opgave arbejdes der med ved første selvstudie-kursusgang.
#   Data til opgaven er prisen i dollars pr. tønde (42 gallon) "West-Texas
# Intermediate Crude Oil (WTI)". Priserne foreligger som månedsdata fra
# Januar 1986 til Juni 2013, i alt 330 observationer. Datasættet er i filen
# olie.RData som kan indlæses med funktionen load(). Til identifikationen
# og estimationen nedenfor bruges data op til december 2012. De sidste 6 måneder,
# fra januar 2013 til juni 2013 reserveres til out-of-sample forecasting
# (Hint: Brug funktionen window() til at opdele datasættet)

# Opgave
# Identificér, estimér og prædiktér (forecast) ARMA modeller for logaritmen til olieprisen
# (her kaldes denne variabel x_t)

# 1. I det følgende betragtes som nævnt de 324 første observationer, altså data til og 
# med december 2012. Plot og beskriv kort log prisen af olie i perioden. Kommentér på 
# særligt usædvanlige egenskaber ved data. Beregn, plot og fortolk korrelogrmmet for 
# log-olieprisen, samt korrelogrammet for første-differensen af log-olieprisen.

# Indlæser oliedata
load("./Selvstudie 1/olie.Rdata")
loil <- log(olie)

# Plotter log-oil
plot(loil)

# Ses at der forekommer en stærk opadgående trend med spikes

# Plotter acf for første-differensen af 'loil'
acf(diff(loil))

# 2. Estimér en AR(1) model for log-olieprisen på to forskellige måder. Estimér
# først autoregressionen

#   x_t = alpha + phi * x_{t-1} + w_t

# og estimér dernæste den middelværdi-justerede model

#   x_t - mu = phi * (x_{t-1} - mu) + w_t

# Laver x_t og x_{t-1}
x      <- loil[2:length(loil)]
xlag   <- loil[1:(length(loil)-1)]
xdm    <- x - mean(x)
xlagdm <- xlag - mean(xlag)

ar1.coef <- lm(x ~ xlag)$coefficients



ar1.demean.coef <- lm(xdm ~ xlagdm)$coefficients

