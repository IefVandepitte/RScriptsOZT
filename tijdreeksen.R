# install.packages("TTR")
# install.packages("forecast")

library(TTR)
library(forecast)

weekly_demand <- c(
  4, 16, 12, 25, 13, 12, 4,  8, 9, 14,
  3, 14, 14, 20,  7,  9, 6, 11, 3, 11,
  8,  7,  2,  8,  8, 10, 7, 16, 9,  4
)
demand_ts <- ts(weekly_demand)
plot.ts(demand_ts, type = 'b', pch = 20,
        xlab = "Time (weeks)",
        ylab = "Weekly demand",
        main = "Dataoverzicht")


b_est_20 <- mean(weekly_demand[1:20]) # the first 20 observations
b_est_30 <- mean(weekly_demand[1:30]) # the first 30 observations
plot.ts(demand_ts, type = 'b', pch = 20, main = "gemiddelde als schatter, rood = mean(eerste 20), blauw eerste 30")
abline(h = b_est_20, col = 'red')
abline(h = b_est_30, col = 'blue')

week <- 1:length(weekly_demand)
demand_lm <- lm(weekly_demand ~ week)
plot.ts(demand_ts, type = 'b', pch = 20, main = "regressierechte als schatter")
abline(demand_lm, col = 'red')



#Voortschrijdend gemiddelde
sma_3 <- SMA(x = demand_ts, n = 3)
sma_5 <- SMA(x = demand_ts, n = 5)
sma_10 <- SMA(x = demand_ts, n = 10)



plot.ts(demand_ts, main = "voortschrijdend gemiddelde")
lines(sma_3, col = 'orange')
lines(sma_5, col = 'blue')
lines(sma_10, col = 'purple')
legend("topright", lty = 1,
       c("Observations", "SMA(3)", "SMA(5)", "SMA(10)"),
       col = c("black", "orange", "blue", "purple"))


#########################
#Exponentiele afvlakking#
#########################

# Play with the value of alpha. What happens if alpha is close to 1?
ema <- HoltWinters(demand_ts, alpha = 0.1, beta = FALSE, gamma = FALSE)
plot(ema, main = "Exponential Moving Average")

#voorspelling
ema_fc <- forecast(ema, h = 10)
plot(ema_fc, main = "Forecast with Exponential Moving Average")

# Play with the value of alpha. What happens if alpha is close to 1?
ema <- HoltWinters(demand_ts, alpha = 0.5, beta = FALSE, gamma = FALSE)
plot(ema, main = "Exponential Moving Average")

ema_fc <- forecast(ema, h = 10)
plot(ema_fc, main = "Forecast with Exponential Moving Average")

# Play with the value of alpha. What happens if alpha is close to 1?
ema <- HoltWinters(demand_ts, alpha = 0.9, beta = FALSE, gamma = FALSE)
plot(ema, main = "Exponential Moving Average")

ema_fc <- forecast(ema, h = 10)
plot(ema_fc, main = "Forecast with Exponential Moving Average")

################################
#Dubbel exponentiele afvlakking# trend in data
################################

dema <- HoltWinters(demand_ts, gamma = FALSE)
plot(dema, main = "Double Exponential Moving Average")

dema_fc <- forecast(dema, h = 10)
plot(dema_fc)



####################################
# 3-dubbele exponentiele afvlakking# wederkerende trend, piekverkoop op zaterdag bv.
####################################
setwd(dir = "A:/schooleke/schooljaar4/semester2/onderzoekstechnieken-cursus/codevoorbeelden")
sales <- read.csv('shoestore-sales.csv')
sales_ts <- ts(sales$x_t, frequency = 7, start = c(1, 1))
plot.ts(sales_ts,
        main = 'Shoestore sales',
        xlab = 'week',
        ylab = 'SKUs')

hw <- HoltWinters(sales_ts)
plot(hw, xlim = c(1, 4))

sales_fc <- forecast(hw, h = 14)
plot(sales_fc)

sales_decomposed <- decompose(sales_ts)
plot(sales_decomposed)


#################
#kings         #
###############
setwd(dir = "A:/schooleke/schooljaar4/semester2/onderzoekstechnieken-cursus/cursus")
kings <- scan (file = "data/tijdreeksen/kings.data", skip = 3)
kingstimeseries <- ts(kings)
plot.ts(kingstimeseries, ylab='leeftijd', xlab='time')
grid (lty=2, lwd=1, col="black")


##############
#rain       #
############

rain <- scan("data/tijdreeksen/precip.data", skip = 1)
rainseries <- ts(rain, start = c(1813))
plot.ts(rainseries)
rainseriesforecast <- forecast(rainseries, h = 20)
plot(rainseriesforecast)
ema <- HoltWinters(rainseries, alpha = 0.1, beta = FALSE, gamma = FALSE)
plot(ema, main = "Exponential Moving Average")

acf(rainseriesforecast$residuals, lag.max = 20, na.action = na.pass)
Box.test(rainseriesforecast$residuals, lag = 20, type = "Ljung-Box")

ploteForecastErrors <- function(forecastErrors){
  #maak een histogram van de voorspelling fouten
  mybinsize <- IQR(forecastErrors)/4
  mysd <- sd(forecastErrors)
  mymin <- min(forecastErrors)
  mymax <- max(forecastErrors)
  # genereer normaal verdeelde data met mean 0 en sd mysd
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if(mymax2 > mymax){ mymax <- mymax2 }
  #maak een rood histogram van de voorspelling fouten, met de normaal verdeling eroverheen
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecastErrors, col = "red", freq = FALSE, breaks = mybins)
  # freq = FALSE garandeert dat het gebied onder het histogram = 1
  # genereer normaal verdeelde data met mean 0 en sd mysd
  myhist <- hist(mynorm, plot = FALSE)
  #plot de normaalcurve als een blauwe lijn over het histogram van voorspelllingsfouten
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}
ploteForecastErrors(rainseriesforecast$residuals)