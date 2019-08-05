#Inspire a Better Tomorrow
set.seed(11)

#install.packages(c('tidyverse','RSocrata','forecast','urca','ggthemes'))

#Packages
library(tidyverse)
library(RSocrata)
library(forecast)
library(urca)
library(ggthemes)

#------------------------------------------------DOWNLOAD THE DATA----------------------------------------------
#
# Download Data from Chicago Open Data Portal
# https://data.cityofchicago.org/Administration-Finance/Performance-Metrics-Family-Support-Services-Homele/vg8w-2w9y
# 

link <- "https://data.cityofchicago.org/resource/vg8w-2w9y.csv" 
data.raw <- read.socrata(link)

#------------------------------------------------TRANSFORM THE DATA----------------------------------------------
#
# Mash and squeeze the data
# Make Time Series object
#

# Convert to ts object
data.raw2 <- data.raw %>%
  select(4,6) #%>%
  #mutate(OverCap = ifelse(overnight_shelter_beds_used > total_overnight_beds , 1 , 0))
  

data.ts <- data.raw2[,1] %>%
  ts(start = 2011 , frequency = 12)

data.overflow <- data.raw %>%
  filter(interim_shelter_beds_used > total_interim_beds | overnight_shelter_beds_used > total_overnight_beds)



#------------------------------------------------EYE-BALL THE DATA----------------------------------------------
#
# Look at overall time-series, seasonality, and white noise
#


# Eye-balled bed usage and bed availability:
autoplot(ts(data.raw2[,1:2] , start = 2011 , frequency = 12)) + 
  ylab("Bed Usage Count") + geom_line(size = 1) +
  theme_stata() + scale_colour_solarized("blue")


# Eye-ball strictly bed usage
autoplot(data.ts) + geom_line(color = 'steelblue',size = 1) +
  ylab("Bed Usage Count") +
  theme_stata()


# Eye-ball for seasonality
ggseasonplot(data.ts , year.labels = TRUE , year.labels.left = TRUE) + 
  ylab("Bed Usage Count") +
  theme_stata() +
  geom_line(size = 0.8)
  # Yikes
  # Does not appear to be any seasonality


# Is there white noise (randomness)?
ggAcf(data.ts) + theme_stata()
  # Each lag should have autocorrelation close to zero for a purely random time series.
  # A whooole bunch of lags breach our 95% confidence interval, so we can be certain that there is a time series component to this data


# Alternatively we could do an Ljung-Box test on the overall randomness.
Box.test(data.ts , lag = 24 , fitdf = 0 , type = "Ljung-Box")
  # P too low, null must go
  # P value < 0.05 suggests changes are significantly different from white noise (randomness)


# There's a time series component. Let's get ready to rumble.


#------------------------------------------------FORECAST THE DATA----------------------------------------------
#
# Start with naive method, then try Holt's method
#

# Start with naive method  ---------------------------------------------------------- 
  # This fully places weighting on the most recent data
fc.naive <- naive(data.ts , h = 12 , lambda = "auto")
autoplot(fc.naive) +
  ylab("Bed Usage Count") +
  theme_stata() 

checkresiduals(fc.naive)
  # Residuals do not seem to have a pattern
  # ACF plot shows only 1 lags that breach 95% confidence interval
  # Ljung-Box test p-value > 0.05, suggesting residuals are white noise
  # These three things above suggest the model captured most of the signal

summary(fc.naive)


# Exponential smoothing method ------------------------------------------------------------------------
  # This places greater weight on recent data and lesser weight on older data
  # Holt-Winters method for seasonal data.
  # We didn't see any seasonality, so we'll roll with Holt
fc.Holt <- holt(data.ts , h = 12)

autoplot(fc.Holt)
summary(fc.Holt)

checkresiduals(fc.Holt)
  # Residuals do not seem to have a pattern
  # ACF plot shows 0 lags that breach 95% conf. interval
  # Ljung-Box test pvalue > 0.05, residuals white noise
  # Model captured most of the signal



# Let's give ets() a try to find a good model ----------------------------------------------------------
m.ets <- ets(data.ts)
summary(m.ets)

fc.ets <- forecast(m.ets , h = 12)

autoplot(fc.ets)
summary(fc.ets)

checkresiduals(fc.ets)
  # ACF suggests white noise. We captured most of the signal.


# Compare in-sample accuracy ----------------------------------------------------------
accuracy(fc.naive)
accuracy(fc.Holt)
accuracy(fc.ets)


# Let's give ARIMA a try  ----------------------------------------------------------

# Standardize variation
autoplot(data.ts)
MyLambda <- BoxCox.lambda(data.ts)
print(MyLambda)

data.ts2 <- BoxCox(data.ts , lambda = MyLambda)
autoplot(data.ts2)

# Check if we need to difference our data. Is the data stationary? Do unit root stationary tests
# Unit root indicates that the statistical properties of a given series are not constant with time
data.ts2 %>% ndiffs(test = "kpss")
data.ts2 %>% ndiffs(test = "adf")
data.ts2 %>% diff() %>% ndiffs(test = "kpss")
data.ts2 %>% diff() %>% ndiffs(test = "adf")
  # kpss and adf methods are in agreement
  # Only first difference needed
  # auto.arima() will check for differencing based on kpss

m.arima <- auto.arima(data.ts, 
                      lambda = "auto",
                      stepwise = FALSE,
                      parallel = TRUE,
                      approximation = FALSE)
summary(m.arima)

checkresiduals(m.arima)
  # Based on ACF, residuals look like white noise

fc.arima <- forecast(m.arima , h = 12)
autoplot(fc.arima)

summary(fc.arima)

fets <- function(x, h) {
  forecast(ets(x), h = h)
}

farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}

TheRMSE <- function(x){sqrt(mean(x^2, na.rm=TRUE))}

test1 <- tsCV(data.ts , fets , h = 12)
test2 <- tsCV(data.ts , farima , h = 12)

TheRMSE(test1)
TheRMSE(test2)


# Let's give a combined method a try  ----------------------------------------------------------
  # https://www.sciencedirect.com/science/article/pii/0169207089900125



train <- window(data.ts, end=c(2015,9))
h <- length(data.ts) - length(train)

ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE, stepwise = F , parallel = T, approximation = F),
                  h=h)
STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Megazord <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5


autoplot(data.ts) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  #autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Megazord, series="Megazord" , size = 1) +
  xlab("Time") + ylab("Bed Usage") +
  ggtitle("Chicago Homeless Shelter Bed Usage") +
  theme_stata()


c(ETS = accuracy(ETS, data.ts)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, data.ts)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, data.ts)["Test set","RMSE"],
  NNAR = accuracy(NNAR, data.ts)["Test set","RMSE"],
  TBATS = accuracy(TBATS, data.ts)["Test set","RMSE"],
  Combination =
    accuracy(Combination, data.ts)["Test set","RMSE"])