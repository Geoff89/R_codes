#Time series analysis
install.packages("forecast")
install.packages("randtests")
install.packages("TTR")
install.packages("tseries")
library("forecast")
library("TTR")
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("yeastExpData")
library("yeastExpData")
rain <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\rain.csv", header = TRUE, sep = "," )
birth <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\births.csv",header = TRUE, sep = "," )
kings <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\kings.csv", header = TRUE, sep = "," )
volcano <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\volcano.csv", header = TRUE, sep = "," )
sovenoir <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\sovenouir.csv", header = TRUE, sep = "," )
skirts <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\skirts.csv", header = TRUE, sep = "," )
skirts
skirtseries <- ts(skirts, start = c(1866))
plot.ts(skirtseries)
#plotting this we seethat it is not stationarry.Non stationary
skirtseriesdiff1 <- diff(skirtseries, differences = 1)
plot.ts(skirtseriesdiff1)
#diffferencing the second time
skirtseriesdiff2 <- diff(skirtseries, differences = 2)
plot.ts(skirtseriesdiff2)
#plotting time series for a king and differentating it
kings <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\kings.csv", header = TRUE, sep = "," )
kingstimeseries <- ts(kings)
kingstimeseries
plot.ts(kingstimeseries)
#not stationary so we diffrentiate
kingtimeseriesdiff1 <- diff(kingstimeseries, differences = 1)
plot.ts(kingtimeseriesdiff1)
#now weplot the values of the correlogram and pacf
acf(kingtimeseriesdiff1, lag.max = 20)#plot the correlogram
acf(kingtimeseriesdiff1, lag.max = 20, plot = FALSE)
#pacf function
pacf(kingtimeseriesdiff1, lag.max = 20)
pacf(kingtimeseriesdiff1, lag.max = 20, plot = FALSE)

library(forecast)
auto.arima(kingstimeseries)
#reading the volcano ash
volcano <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\volcano.csv", header = TRUE, sep = "," )
volcano$VOLC
volcano$DUS
volcanodustseries <- ts(volcano$DUS,start = c(1500))
volcanodustseries
plot.ts(volcanodustseries)
#No need to difference to fit the arima model
acf(volcanodustseries, lag.max = 20)
acf(volcanodustseries, lag.max = 20, plot = FALSE)
#findinf the autoregressive model of p
pacf(volcanodustseries, lag.max = 20)
pacf(volcanodustseries, lag.max = 20, plot = FALSE)
auto.arima(volcanodustseries)
#fiting this arima model in our time series king
kingstimeseriesarima <- arima(kingstimeseries, order = c(0,1,1))
kingstimeseriesarima
#Now that now we are forecasting
library(forecast)
kingtimeseriesarimaforecast <- forecast(kingstimeseriesarima, h=5)
kingtimeseriesarimaforecast
#plotting the forecatst
plot(kingtimeseriesarimaforecast)
#checking for forecast errors
acf(kingtimeseriesarimaforecast$residuals, lag.max = 20)
Box.test(kingtimeseriesarimaforecast$residuals, lag = 20, type = "Ljung-Box")
#To investigate whether the forecast errors are normally distributed 
#with mean zero and constant variance, we can make 
#a time plot and histogram (with overlaid normal curve) of the forecast errors:
plot.ts(kingtimeseriesarimaforecast$residuals)
plot(kingtimeseriesarimaforecast$residuals)
hist(kingtimeseriesarimaforecast$residuals)
mean(kingtimeseriesarimaforecast$residuals)
#ARIMA forecasting of volcanoes


#The rima model is (0,0,0)
volcanodustseriesarima <- arima(volcanodustseries, order = c(2,0,0))
volcanodustseriesarima
#forecast the arima model
volcanodustseriesarimaforecast <- forecast(volcanodustseriesarima, h = 31 )
volcanodustseriesarimaforecast
#plotting the forecast 
plot(volcanodustseriesarimaforecast)
#Checking to see if there are any correlations in the time series and normalization
acf(volcanodustseriesarimaforecast$residuals, lag.max = 20)
Box.test(volcanodustseriesarimaforecast$residuals, lag = 20, type = "Ljung-Box")
#Plotting to see  whether the time series is normal
plot.ts(volcanodustseriesarimaforecast$residuals)#make a time plot for forecast errors
plotForecastErrors(volcanodustseriesforecast$residuals)

#mean of forecast errors
mean(volcanodustseriesarimaforecast$residuals)

#Time series for souvenir and birthtime series
sovenoirtimeseries <- ts(sovenoir, frequency = 12, start = c(1987,1))
sovenoirtimeseries
birthstimeseries <- ts(birth, frequency = 12, start = c(1946,1))
birthstimeseries
raintimeseries<- ts(rain$inches, start = c(1813))
raintimeseries
#plotting thistime series
plot.ts(skirtseries)
plot.ts(kingstimeseries)
plot.ts(volcanodustseries)
plot.ts(birthstimeseries)
plot.ts(raintimeseries)
plot.ts(sovenoirtimeseries)
#for sovenoirtime series
plot.ts(sovenoirtimeseries)
#it is not an additive modelso we canuse log to makethe time series additive
logsovenoirtimeseries <- log(sovenoirtimeseries)
plot.ts(logsovenoirtimeseries)

#Decomposing time series data- trend component+irregular component or seasonal component
library(TTR)
kingstimeseriesSMA3 <- SMA(kingstimeseries, n = 3)
plot.ts(kingstimeseriesSMA3)
#the SMA of 3 is not predictive akinas there is a lot of fluctuations
#using smoothing if SMA 8 trial and error until we get the right fit
kingstimeseriesSMA8 <- SMA(kingstimeseries, n = 8)
plot.ts(kingstimeseriesSMA8)

#decomposing seasonal data
plot.ts(birthstimeseries)
#The series is additive therefore we can estimate the trend,seasonal and irregular(random) compoonets
birthtimeseriesdeco <- decompose(birthstimeseries)
birthtimeseriesdeco$seasonal
birthtimeseriesdeco$trend
birthtimeseriesdeco$random
#Plot the estimates to 
plot(birthtimeseriesdeco$trend)
plot(birthtimeseriesdeco$seasonal)
plot(birthtimeseriesdeco$random)
plot(birthtimeseriesdeco)

#seasonal adjusting
birthtimeseriescomponents <- decompose(birthstimeseries)
birthtimeseriescomponentsseasnadjusted <- birthstimeseries - birthtimeseriescomponents$seasonal
plot(birthtimeseriescomponentsseasnadjusted)

#simple exponential smoothing
rainseriesforecast <- HoltWinters(rain$annua, beta = FALSE, gamma = FALSE)
rainseriesforecast
#findng fiitting data
rainseriesforecast$fitted
#finding sum of squared errors
rainseriesforecast$SSE
#predicting using rhe first value
rainseriesforecast <- HoltWinters(rain$inches, beta = FALSE, gamma = FALSE, l.start = 23.56)
plot(rainseriesforecast)

#Forecasting beyond current periods
rainseriesforecast2 <- forecast(rainseriesforecast, h = 8)
rainseriesforecast2
plot(rainseriesforecast2)
#If the predictive model cannot be improved upon, there should be no correlations between
#forecast errors for successive predictions
acf(rainseriesforecast2$residuals, lag.max = 20)
Box.test(rainseriesforecast2$residuals, lag = 20, type = "Ljung-Box")
#furher imrove our model by ensring it has a normal distribution of mean 0 and
#constant variance
plot.ts(rainseriesforecast2$residuals)
#checking for normal distribution
plotForecastErrors <- function(forecasterrors){
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(rainseriesforecasts2$residuals)
