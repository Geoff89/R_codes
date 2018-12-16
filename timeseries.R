#tseries
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
myVector <- c(seq(from = 1, to = 72))
myts <- ts(myVector, start=c(2009, 1), end=c(2014, 12), frequency=12)
myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12))
plot(myts)
acf(myts)
pacf(myts)

#fitting an ARIMA Model
library(forecast)
library(tseries)
library(randtests)
# fit an ARIMA model of order P, D, Q
fit <- arima(myts, order=c(p, d, q))
fit <- arima(myts, order = c(0,1,1))
fit <- arima(myts, order = c(2,0,0))
fit <- arima(myts, order = c(0,1,2))
fit             
# predictive accuracy
library(forecast)
accuracy(fit)

# predict next 5 observations
library(forecast)
forecast(fit, 5)
plot(forecast(fit, 5)) 

#airline passengers
data("AirPassengers")
head(AirPassengers)
View(AirPassengers)
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
str(AirPassengers)
table(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)
#plotting to see the distribution
plot(AirPassengers)
#fitting a line in the series
abline(reg = lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers)#This will print the cycles across years
plot(aggregate(AirPassengers,FUN=mean))#This will aggregate the cycles and display a year on year trend
boxplot(AirPassengers~cycle(AirPassengers))#Box plot across months will give us a sense on seasonal effect
library(tseries)
#testing stationarrity while addressing trend and variance in the series
adf.test(diff(log(AirPassengers)), alternative = "stationary", k=0)
#Finding the right parameters to be used in modelling
acf(log(AirPassengers))
#with the above the acf plotis not stationary so we differentiate
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

#ARIMA model
sim.ar <- arima.sim(list(ar = c(0.4,0.4)), n = 1000)
sim.ma <- arima.sim(list(ma = c(0.6, -0.4)), n = 1000)
sim.ar
sim.ma
par(mfrow = c(2,2))
#plot the correlograms
acf(sim.ar, main = "ACF of AR(2) process")
acf(sim.ma, main = "ACF of MA(2) process")
pacf(sim.ar, main = "PACF of AR(2) process")
pacf(sim.ma, main = "PACF of MA(2) process")
#fitting arima model in lake harun model
data("LakeHuron")
fit <- arima(LakeHuron, order = c(1,0,0))
fit
#Diagostic checking to check for any non-randomness
tsdiag(fit)
#Nairobi securities exchange NASI
nasi <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\NASI.csv", header = TRUE, sep =",",stringsAsFactors = FALSE)
nasi
nasi$NASI
apply(na.omit(nasi))
apply()
class(nasi$NASI)
class(nasi$Date)
str(nasi$NASI)
adf.test(nasi$NASI,)
runs.test(nasi$NASI)
range(nasi$NASI)
factor(nasi$NASI, exclude = NA)
nasi <- factor(nasi, exclude = NA)
na.omit(your.data.frame)
x <- c(1,2,NA,3)
mean(x, na.rm=TRUE)
newdata <- na.omit(mydata) 
#dataframe
data<-data.frame(read.csv("test.csv"))
apply(data,2,max,na.rm=TRUE); # this will remove the NA's from columns that contain them
apply(na.omit(data),2,max); ## this will remove the NA rows from the data frame and then calculate the max values