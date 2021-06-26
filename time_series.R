#Author: Pierre Mulliez,
#Last edited: 17/06/2021
#Project: Covid prediction
#time series

#Load libraries 
library(data.table)  
library(fBasics)
library(dplyr)
library(readxl)
library(forecast) 
library(lubridate)

#uniform way to get file
getwd()
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

path_train = "data/train.csv"
path_location = "data/country_location.xlsx"
data = read.csv(path_train)
data2 = data
dat2 = read_xlsx(path_location)
dat2 = data.frame(dat2)
tail(data)
summary(data)


#Cases
Cases <- data.frame(data2) %>%  group_by(grouped_date = Date) %>% summarise(cases=sum(ConfirmedCases)) %>% arrange(grouped_date)
#fatalities
Fatalities <- data.frame(data2) %>%  group_by(grouped_date = Date) %>% summarise(fatal=sum(Fatalities)) %>% arrange(grouped_date)


#what day is the minimum and maximum
as.Date(min(data$Date))
as.Date(max(data$Date))


#input case or fatality here
val <- Fatalities
val
#time series class weekly 

#####convert to time serie class
#ts method
y <- ts(val$fatal, start=c(2020, 22), frequency=365)
y

#xts method
#library(xts)
#xts(val$fatal,order_by(as.Date(val$grouped_date)))
#?xts












ts.plot(val)

par(mfrow=c(2,1))
acf(val)
pacf(val)  



ndiffs(y, alpha=0.05, test=c("adf")) # number of regular differences?


#take 2 diff off to achieve stationarity (mean = 0 and variance = 0 )
b<-diff(y)  
z <- diff(b)
ts.plot(z)

par(mfrow=c(3,1))
ts.plot(z)   
acf(z)
pacf(z)
ndiffs(z, alpha=0.05, test=c("adf")) 



# formal normality test
# Ho: the data is normally distributed
# H1: the data is not normally distributed
shapiro.test(z)
#B.	Testing for WHITE NOISE graphically


# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test (z, lag = 20, type="Ljung")  # Null: ro1=.=ro20=0

arg <- Arima(y,order=c(7,2,0))
arg
#abs(0.3075/0.0389) #is it over 1.96? -> significant #order 1
abs( 0.4051/ 0.0858 ) #order 6 -> contribution r**2?

#residual correlation
Box.test(arg$residuals, lag = 20) 
par(mfrow=c(3,1))
ts.plot(arg$residuals)   
acf(arg$residuals)
pacf(arg$residuals)
#residual strict white noise ? -> volatility -> square residuals
#Checking for normality - Gaussian white noise 

shapiro.test(arg$residuals)

hist(z,prob=T,ylim=c(0,0.6),xlim=c(mean(z)-3*sd(z),mean(z)+3*sd(z)),col="red")
lines(density(z),lwd=2)
mu<-mean(z)
sigma<-sd(z)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

#squared residuals -> strict white noise
par(mar=c(1,1,1,1)) # to adjust graphic size

par(mfrow=c(3,1)) # analysis of the squared data
ts.plot(arg$residuals^2)   
acf(arg$residuals^2)
pacf(arg$residuals^2)

#not strict white noise -> (out of bound value)
#quadratic model, predict variance ?


#####predict 
predict(arg,n.ahead = 5) 


##############Variance model #############

par(mar=c(1,1,1,1)) 

par(mfrow=c(3,1)) # analysis of the squared data
ts.plot(y^2)   
acf(y^2)
pacf(y^2)


ndiffs(y^2, alpha=0.05, test=c("adf")) 
#comment: value decreasing in the ACF: stationary 


# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test(y^2,lag=20, type="Ljung")    # Null: ro1=.=ro20=0


arg <- Arima(y^2,order=c(1,2,0))
arg

abs( 0.1265/0.0929) #is it over 1.96? -> significant #order 1

####value less significant for a variance model 
predict(arg,n.ahead = 5) 
#compare to last value of occurances (20 first value as we analyse 20 lags )
val[1:20,]



#######auto arima ########
arg <- auto.arima(y)
arg
abs(  0.9902/0.1211) #is it over 1.96? -> significant #order 1 ar
abs(0.7563/0.0760) #ma order 2

predict(arg,n.ahead = 5) 
val[1:20,]
y


#####nSeasonality ######

