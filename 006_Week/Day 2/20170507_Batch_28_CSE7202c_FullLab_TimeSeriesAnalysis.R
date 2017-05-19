rm(list=ls(all=TRUE))
# Set the working directory
setwd(""C:/Users/Mahesh/Desktop/20170507/TS"")
# Loading the data
data <- read.csv("DeliveryRate.csv")
str(data)
head(data)
tail(data)

#Date conversion
data$Month=as.Date(data$Month,format="%d-%m-%Y")

# To find the minimum of the dates 
minDate=min(as.Date(data$Month,format="%Y-%m-%d"))
# To find the maximum of the dates
maxDate =max(as.Date(data$Month,format="%Y-%m-%d"))
# generating the series of dates
seq <- data.frame("dateRange"=seq(minDate,maxDate,by="months"))

# left joining to see the missing values for the dates. all.x will do the left join."all.y" will do right join.  
data2= merge(seq,data,by.x="dateRange",by.y="Month",all.x=T)
head(data2)

library(zoo)
#Imputing Missing Values
data2$DeliveryRate=(na.locf(data2$DeliveryRate) + 
                          rev(na.locf(rev(data2$DeliveryRate))))/2
head(data2)

#Dividing data as Train & Test
Train=data2$DeliveryRate[1:32]
Test=data2$DeliveryRate[33:36]

# Creating the Time series Object
Train<- ts(Train,frequency=12,start=c(2013,1))
Train

#Plotting
plot.ts(Train)

#Decomposition
datatimeseriescomponents <- decompose(Train)
plot(datatimeseriescomponents)

#ACF and PACF of real world data
par(mfrow=c(1,3))
plot.ts(Train)
acf(Train, lag.max=20)
pacf(Train, lag.max=20)


### MAPE Function #####
#https://en.wikipedia.org/wiki/Mean_absolute_percentage_error
FnMAPE<-function(A,F){
  MAPE=mean(abs((A-F)/A))*100
  return(MAPE)
}
#Example
a=c(10,20)
f=c(12,18)
((abs((10-12)/10)+abs((20-18)/20))/2)*100
FnMAPE(a,f)

# The library TTR stands for Technical trading rules. 
library(TTR)

#Moving Averages
fitsma <- SMA(Train,n=2)
length(fitsma)
length(Train)
Train
fitsma
smaMape_train <- FnMAPE(Train[3:length(Train)],fitsma[2:(length(fitsma)-1)])

fitwma<- WMA(Train,n=2,1:2)
fitEma <- EMA(Train, n = 2)

#Holts Winter
HW <-  HoltWinters(Train, beta=TRUE, gamma=TRUE)
HW
HW_F=data.frame(HW$fitted)
Train
HW_MAPE_Train=FnMAPE(Train[13:32],HW_F$xhat)
HW_MAPE_Train
HW_F2<-data.frame(forecast.HoltWinters(HW, h=4))
HW_F2
HW_MAPE_Test=FnMAPE(Test,HW_F2$Point.Forecast)
HW_MAPE_Test
par(mfrow=c(1,1))
plot.forecast(forecast.HoltWinters(HW, h=4))
#ARIMA
library("forecast")
MODEL_ARIMA <- auto.arima(Train, ic='aic')
MODEL_ARIMA$fitted
Train
length(MODEL_ARIMA$fitted)
MAPE_Train=FnMAPE(Train,MODEL_ARIMA$fitted)
MAPE_Train
forecast.Arima(MODEL_ARIMA, h=4)
ARIMA_F <- data.frame(forecast.Arima(MODEL_ARIMA, h=4))
ARIMA_F
MAPE_Test=FnMAPE(Test,ARIMA_F$Point.Forecast)
MAPE_Test

par(mfrow=c(2,1))
# Lest us look at the acf and Pacf graphs to check if 
# there are patterns
acf(as.numeric(MODEL_ARIMA$residuals) ,lag.max = 30, 
    main = "Residuals ACF plot")
pacf(as.numeric(MODEL_ARIMA$residuals) ,lag.max = 30, 
     main = "Residuals PACF plot")
par(mfrow=c(1,1))
plot.forecast(forecast.Arima(MODEL_ARIMA, h=4))
