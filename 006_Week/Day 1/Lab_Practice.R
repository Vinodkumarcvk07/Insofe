#Import the ???.RDdata??? files into R setwd("??????????????????????????????.") load("Data.RData")


load("/Users/suresh/Desktop/Insofe/006_Week/Day 1/Data.RData")
head(data)
tail(data)
str(data)

unique(data$ProductKey)

library(sqldf)
RtData2.Day <-sqldf("select Date,min(Price) as MIN_PRICE from data group by Date")

str(RtData2.Day)
RtData2.Day$Date = as.Date(RtData2.Day$Date,format="%Y-%m-%d")

head(RtData2.Day)
sum(is.na(RtData2.Day))


minDate = min(as.Date(RtData2.Day$Date,format="%Y-%m-%d"))
maxDate = max(as.Date(RtData2.Day$Date,format="%Y-%m-%d"))
seq = data.frame(daterange = seq(minDate,maxDate,by="days"))
str(seq)
RtData2.Day2 = merge(seq,RtData2.Day,by.x="daterange",by.y="Date",all.x=T)
head(RtData2.Day2)
sum(is.na(RtData2.Day2))

library(zoo)
#x <- c(1,2,3,4,5,NA,7,8)
#na.locf(x)
#rev(na.locf(rev(x)))

RtData2.Day2$MIN_PRICE = na.locf(RtData2.Day2$MIN_PRICE)
head(RtData2.Day2)



RtData2.Day2$Week = as.numeric(format(RtData2.Day2$daterange,"%Y.%W")) 
head(RtData2.Day2)

library(sqldf)
str(RtData2.Day2)

RtData2.Week = sqldf("select Week as Week,min(MIN_PRICE) as MIN_PRICE from [RtData2.Day2] group by Week")
str(RtData2.Week)
head(RtData2.Week)

train = RtData2.Week[which(RtData2.Week$Week<=2013.47),]
test = RtData2.Week[which(RtData2.Week$Week>2013.47),]


price = ts(train$MIN_PRICE,frequency=52)
str(price)
head(price)

par(mfrow = c(1,1))
plot(price,type = 'l',lwd=3,col="blue",xlab="week",ylab='Price',main="Time Series Plot")
pricedecomp <- decompose(price)
plot(pricedecomp)

par(mfrow=c(1,2))
acf(price,lag=30)
pacf(price,lag=30)


price1 = ts(train$MIN_PRICE,frequency=1)

pricedecomp1 <- decompose(price1)
plot(pricedecomp1)

par(mfrow=c(1,2))
acf(price1,lag=52)
pacf(price1,lag=52)


par(mfrow=c(1,2)) 
plot(diff(price1,lag=1),type="l") 
plot(diff(price1,lag=2),type="l")



# The library TTR stands for Technical trading rules. 
library(TTR) 
fitsma <- SMA(price,n=3) 
length(fitsma) 
length(price) 
#Let us see how this model performs. 
#You could choose any of the error metrics. 
#Here we used MAPE to compute the error. 
smaMape <- mean(abs((price[3:length(price)]-fitsma[3:length(price)])/price[3:length(price)])) 
smaMape
fitwma<- WMA(price,n=2,1:2)
fitEma <- EMA(price, n = 2) 
emaMape <- mean(abs((price[2:length(price)]-fitEma[2:length(price)]) /price[2:length(price)])) 
emaMape


par(mfrow=c(2,2)) 
plot(train$MIN_PRICE, type="l", col="black") 
plot(fitsma, type="l", col="red") 
plot(fitwma, type="l", col="blue") 
plot(fitEma, type="l", col="brown") 
par(mfrow=c(1,1)) 
plot(train$MIN_PRICE, type="l", col="black") 
lines(fitsma,col="red",) 
lines(fitwma, col="blue") 
lines(fitEma, col="brown")


#Building the Holt winter???s model taking only Trend component. 

holtpriceforecast <- HoltWinters(train$MIN_PRICE, beta=TRUE, gamma=FALSE)
head(holtpriceforecast$fitted)

priceholtforecast <- HoltWinters(price, beta=TRUE, gamma=TRUE, seasonal="additive") 
# Look the fitted or forecasted values . Did you notice the 
head(priceholtforecast$fitted)


# Getting the predictions on Training data 
holtforecastTrain <- data.frame(priceholtforecast$fitted) 
holtforecastTrainpredictions <- holtforecastTrain$xhat 
head(holtforecastTrainpredictions) 
# To get the predictions on Test Data, you can use 
#function ???forecast.Holt???.???h??? indicates the number of future weeks 
#(or whatever be your reference time period, say months, quarters, etc.,) 
#for which you want to get the predictions 
library(forecast)
priceforecast <- forecast.HoltWinters(priceholtforecast, h=8)


# Model with no trend and no seasonality. 
model1 <- arima(price,c(0,0,0)) 
model1 
library("forecast")
forecast.Arima(model1, h=4) 
model2 <- arima(price,c(0,1,0)) 
model2
model3 <- arima(price,c(0,2,0)) 
model3
model4 <- arima(price,c(1,1,1)) 
model4
par(mfrow=c(1,4))
plot(model1$residuals,ylim=c(-50,50))
plot(model2$residuals,ylim=c(-50,50))
plot(model3$residuals,ylim=c(-50,50)) 
plot(model4$residuals,ylim=c(-50,50))

MODEL_ARIMA <- auto.arima(price, ic='aic') 
summary(MODEL_ARIMA)

acf(as.numeric(MODEL_ARIMA$residuals) ,lag.max = 20, main = "Residuals ACF plot") 
pacf(as.numeric(MODEL_ARIMA$residuals) ,lag.max = 20, main = "Residuals PACF plot")

pricearimaforecast = forecast.Arima(MODEL_ARIMA,h=4)
