timeseries = read.csv("/Users/suresh/Desktop/Insofe/006_Week/Day 2/DeliveryRate.csv",header =T)
str(timeseries)



timeseries$Date = as.Date(timeseries$Month,format="%d-%m-%Y")

minDate = min(timeseries$Date)
maxDate = max(timeseries$Date)
seq = data.frame(daterange = seq(minDate,maxDate,by="months"))

missing_timeseries = merge(seq,timeseries,by.x="daterange",by.y="Date",all.x=T)[,c('daterange','DeliveryRate')]

sum(is.na(missing_timeseries))


library(zoo)
missing_timeseries$DeliveryRate = na.locf(missing_timeseries$DeliveryRate)
final_data = missing_timeseries


train = final_data[as.numeric(rownames(final_data))<33,]
test = final_data[!as.numeric(rownames(final_data))<33,]

deliveryrate = ts(train$DeliveryRate,frequency = 12,start=c(2013,01))

par(mfrow = c(1,1))
plot(deliveryrate,type = 'l',lwd=3,col="blue",xlab="Month",ylab='DeliveryRate',main="Time Series Plot")
deliveryratedecomp <- decompose(deliveryrate)
plot(deliveryratedecomp)

par(mfrow=c(1,2))
acf(deliveryrate,lag=24)
pacf(deliveryrate,lag=24)


holtforecast <- HoltWinters(deliveryrate, beta=TRUE, gamma=TRUE, seasonal="additive") 
head(holtforecast$fitted)
summary(holtforecast)
holtforecast_train = data.frame(holtforecast$fitted)
holtforecast_train_predictions = holtforecast_train$xhat

library(forecast)
deliveryrateforecast = forecast(holtforecast,h=4)



