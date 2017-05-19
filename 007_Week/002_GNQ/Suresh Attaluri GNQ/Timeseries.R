
#a

data = read.csv("/Users/suresh/Desktop/Insofe/007_Week/002_GNQ/20170513_Batch 28_CSE 7202c_GNQ_Data/TSData.csv",header=T)
str(data)

data$Date = as.Date(data$Date,format="%d-%m-%Y")

minDate = min(data$Date)
maxDate = max(data$Date)
seq = data.frame(daterange = seq(minDate,maxDate,by="weeks"))


missing_data = merge(seq,data,by.x="daterange",by.y="Date",all.x=T)[,c('daterange','Value')]

library(zoo)
missing_data$Value = na.locf(missing_data$Value)
missing_data$Week = as.numeric(format(missing_data$daterange,"%Y.%W")) 
final_data = missing_data
#b

train = final_data[as.numeric(rownames(final_data))<975,]
test = final_data[!as.numeric(rownames(final_data))<975,]

#c
final_data[1,]
Value = ts(train$Value,frequency = 52,start=c(2010,00))


par(mfrow=c(1,2))
acf(Value,lag=30)
pacf(Value,lag=30)

# lag 2 has some significance

#d

priceholtforecast <- HoltWinters(Value, beta=TRUE, gamma=TRUE, seasonal="additive") 
holtforecastTrain <- data.frame(priceholtforecast$fitted) 
library(forecast)
priceforecast <- forecast.HoltWinters(priceholtforecast, h=10)
