

BikeShare = read.csv("/Users/suresh/Desktop/Insofe/004_Week/Day 2/BikeShare.csv",header = T)
str(BikeShare) 
BikeShare[,c("season","holiday","workingday","weather")] = apply(BikeShare[,c("season","holiday","workingday","weather")],2,function(x) as.factor(x))

BikeShare$weather <- factor(BikeShare$weather)
BikeShare$holiday <- factor(BikeShare$holiday)
BikeShare$workingday <- factor(BikeShare$workingday)
BikeShare$season <- factor(BikeShare$season)

BikeShare = BikeShare[,-which(colnames(BikeShare)%in%c("casual","registered"))]

sum(is.na(BikeShare))

library("caTools")

set.seed(107)
library(caret)
intrain = createDataPartition(y= BikeShare$count,p=0.75,list=FALSE)


train = BikeShare[intrain,]
test = BikeShare[-intrain,]
dim(train)
dim(test)

naive_model=lm(count~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed,data = train)
summary(naive_model)

naive_model$xlevels[["weather"]]<- union(naive_model$xlevels[["weather"]], levels(test$weather))

unique(BikeShare$weather)
unique(train$weather)

unique(BikeShare$season)
str(train$weather)
str(test$weather)
#Lets extract the predictions of the model for the TestData
OutputForTest0 <- predict(naive_model,newdata=test)

library(Metrics)

#Lets compute the root-mean-square error between actual and predicted
(Error0<-rmse(test$count,OutputForTest0))

#create day of week column
BikeShare$day <- weekdays(as.Date(BikeShare$datetime))
BikeShare$day <- factor(BikeShare$day)

#Now lets extract date and time from the datetime stamp, which is between 12th & 20th character of timestamp
BikeShare$time <- substring(BikeShare$datetime,12,20)  #substring cuts a input string at the places given by the arguments

hournum <- as.numeric(substr(BikeShare$time,1,2))

# Lets see which weekdays has how much demand
aggregate(BikeShare[,"count"],list(BikeShare$day),mean)

#second attempt
train2 <- BikeShare[intrain,]
test2 <- BikeShare[-intrain,]
naive_model2 <- lm(count~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed+ day +time,data=train2)


summary(naive_model2)
summary(naive_model)


#Lets extract the predictions of the model for the TestData
OutputForTest1 <- predict(naive_model2,newdata=test2)

#Lets compute the root-mean-square error between actual and predicted
( Error1<-rmse(test2$count,OutputForTest1))
( Error1<-rmse(test2$count,OutputForTest0))

#create daypart column, default to 4 to make things easier for ourselves
BikeShare$daypart <- "4"

str(BikeShare)

#4AM - 10AM = 1
BikeShare$daypart[(hournum < 10) & (hournum > 3)] <- 1

#11AM - 3PM = 2
BikeShare$daypart[(hournum < 16) & (hournum > 9)] <- 2

#4PM - 9PM = 3
BikeShare$daypart[(hournum < 22) & (hournum > 15)] <- 3

#convert daypart to factor
BikeShare$daypart <- as.factor(BikeShare$daypart)

#Third attempt
train3 <- BikeShare[intrain,]
test3 <- BikeShare[-intrain,]
naive_model3 <- lm(log(count)~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed+ day +time,data=train3)

summary(naive_model3)
summary(naive_model2)
library(MASS)
stepout = stepAIC(naive_model3,direction='both')
#Lets extract the predictions of the model for the TestData
OutputForTest2 <- predict(stepout,newdata=test3)

#Lets compute the root-mean-square error between actual and predicted
( Error1<-rmse(test2$count,exp(OutputForTest2)))
( Error1<-rmse(test2$count,OutputForTest1))

#divide Data to weekday and weekend
BikeShare$Weekend = "0"
BikeShare[(BikeShare$day =='Saturday') | (BikeShare$day =='Sunday'),'Weekend'] = 1
unique(BikeShare$Weekend)


train4 <- BikeShare[intrain,]
test4 <- BikeShare[-intrain,]
naive_model4 <- lm(log(count)~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed+ day+time+Weekend,data=train4)

summary(naive_model4)
summary(stepout)

library(MASS)
stepout = stepAIC(naive_model4,direction='both')
#Lets extract the predictions of the model for the TestData
OutputForTest3 <- predict(stepout,newdata=test4)
exp(OutputForTest3)[OutputForTest3<0]
#Lets compute the root-mean-square error between actual and predicted
( Error1<-rmse(test4$count,exp(OutputForTest3)))
( Error1<-rmse(test3$count,exp(OutputForTest1)))

rmsle(test4$count,exp(OutputForTest3))

train5 <- BikeShare[intrain,]
test5 <- BikeShare[-intrain,]
model5 = lm(log(count) ~ season + weather + temp + atemp + humidity + windspeed + Weekend + time,data=train5)
summary(model5)
OutputForTest5 <- predict(model5,newdata=test5)
rmsle(test5$count,exp(OutputForTest5))
rmse(test5$count,exp(OutputForTest5))
unique(train5$time)


par(mfrow=c(2,2))
plot((model5))

shapiro.test(OutputForTest5)

BikeShare[c(31,355,719),]

