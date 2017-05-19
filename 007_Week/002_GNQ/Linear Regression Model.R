
remove(list=ls())

housing_data = read.table("/Users/suresh/Desktop/Insofe/007_Week/002_GNQ/20170513_Batch 28_CSE 7202c_GNQ_Data/housing.data.txt",header=F)
head(housing_data)

#library(vegan)
#data_no_target = housing_data[,-which(colnames(housing_data)=="MEDV")]
#housing_data[,-which(colnames(housing_data)=="MEDV")][,sapply(housing_data[,-which(colnames(housing_data)=="MEDV")],is.numeric)]=decostand(data_no_target[,sapply(data_no_target,is.numeric)],"standardize")


#a

colnames(housing_data) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
str(housing_data)

housing_data$CHAS = as.factor(as.character(housing_data$CHAS))

#b

library("corrplot")
corrplot(cor(housing_data[,sapply(housing_data,is.numeric)]))
cor(housing_data$MEDV,housing_data$RM)
cor(housing_data$MEDV,housing_data$LSTAT)
#Looking at the corelation plot MEDV is negatively coorelated with LSTAT
# and positively corelated with RM

#c

set.seed(1234)
library(caret)
intrain = createDataPartition(y= housing_data$MEDV,p=0.80,list=FALSE)
train = housing_data[intrain,]
test  = housing_data[-intrain,]

linreg = lm(formula = MEDV~.,data=train) 
summary(linreg)

predict_data_train = predict(linreg,newdata = train)
predict_data_test = predict(linreg,newdata = test)
regr.eval(train$MEDV,predict_data_train)
regr.eval(test$MEDV,predict_data_test)

# following are eror metrics for train and test
# train
# mae        mse       rmse       mape 
# 3.1126094 19.0347149  4.3628792  0.1532102
# test
# mae        mse       rmse       mape 
# 3.9348952 34.6509434  5.8865052  0.2137763

#Since I am looking at mape and see that the mape value in both train and test is very high.
#Also the mape value for train and test has high variance. 153% for train and 213% test We should expect closer results for both test and train datasets.

#d

par(mfrow=c(2,2))
plot(linreg)

#We can observe a pattern available in residuals and fitterd value plot
#From Q-Q plot we can see that the residuals are not normal
#By these observations we can say that the model does not follow the linear regression assumptions

library(MASS)
stepout = stepAIC(linreg,direction= "both")
summary(stepout)
library(car)

vif(stepout)

predict_data_train = predict(stepout,newdata = train)
predict_data_test = predict(stepout,newdata = test)
library(DMwR)
regr.eval(train$MEDV,predict_data_train)
regr.eval(test$MEDV,predict_data_test)

#tried to do step AIC but the results are similar to that of the naive model

housing_data$Crimerate<-NA


for (i in 1:nrow(housing_data)){
  if (housing_data$CRIM[i]>=5){ 
    housing_data$Crimerate[i]="High"
  }
  else {
    housing_data$Crimerate[i]="Low"
  }
}


table(housing_data$LSTAT)
str(housing_data)
housing_data$Crimerate = as.factor(housing_data$Crimerate)
housing_data = housing_data[-which(rownames(housing_data)%in%c(413,373)),]
train = housing_data[intrain,]
test  = housing_data[-intrain,]


linreg_transform = lm(formula = log(MEDV)~CRIM + ZN + CHAS + NOX + RM + DIS + RAD + 
                        TAX + PTRATIO + B + LSTAT,data=train) 

predict_data_train = predict(linreg_transform,newdata = train)
predict_data_test = predict(linreg_transform,newdata = test)
regr.eval(train$MEDV,exp(predict_data_train))
regr.eval(test$MEDV,exp(predict_data_test))

data.frame(actual = train$MEDV,predicted=exp(predict_data_train))

par(mfrow=c(2,2))
plot(linreg_transform)


#I have added new feature to the CRIM column clasifying the data as low, medium and high 
#and used log transformation for the MEDV and the percentage of the mape has reduced from 150 to 120


#e

#The performance of the model has improved after the tarnsformations as the error metric got reduced also the 
#hetroscedarstiy in the graphs also reduced.


