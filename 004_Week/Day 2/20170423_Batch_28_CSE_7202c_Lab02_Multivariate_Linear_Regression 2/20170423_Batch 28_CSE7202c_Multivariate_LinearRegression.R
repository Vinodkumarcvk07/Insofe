# Problem Statement: 
# An online gaming portal wants to understand their customer patterns based on their 
# transactional behavior. For this, they have constructed a customer level data based 
# on the details they are tracking. The customer database consists of demographic and 
# transactional information for each customer. Building a regression model to predict 
# the customer revenue based on other factors.
 
rm(list=ls(all=TRUE))
setwd("F:\\TA\\Labs\\Batch 28\\CSE 7202c\\Day2_MLR\\Reg_LabActivity")
#Reading data
data<-read.csv("CustomerData.csv",header=T)

#Data preparation
names(data)
str(data)
data<-data[,-1]
sum(is.na(data))
data$City<-as.factor(data$City)
str(data)

## Standardize the numeric attributes.
library(dplyr)
num_attributes = data[,2:10]
library(vegan)
num_attributes = apply(num_attributes, 2, function(x){decostand(x,method = 'standardize')})
data[,2:10] = num_attributes


####Splitting the Data as training  and testing
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train = data[trainRows,] 
test = data[-trainRows,]

##############################################################
##########****Naive Model with standardizing****###########
# Build the naive model again to check for improvement.
naive_model = lm(TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
                   MaxAgeOfChild + Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + 
                   FrequencyOFPlay + NoOfGamesPlayed + NoOfGamesBought + FavoriteChannelOfTransaction + 
                   FavoriteGame, data=train)
summary(naive_model)

##############################################################
#############*****Check for multicolliearity*****#############
#Multicollinearity check
#Variance inflation factor
library(car)
vif(LinReg_Aic)
LinReg<- lm(TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
              MaxAgeOfChild + Tenure + NoOfUnitsPurchased + 
              FrequencyOFPlay + NoOfGamesPlayed  + FavoriteChannelOfTransaction + 
              FavoriteGame, data=train)
summary(LinReg)

##############################################################
###################*******Step AIC*******#####################
# Stepwise Regression
library(MASS)
step <- stepAIC(LinReg, direction="both")
step
LinReg_Aic<- lm(formula = TotalRevenueGenerated ~ City + MinAgeOfChild + Tenure + 
     NoOfUnitsPurchased + FrequencyOFPlay + NoOfGamesPlayed + 
     FavoriteChannelOfTransaction + FavoriteGame, data = train)
summary(LinReg_Aic)

##############################################################
################*********Error metrics********################
library(DMwR)
#Error verification on train data
regr.eval(train$TotalRevenueGenerated, LinReg_Aic$fitted.values) 
#Error verification on test data
Pred<-predict(LinReg_Aic,test)
regr.eval(test$TotalRevenueGenerated, Pred) 

##############################################################
###############********Diagnostics plots********##############
#Analyzing residuals
par(mfrow=c(2,2))
plot((LinReg_Aic))

#Influential Observations 
#Leverage
lev= hat(model.matrix(LinReg_Aic))
plot(lev)
# obtain leverage values greater than 0.2
train[lev>0.2,]
train<-train[-(lev>0.2),]

#cooks distance
cook = cooks.distance(LinReg_Aic)
plot(cook,ylab="Cooks distances")
max=as.numeric(which.max(cook))
points(max,cook[max],col='red', pch=19)
train[max,]

#Residual outliers
residuals = LinReg_Aic$residuals
outliers <- boxplot(residuals,plot=T)$out
sort(outliers)
train[residuals%in% outliers,]

eval<-read.csv("Eval.csv",header=T)
#Error verification on test data
eval$City<-as.factor(eval$City)
Pred<-predict(LinReg_Aic,eval)
regr.eval(eval$TotalRevenueGenerated, Pred) 


