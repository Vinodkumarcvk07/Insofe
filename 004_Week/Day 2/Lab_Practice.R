
customer<-read.csv("/Users/suresh/Desktop/Insofe/004_Week/Day 2/CustomerData.csv",header= T)
names(customer)
str(customer)
customer<-customer[,-1]
sum(is.na(customer))
customer$City<-as.factor(customer$City)
str(customer)



## Standardize the numeric attributes.
library(dplyr)
num_attributes = customer[,2:10]
library(vegan)
head(num_attributes)
num_attributes = apply(num_attributes, 2, function(x){decostand(x,method = 'standardize')})
customer[,2:10] = num_attributes


####Splitting the Data as training  and testing
rows=seq(1,nrow(customer),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(customer))/100)
train = customer[trainRows,]
test = customer[-trainRows,]


##############################################################
##########****Naive Model without standardizing****###########
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
vif(naive_model)
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
summary(step)
LinReg_Aic<- lm(formula = TotalRevenueGenerated ~ City + MinAgeOfChild + Tenure + 
                  NoOfUnitsPurchased + FrequencyOFPlay + NoOfGamesPlayed + 
                  FavoriteChannelOfTransaction + FavoriteGame, data = train)
summary(LinReg_Aic)

##############################################################
################*********Error metrics********################
library(DMwR)
#Error verification on train data
regr.eval(train$TotalRevenueGenerated, LinReg_Aic$fitted.values) 
regr.eval(train$TotalRevenueGenerated, LinReg$fitted.values) 
#Error verification on test data
Pred<-predict(LinReg_Aic,test)
regr.eval(test$TotalRevenueGenerated, Pred)


Pred<-predict(LinReg,test)
regr.eval(test$TotalRevenueGenerated, Pred)

##############################################################
###############********Diagnostics plots********##############
#Analyzing residuals
par(mfrow=c(2,2))
plot((LinReg))
names(LinReg_Aic)
head(LinReg_Aic$fitted.values)
#Influential Observations 
#Leverage
lev= hat(model.matrix(LinReg))
lev
plot(lev)
# obtain leverage values greater than 0.2
train[lev>0.2,]
train<-train[-(lev>0.2),]

#cooks distance
cook = cooks.distance(LinReg)
plot(cook,ylab="Cooks distances")
max=as.numeric(which.max(cook))
points(max,cook[max],col='red', pch=19)

train[max,]
row.names(train[max,])
customer[row.names(train[max,]),]
d[row.names(train[max,]),]
d<-read.csv("/Users/suresh/Desktop/Insofe/004_Week/Day 2/CustomerData.csv",header= T)

#Residual outliers
residuals = LinReg$residuals
outliers <- boxplot(residuals,plot=T)$out
sort(outliers)
train[residuals%in% outliers,]

eval<-read.csv("/Users/suresh/Desktop/Insofe/004_Week/Day 2/Eval.csv",header=T)
#Error verification on test data
head(eval)
eval= eval[,-1]
eval$City<-as.factor(eval$City)
Pred<-predict(LinReg,eval)
regr.eval(eval$TotalRevenueGenerated, Pred) 

