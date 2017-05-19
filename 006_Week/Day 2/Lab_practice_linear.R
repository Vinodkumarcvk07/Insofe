remove(list=ls())
auto_mobiles = read.csv("/Users/suresh/Desktop/Insofe/006_Week/Day 2/Automobiles.csv",header= T,sep="\t")
head(auto_mobiles)
str(auto_mobiles)


sum(is.na(auto_mobiles))

auto_mobiles = auto_mobiles[!is.na(auto_mobiles$price),]
auto_mobiles = auto_mobiles[!auto_mobiles$num_cyl%in%c("three","twelve"),]
auto_mobiles[rownames(auto_mobiles)%in%c(135,67,2,106),]
auto_mobiles = auto_mobiles[!rownames(auto_mobiles)%in%c(135,67,2,106),]


auto_mobiles$num_cyl = as.character(auto_mobiles$num_cyl)

#auto_mobiles[auto_mobiles$num_cyl%in%c("twelve"),"num_cyl"] = "eight"
#auto_mobiles[auto_mobiles$num_cyl%in%c("three"),"num_cyl"] = "two"

#auto_mobiles[auto_mobiles$num_cyl%in%c("two"),"num_cyl"]="2"
# auto_mobiles[auto_mobiles$num_cyl%in%c("three"),"num_cyl"]="3"
# auto_mobiles[auto_mobiles$num_cyl%in%c("four"),"num_cyl"]="4"
# auto_mobiles[auto_mobiles$num_cyl%in%c("five"),"num_cyl"]="5"
# auto_mobiles[auto_mobiles$num_cyl%in%c("six"),"num_cyl"]="6"
# auto_mobiles[auto_mobiles$num_cyl%in%c("eight"),"num_cyl"]="3"
# auto_mobiles[auto_mobiles$num_cyl%in%c("twelve"),"num_cyl"]="12"

auto_mobiles$num_cyl = as.factor(as.character(auto_mobiles$num_cyl))
table(auto_mobiles$num_cyl)
unique(auto_mobiles$num_cyl)

sapply(auto_mobiles,FUN=function(x){sum(is.na(x))})


# unique(auto_mobiles$peak_rpm)
# min(auto_mobiles[!is.na(auto_mobiles$peak_rpm),"peak_rpm"])
# max(auto_mobiles[!is.na(auto_mobiles$peak_rpm),"peak_rpm"])
# 
# auto_mobiles[is.na(auto_mobiles$peak_rpm),"peak_rpm"] = mean(auto_mobiles[!is.na(auto_mobiles$peak_rpm),"peak_rpm"])
# 
# cor(auto_mobiles[!is.na(auto_mobiles$normalized_losses),"normalized_losses"],auto_mobiles[!is.na(auto_mobiles$normalized_losses),"price"])

library(DMwR)

auto_mobiles<-knnImputation(auto_mobiles,k=5) 

sapply(auto_mobiles,FUN=function(x){sum(is.na(x))})

cor(auto_mobiles[!is.na(auto_mobiles$normalized_losses),"normalized_losses"],auto_mobiles[!is.na(auto_mobiles$normalized_losses),"price"])

library(vegan)

auto_mobiles[,sapply(auto_mobiles,is.numeric)] = decostand(auto_mobiles[,sapply(auto_mobiles,is.numeric)],'standardize')
sapply(auto_mobiles,is.numeric)
set.seed(1234)
library(caret)
intrain = createDataPartition(y= auto_mobiles$price,p=0.75,list=FALSE)
train = auto_mobiles[intrain,]
test  = auto_mobiles[-intrain,]

str(train)
names(train)
lireg = lm(formula = price~.,data=train)
summary(lireg)

lireg$xlevels[["fuel_system"]]<- union(lireg$xlevels[["fuel_system"]], levels(test$fuel_system))

predict_data = predict(lireg,newdata = test)

regr.eval(test$price,predict_data)

par(mfrow=c(2,2))
plot(lireg)

library(car)
library(MASS)
stepout = stepAIC(lireg,direction="both")

vif(stepout)
str(train)

aic_predict_data = predict(stepout,newdata = test)
regr.eval(test$price,aic_predict_data)
plot(stepout)
summary(stepout)
train = train[!rownames(train)%in%c(14,39,43,17),]


lireg_outlier = lm(formula = price ~ drive_wheel + width + height + num_cyl + 
                     bore + stroke + compression_ratio + hoesepower + 
                     peak_rpm, data = train)


lireg_outlier_predict = predict(lireg_outlier,newdata = test)
regr.eval(test$price,lireg_outlier_predict)
regr.eval(test$price,aic_predict_data)
plot(lireg_outlier)
summary(lireg_outlier)

train = auto_mobiles[intrain,]
test  = auto_mobiles[-intrain,]

lireg_outlier2 = lm(formula = log(price) ~ drive_wheel + width + height + num_cyl + 
                      bore + stroke + compression_ratio
                      + hoesepower + 
                      peak_rpm, data = train)

lireg_outlier2_predict_train = predict(lireg_outlier2)
lireg_outlier2_predict_test = predict(lireg_outlier2,newdata = test)
regr.eval(train$price,exp(lireg_outlier2_predict_train))
regr.eval(test$price,exp(lireg_outlier2_predict_test))

par(mfrow=c(2,2))
plot(lireg_outlier2)

summary(lireg_outlier2)
data.frame(test_price = test$price,predict=exp(lireg_outlier2_predict))

min(train$hoesepower)
max(train$hoesepower)
table(train$hoesepower)
train[train$compression_ratio = 7.6]
train[rownames(train)%in%c(17,135),]

train



lireg_outlier3 = lm(formula = log(price) ~ drive_wheel + wheel_base + 
                      stroke + compression_ratio + hoesepower + peak_rpm, data = train)


lireg_outlier3_predict = predict(lireg_outlier3,newdata = test)
regr.eval(test$price,exp(lireg_outlier3_predict))
regr.eval(test$price,exp(lireg_outlier2_predict))

par(mfrow=c(2,2))
plot(lireg_outlier3)
summary(lireg_outlier3)



par(mfrow=c(1,1))
marked <- identify(lireg_outlier2$fitted.values,lireg_outlier2$residuals)
train[marked,]


automobiles = auto_mobiles[!rownames(auto_mobiles)%in%c(99,68),]
