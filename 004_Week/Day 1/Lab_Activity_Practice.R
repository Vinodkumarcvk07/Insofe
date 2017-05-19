data<-read.csv("/Users/suresh/Desktop/Insofe/004_Week/CustomerData.csv",header= T)

sum(is.na(data))
str(data)

name <- c("NoOfChildren","MinAgeOfChild","MaxAgeOfChild","Tenure","FrquncyOfPurchase"
          ,"NoOfUnitsPurchased","FrequencyOFPlay","NoOfGamesPlayed","NoOfGamesBought"
          ,"TotalRevenueGenerated")

data_num<-data[,names(data)%in%name]
str(data_num)
  
install.packages("corrplot")
library("corrplot")
corrplot(cor(data_num))
cor(data_num$NoOfUnitsPurchased,data_num$TotalRevenueGenerated)
par(mfrow=c(1,1))
plot(data_num$NoOfUnitsPurchased,data_num$TotalRevenueGenerated)

modl_lm <- lm(TotalRevenueGenerated~NoOfUnitsPurchased,data = data_num)
summary(modl_lm)

data_num$predictedd_revenue = predict(modl_lm,newdata= data_num)
regr.eval(data_num$TotalRevenueGenerated,data_num$predictedd_revenue)
str(data_num)


par(mfrow=c(1,1))
plot(modl_lm)

data_num$lev<-(((data_num$NoOfUnitsPurchased - mean(data_num$NoOfUnitsPurchased))/sd(data_num$NoOfUnitsPurchased))^2 +1)/3209
data_num$resid<-modl_lm$residuals
names(modl_lm)
shapiro.test(data_num$resid)



data[which.max(data_num$lev),] 
# Observe that the point with highest leverage is point 33 which has a residual of 171.18

#From the plot, we observe that point 10 has a highest residual
data[which.max(data_num$resid),]
#Observe that though, this point has high residual its leverage is not very high.

#Which of these points are outliers. Lets consider cook's distance
data_num$cook<-round(cooks.distance(modl_lm),2)

data2 = data_num[-c(974,1764,1489,2046),c("NoOfUnitsPurchased","TotalRevenueGenerated")]
str(data2)
