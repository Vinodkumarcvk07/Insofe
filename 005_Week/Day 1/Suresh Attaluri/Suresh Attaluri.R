#Name:Suresh Attaluri
#1 a)

setwd("/Users/suresh/Desktop/Insofe/005_Week/Day 1")
product = read.csv("Product_Purchase_Info.csv",header=T)
product
product$Date<-as.factor(product$Date)
str(product)

#1b)
product$Date<-as.Date(product$Date,"%d-%m-%Y")
product
str(product)

#1c)
product$Month = format(product$Date,'%m')


#1d)
revenue = data.frame(tapply(product$Price,product$Month,sum))
colnames(revenue) = 'TotalRevenue'
revenue$Month = rownames(revenue)
#2
product$Product<-as.factor(product$Product)
price_data = product[,c('Product','Price')]
model = aov(Price~Product,data=price_data)
summary(model)
qf(.95,2,21)
#fvalue is greater than the critical region 3.4668 at 0.05 significance level we can reject the null hypothesis and confirm that the prices vary
qf(.99,2,21)
#fvalue is lesser than the critical region 5.780416 at 0.01 significance level we can accept the null hypothesis and confirm that the prices do not vary

#3

aggregate = product[,c('Product','Month','Price')]
library(reshape2)
dcast(aggregate, Month  ~  Product, value.var = "Price")

#4

library(ggplot2)
revenue$Month<-as.factor(revenue$Month)
str(revenue)
ggplot(revenue,aes(x=Month,y=TotalRevenue))+geom_point()+geom_smooth()

#5

mean(product$Price)
sd(product$Price)
as.numeric(apply(product,2,min)['Price'])-mean(product$Price)
as.numeric(apply(product,2,max)['Price'])-mean(product$Price)
