setwd("C:\\Users\\Classroom 4\\Desktop\\Day1")

#Regression MTcars data

mtcarslm <- lm(mpg~wt,data=mtcars)
summary(mtcarslm)
par(mfrow=c(1,1))
plot(mtcars$wt,mtcars$mpg)
abline(mtcarslm)
par(mfrow=c(2,2))
plot(mtcarslm)

# Regression

bigmac <- read.csv("BigMac-NetHourlyWage.csv", header = T, sep = ",")
bigmac
lm(bigmac$NetHourlyWage~bigmac$BigMacPrice,bigmac)
bigmaclm <- lm(NetHourlyWage~BigMacPrice,data=bigmac)
summary(bigmaclm)
par(mfrow=c(1,1))
plot(bigmac$BigMacPrice,bigmac$NetHourlyWage)
abline(bigmaclm)
par(mfrow=c(2,2))
plot(bigmaclm)

