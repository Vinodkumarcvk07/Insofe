setwd("F:\\Users\\AnandJayaraman\\Documents\\Research\\Talks n Presentations\\Insofe\\Prob And Stats\\2016-10-22\\Day5")

# Regression

concertData <- read.csv("sunshineDat.csv", header = T, sep = ",")
concertData
concertLinModel <- lm(Count~Hours,data=concertData)

#Plot the data
plot(concertData$Hours,concertData$Count)

abline(concertLinModel)      #plot the Best fit line

#Compute Correlation Coffecient
cor(concertData$Hours,concertData$Count)


summary(concertLinModel)
