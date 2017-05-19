#

dirpath <-paste0(INSOFE_DIR,"RegressionTimeSeries\\2017-04-22\\Day2\\")
setwd(dirpath)

wordRecall <- read.csv("wordRecall.csv")

plot(wordRecall)

#Plot the transformed variables
plot(log(wordRecall$time),wordRecall$prop)

#Fit a linear model

lmRecall <- lm(prop~log(time),wordRecall)
summary(lmRecall)

