#

dirpath <-paste0(INSOFE_DIR,"RegressionTimeSeries\\2017-04-22\\Day2\\")
setwd(dirpath)

carstop <- read.csv("SpeedVsStopNADA.csv", header = T, sep = ",")

#Build a linear Model
lmstop <- lm(StopDist.ft~ Speed.mph, data=carstop )

summary(lmstop)

shapiro.test(lmstop$residuals)  # Check if the residuals are Normally distributed

# Try drawing a smoothed line using LOcally Weighted RegrESSion (loess)
# If you have ggplot2 installed, the line below will work
#ggplot(carstop,aes(x=Speed.mph, y=StopDist.ft)) + geom_point()+geom_smooth(method="loess")

#or

with(carstop, scatter.smooth(Speed.mph,StopDist.ft,family="gaussian"))




