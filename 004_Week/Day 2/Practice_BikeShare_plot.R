library(readr)
library(ggplot2)
library(lubridate)
library(scales)
library(plyr)


bike_plot = read.csv("/Users/suresh/Desktop/Insofe/004_Week/Day 2/BikeShare.csv",header = T)



bike_plot$season  <- factor(bike_plot$season, labels = c("Spring", "Summer", "Fall", "Winter"))
bike_plot$weather <- factor(bike_plot$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
bike_plot$hour    <- factor(hour(ymd_hms(bike_plot$datetime)))
bike_plot$times   <- as.POSIXct(strftime(ymd_hms(bike_plot$datetime), format="%H:%M:%S"), format="%H:%M:%S")
bike_plot$Weekday <- wday(ymd_hms(bike_plot$datetime), label=TRUE)


season_summary <- ddply(bike_plot,.(season,hour),summarise, count = mean(count))
dim(season_summary)
dim(bike_plot)

ggplot(bike_plot, aes(x = hour, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more in Fall, and much less in Spring.\n") + 
  theme(plot.title=element_text(size=18))


weather_summary <- ddply(bike_plot,.(weather,hour),
                         summarise, count = mean(count))
ggplot(bike_plot, aes(x = hour, y = count, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more when the weather is Good.\n") + 
  theme(plot.title=element_text(size=18))

day_summary <- ddply(train,.(Weekday,hour),
                     summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = Weekday)) +
  geom_point(data = day_summary, aes(group=Weekday)) +
  geom_line(data = day_summary, aes(group=Weekday)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes for morning/evening commutes on weekdays,
          and daytime rides on weekends\n")



weather_prob <- ddply(train,.(season, hour),
                      summarise, Good = mean(weather == "Good"),
                      Normal = mean(weather == "Normal"),
                      Bad = mean(weather == "Bad"),
                      Very_bad = mean(weather == "Very Bad"))


ggplot(train, aes(x = hour, y = Good, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Good") +
  theme_minimal() +
  ggtitle("The probability of Good weather is higher in all. \n") + 
  theme(plot.title=element_text(size=18))


ggplot(train, aes(x = hour, y = Normal, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Normal") +
  theme_minimal() +
  ggtitle("The probability of Normal weather is higher in Spring. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Bad, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Bad") +
  theme_minimal() +
  ggtitle("The probability of Bad weather is higher in Summer and Winter. \n") + 
  theme(plot.title=element_text(size=18))
