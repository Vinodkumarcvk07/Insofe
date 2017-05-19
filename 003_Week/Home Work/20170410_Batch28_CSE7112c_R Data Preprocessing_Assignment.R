#1
library(XLConnect)
xls = loadWorkbook("/Users/suresh/Desktop/Insofe/003_Week/Home Work/Airfares.xls")
getSheets(xls)
data = readWorksheet(xls,"data",header = T)
data

#2
dim(data)
str(data)
View(data)

data[,c('VACATION','SW','SLOT','GATE')] <- data.frame(apply(data[,c('VACATION','SW','SLOT','GATE')],2,function(x){as.factor(x)}))

#3
library(dplyr)
dim(filter(data,S_CODE == '*'|S_CITY == '*'|E_CODE =='*'|E_CITY =='*'|COUPON =='*'|NEW =='*'
           |VACATION == '*'|SW =='*'|HI =='*'|S_INCOME=='*'|E_INCOME =='*'
           |S_POP =='*'|E_POP=='*'|SLOT=='*'|GATE=='*'|DISTANCE=='*'
           |PAX=='*'|FARE=='*'))
head(filter(data,S_CODE == '*'|S_CITY == '*'|E_CODE =='*'|E_CITY =='*'|COUPON =='*'|NEW =='*'
       |VACATION == '*'|SW =='*'|HI =='*'|S_INCOME=='*'|E_INCOME =='*'
       |S_POP =='*'|E_POP=='*'|SLOT=='*'|GATE=='*'|DISTANCE=='*'
       |PAX=='*'|FARE=='*'),100)
filter(data,S_CODE != '*'&S_CITY != '*'&E_CODE !='*'&E_CITY !='*'&COUPON !='*'&NEW !='*'
           &VACATION != '*'&SW !='*'&HI !='*'&S_INCOME!='*'&E_INCOME !='*'
           &S_POP !='*'&E_POP!='*'&SLOT!='*'&GATE!='*'&DISTANCE!='*'
           &PAX!='*'&FARE!='*')

dim(data[data$S_CODE == '*'|data$S_CITY == '*',])

data[as.na(data=='*'),]

data_mv_imputed<-centralImputation(data) 
library(DMwR)
sum(data_mv_imputed=='*')
sum(is.na(data))
sum(data=='*')

summarise(group_by(filter(data,VACATION!='*'&is.na(VACATION)==F),VACATION),mean(FARE))

select(filter(data,S_CITY!="*"),mean(FARE))
select(data,strsplit(as.character(data)))
strsplit(data$S_CITY,'/')
strsplit(as.character(S_CITY),"/"))
head(data)

strsplit("Dallas/Fort Worth","/")
data[,strsplit(as.character(data$S_CITY),"/")]
apply(data[,c('S_CITY','E_CITY')],2,function(x){strsplit(as.character(x))})


summarise(group_by(filter(data,SLOT!='*'&is.na(SLOT)==F),SLOT),mean(FARE))
summarise(group_by(filter(data,GATE!='*'&is.na(GATE)==F),GATE),mean(FARE))

start_to_dest = summarise(group_by(data,S_CITY,E_CITY),mean(FARE),n())
View(start_to_dest)

boxplot(data$FARE,data$PAX,data$S_INCOME,data$E_INCOME,names= c("FARE","PAX","S_INCOME","E_INCOME"))
str(data)

hist(data$S_INCOME,breaks = 10)
hist(data$E_INCOME,breaks = 10)
hist(data$COUPON,breaks = 10)





