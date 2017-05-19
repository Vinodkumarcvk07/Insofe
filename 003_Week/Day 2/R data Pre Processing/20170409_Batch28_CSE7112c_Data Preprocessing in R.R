rm(list=ls(all=TRUE))
setwd("")

#Importing & exporting data
data<-read.csv("/Users/suresh/Desktop/Insofe/003_Week/Day 2/R data Pre Processing/Data.csv",header=T)

#Undertanding data structure
str(data)
head(data)
tail(data)
summary(data)
sum(is.na(data))#To check null values in data
names(data)

#Subsetting data
Data_NumAtr<-subset(data,select=c(Age,Experience,Income,CCAvg,Mortgage))
Data_CatAtr<-subset(data,select=-c(Age,Experience,Income,CCAvg,Mortgage))

#Converting categorical attributes as factors
Data_CatAtr$ID<-as.factor(Data_CatAtr$ID) # One by one
#All at once  using apply function
Data_CatAtr<-data.frame(apply(Data_CatAtr,2,function(x){as.factor(x)}))
str(Data_CatAtr)

#Subsetting data by row
Data1<-data[data$Age>50,c("ID","Age")]
Data2<-data[data$Income>70,c("ID","Income")]
length(intersect(Data1$ID,Data2$ID))
dim(data[data$Income>70,c("ID","Income")])
dim(data[data$Income>50,c("ID","Income")])

#Merging two datasets
MergedData<-merge(Data1,Data2,by.x="ID",by.y="ID",all.x=TRUE) #left (outer) join
MergedData<-merge(Data1,Data2,by.x="ID",by.y="ID",all.y=TRUE) #right (outer) join

#Discretizing the variable
library(infotheo)
IncomeBin <- discretize(data$Income, disc="equalfreq",nbins=4)
table(IncomeBin)
#tapply usage
tapply(data$Income,IncomeBin,min)
tapply(data$Income,IncomeBin,max)

IncomeBin <- discretize(data$Income, disc="equalwidth",nbins=4)
table(IncomeBin)
#tapply usage
tapply(data$Income,IncomeBin,min)
tapply(data$Income,IncomeBin,max)

#Manual recoding (Also learning for loop & if else statements)
summary(data$Age)
data$AgeNew<-0
for (i in 1:nrow(data)){
  if (data$Age[i]>=45){ 
    data$AgeNew[i]=2
  }
  else {
    data$AgeNew[i]=1
  }
}
table(data$AgeNew)
tapply(data$Age,data$AgeNew,min)
tapply(data$Age,data$AgeNew,max)

#Creating dummy variables and adding to original table
library(dummies)
EduDummyVars<-dummy(data$Education)
head(EduDummyVars)
Data<-data.frame(data,EduDummyVars)

head(Data,n=1)

#Standardizing the data 
library(vegan)
Data_NumAtr2 <- decostand(Data_NumAtr,"range") # using range method 
Data_NumAtr2 <- decostand(Data_NumAtr,"standardize") # Using Z score method

#Missing values handling
data_mv<-read.csv("/Users/suresh/Desktop/Insofe/003_Week/Day 2/R data Pre Processing/DataWithMissingValues.csv",header=T)
sum(is.na(data_mv))#To check null values in data
summary(data)

#Dropping the recrods with missing values
data_mv2<-na.omit(data_mv)
dim(na.omit(data_mv))
dim(data_mv)

#To identify rows where more than 20% attributes are missing
library(DMwR)
manyNAs(data_mv, 0.2)

sum(is.na(data_mv))
data_mv[c(4,14  ,19  ,22  ,42 ),]
data_mv[c(4   ,9  ,14  ,17  ,19  ,22  ,42 ),]
          ,51  ,57  ,71 ,102 ,111 ,141 ,167 ,177 ,208, 261 ,318 ,391 ,421 ,441 ,548),]

#Imputing missing values
library(DMwR)
data_mv_imputed<-centralImputation(data_mv) #Cenral Imputation
sum(is.na(data_mv_imputed))
data_mv_imputed<-knnImputation(data_mv,k=5) #KNN Imputation
sum(is.na(data_mv_imputed))

#Reading Data from other than CSV
rm(list=ls(all=TRUE))
csv<-read.csv("Data.csv",header=T)
txt<-read.table("txt.txt",sep="\t",header=T)
#install.packages("XLConnect")
library(XLConnect)
xls<-loadWorkbook("xls.xlsx")
getSheets(xls)
sheet1<-readWorksheet(xls,"Data",header=T)



