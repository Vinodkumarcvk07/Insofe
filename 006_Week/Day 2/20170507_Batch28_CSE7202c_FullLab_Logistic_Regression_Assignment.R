rm(list = ls())

setwd("F:\\TA\\Labs\\Batch 28\\CSE 7202c\\FullDayLab\\Classification\\Horse")
# Write your code to read the dataset here
# /* 
horse_data = read.table("/Users/suresh/Desktop/Insofe/006_Week/Day 2/horse-colic_data.txt",header=F,na.strings = "?")
# */

head(horse_data)

table(horse_data$V25)
table(horse_data$V26)
table(horse_data$V27)
# Check for NAs
# /* 
sum(is.na(horse_data))
dim(horse_data)
str(horse_data)
horse_data = horse_data[,-3]

# */

# Function to compute percentage of NAs in a vector. 'x' is a vector
# /* 
  rem_na = function(x){
    (sum(is.na(x))/length(x))*100
  }
# */

# Remove uneccesary columns
head(horse_data)
dim(horse_data)
horse_data = horse_data[,-27]
target = horse_data$V24
table(horse_data$V28)
str(horse_data)

horse_data[,c("V1","V2","V7","V8","V9","V10","V11","V12","V13","V14","V15","V17","V18","V21","V23")] = apply(horse_data[,c("V1","V2","V7","V8","V9","V10","V11","V12","V13","V14","V15","V17","V18","V21","V23")],2,FUN=function(x){as.factor(x)})



# Relove columns with NAs above a certain thresold using
# /* 
  horse_data_col_rem = horse_data[,!apply(horse_data,2,rem_na)>30]
  
  
# */

# Relove rows with NAs above a certain thresold
# /* 
  horse_data_row_rem = horse_data_col_rem[apply(horse_data_col_rem,1,rem_na)<30,]
# */

# Impute the missing values
sum(is.na(horse_data_row_rem))
dim(horse_data_row_rem)
# /* 
str(horse_data_row_rem)
mode = function(x){
  df = data.frame(table(x))
  mode = df[max(df[,2])==df[,2],1]
  mode
}

replace_nawithmode = function(x){
  x[is.na(x)] = mode(x)[1]
  return(x)
}
  

horse_data_row_rem[,!sapply(horse_data_row_rem,is.character)]
horse_data_row_rem[,sapply(horse_data_row_rem,is.character)] = sapply(horse_data_row_rem[,sapply(horse_data_row_rem,is.character)],FUN=replace_nawithmode)
horse_data_row_rem[,sapply(horse_data_row_rem,is.character)] = data.frame(sapply(horse_data_row_rem[,sapply(horse_data_row_rem,is.character)],as.factor))

str(horse_data_row_rem)

horse_data_row_rem[,apply(horse_data_row_rem,2,rem_na)>7]
horse_data_row_rem[is.na(horse_data_row_rem$V4),"V4"] = mean(horse_data_row_rem[!is.na(horse_data_row_rem$V4),"V4"])
horse_data_row_rem[is.na(horse_data_row_rem$V6),"V6"] = mean(horse_data_row_rem[!is.na(horse_data_row_rem$V6),"V6"])
clean_data = knnImputation(horse_data_row_rem,5)
clean_data = centralImputation(horse_data_row_rem)
# */

#  Convert the target column values to 1's & 0's
# /* 
  str(clean_data)
  clean_data$V24 = ifelse(clean_data$V24 == "2" , "0","1")
  clean_data$V24 = as.numeric(clean_data$V24)
# */

# Slipt the data into train & validation using 'caret' library
# /* 
  library(caret)
  intrain = createDataPartition(clean_data$V24,p=0.7,list=FALSE)
  train = clean_data[intrain, ]
  validation = clean_data[-intrain, ]
# */

# Train the logistic regression model 
fit = glm(V24~., data = train,family=binomial)

# Confusion matrix on train predictions
thresold = 0.5
pred_train = predict(fit, newdata = train, type = 'response')
pred_train_rec = ifelse(pred_train<thresold, 0, 1)
table(train$V24, pred_train_rec)

dim(validation)
dim(train)


fit$xlevels$V10 = union(fit$xlevels$V10,levels(validation$V10))
pred_test = predict(fit,newdata=validation,type='response')
pred_test_rec = ifelse(pred_test<thresold,0,1)
table(validation$V24, pred_test_rec)

# ROCR curve for threshold 
library(ROCR)
library(ggplot2)
# /*

prob = prediction(pred_train,train$V24)
tprfpr <- performance(prob, "tpr", "fpr")

par(mfrow=c(1,1))
plot(tprfpr)
str(tprfpr)
cutoffs = data.frame(cut=tprfpr@alpha.values[[1]],tpr = tprfpr@x.values[[1]],fpr = tprfpr@y.values[[1]])
cutoffs[order(cutoffs$tpr,decreasing = T),]
head(subset(cutoffs, fpr < 0.2))


plot(tprfpr,colorize=T,print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))

# */

thresold = 0.2
pred_train = predict(fit, newdata = train, type = 'response')
pred_train_rec = ifelse(pred_train<thresold, 0, 1)
table(train$V24, pred_train_rec)

pred_validation = predict(fit, newdata = validation, type = 'response')
pred_validation_rec = ifelse(pred_validation<thresold, 0, 1)
table(validation$V24, pred_validation_rec)

stepout = stepAIC(fit,direction = "both")
summary(stepout)
summary(fit)


thresold = 0.5
stepaic_train = predict(stepout,newdata = train,type = 'response')
pred_train_rec = ifelse(stepaic_train<thresold,0,1)
table(train$V24, pred_train_rec)


prob = prediction(stepaic_train,train$V24)
tprfpr = performance(prob,"tpr","fpr")
cutoffs = data.frame(cut = tprfpr@alpha.values[[1]],tpr=tprfpr@x.values[[1]],fpr=tprfpr@y.values[[1]])

plot(tprfpr,colorize=T,print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))

ConfMat1 = table(train$V24, pred_train_rec)
# Accuracy
accuracy1 = sum(diag(ConfMat1))/sum(ConfMat1)
# Precision
precision1 = ConfMat1[2,2]/sum(ConfMat1[,2])
# Recall
recall1 = ConfMat1[2,2]/sum(ConfMat1[2,])

c(accuracy1,precision1,recall1)

thresold = 0.2
stepaic_train = predict(stepout,newdata = train,type = 'response')
pred_train_rec = ifelse(stepaic_train<thresold,0,1)
table(train$V24, pred_train_rec)

stepaic_test = predict(stepout,newdata = validation,type = 'response')
pred_test_rec = ifelse(stepaic_test<0.5,0,1)
table(validation$V24, pred_test_rec)
