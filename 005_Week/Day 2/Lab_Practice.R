data = read.table(file="/Users/suresh/Desktop/Insofe/005_Week/Day 2/bank.txt",header=T,sep=";")
summary(data)
str(data)
unique(data$poutcome)



##Recode the levels of ???y???
data$outcome<-ifelse(data$y=="yes",1,0)
# Converting the ???outcome??? column into factor
data$outcome <- as.factor(as.character(data$outcome))


which(colnames(data)=='y')
data = data[,-which(colnames(data)=='y')]


# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(data$outcome, SplitRatio = 0.70)

train = subset(data,split==TRUE)
test = subset(data,split==F)

dim(train[train$outcome==1,])[1]/dim(train[train$outcome==0,])[1]
dim(test[test$outcome==1,])[1]/dim(test[test$outcome==0,])[1]


LogReg <- glm(outcome ~ ., data=train, family=binomial)
# Predicting on the train data
prob<-predict(LogReg, type="response")
# Considering the threshold as 0.5
pred_class <- ifelse(prob > 0.5, 1, 0)
table(train$outcome,pred_class)


prob_test = predict(LogReg,newdata = test,type="response")
summary(LogReg)
pred_class_test <- ifelse(prob_test > 0.5, 1, 0)
table(test$outcome,pred_class_test)


ConfMat1 = table(train$outcome,pred_class)
# Accuracy
accuracy1 = sum(diag(ConfMat1))/sum(ConfMat1)
# Precision
precision1 = ConfMat1[2,2]/sum(ConfMat1[,2])
# Recall
recall1 = ConfMat1[2,2]/sum(ConfMat1[2,])
# Predicting on test data
# Generating the confusion metric on test data
ConfMat2 = table(test$outcome,pred_class_test)
# Calculating the accuracy of the model on test data
accuracy2 = sum(diag(ConfMat2))/sum(ConfMat2)
# Calculating the precision of the model
precision2 = ConfMat2[2,2]/sum(ConfMat2[,2])
# Calculating the recall of the model
recall2 = ConfMat2[2,2]/sum(ConfMat2[2,])

c(accuracy1,precision1,recall1)
c(accuracy2,precision2,recall2)

library(MASS)

stepOut<- stepAIC(LogReg, direction = "both")

LogReg2 = glm(outcome ~ education + housing + loan + contact + month + duration + campaign + poutcome,data=train,family='binomial')
summary(LogReg2)
summary(LogReg)
# Predicting on the train data
prob_afteraic<-predict(LogReg2, type="response")

# Considering the threshold as 0.5
pred_class_afteraic <- ifelse(prob_afteraic > 0.09908983, 1, 0)
table(train$outcome,pred_class_afteraic)


prob_test_afteraic = predict(LogReg2,newdata = test,type="response")
pred_class_test_afteraic <- ifelse(prob_test_afteraic > 0.09908983, 1, 0)
table(test$outcome,pred_class_test_afteraic)


ConfMat1_afteraic = table(train$outcome,pred_class_afteraic)
# Accuracy
accuracy1_afteraic = sum(diag(ConfMat1_afteraic))/sum(ConfMat1_afteraic)
# Precision
precision1_afteraic = ConfMat1_afteraic[2,2]/sum(ConfMat1_afteraic[,2])
# Recall
recall1_afteraic = ConfMat1_afteraic[2,2]/sum(ConfMat1_afteraic[2,])
# Predicting on test data
# Generating the confusion metric on test data
ConfMat2_afteraic = table(test$outcome,pred_class_test_afteraic)
# Calculating the accuracy of the model on test data
accuracy2_afteraic = sum(diag(ConfMat2_afteraic))/sum(ConfMat2_afteraic)
# Calculating the precision of the model
precision2_afteraic = ConfMat2_afteraic[2,2]/sum(ConfMat2_afteraic[,2])
# Calculating the recall of the model
recall2_afteraic = ConfMat2_afteraic[2,2]/sum(ConfMat2_afteraic[2,])

c(accuracy1,precision1,recall1)
c(accuracy2,precision2,recall2)

c(accuracy1_afteraic,precision1_afteraic,recall1_afteraic)
c(accuracy2_afteraic,precision2_afteraic,recall2_afteraic)


##ROCR curves
##Loading the required libraries
library(ROCR)
library(ggplot2)
# Predicting on the train data
predicted <- predict(LogReg2,type="response")
prob <- prediction(predicted, train$outcome)
str(prob)

# Getting the true positive rate and false negative rate
tprfpr <- performance(prob, "tpr", "fpr")
# Plotting the true positive rate and false negative rate based on the threshold value
plot(tprfpr)
str(tprfpr)

# For different threshold values identifying the tpr and fpr
cutoffs <- data.frame(cut=tprfpr@alpha.values[[1]], fpr=tprfpr@x.values[[1]],
                      tpr=tprfpr@y.values[[1]])
# Sorting the data frame in the decreasing order based on tpr
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.2))
# Plotting the true positive rate and false negative rate based based on the cutoff
# increasing from 0.1-1
plot(tprfpr, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

tpr <- unlist(slot(tprfpr, "y.values"))
fpr <- unlist(slot(tprfpr, "x.values"))
# creating the data frame with tpr and fpr
roc <- data.frame(tpr, fpr)
# Plotting the graph

ggplot(roc) + geom_line(aes(x = fpr, y = tpr)) +
  geom_abline(intercept=0,slope=1,colour="gray") +
  ylab("Sensitivity") + xlab("1 - Specificity")
