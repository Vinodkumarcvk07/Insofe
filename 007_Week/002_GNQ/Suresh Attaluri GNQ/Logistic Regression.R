#a
load("/Users/suresh/Desktop/Insofe/007_Week/002_GNQ/20170513_Batch 28_CSE 7202c_GNQ_Data/LogReg.RData")
head(data)

#b

summary(LogReg)

predict_data = predict(LogReg,type="response",newdata= data)
prob_class = ifelse(predict_data > 0.5 ,1,0)
data_mat = table(data$V12,prob_class)


accuracy = sum(diag(data_mat))/sum(data_mat)
precision = data_mat[2,2]/sum(data_mat[,2])
recall = data_mat[2,2]/sum(data_mat[2,]) 

c(accuracy,precision,recall)

#c

library(ROCR)
library(ggplot2)

prob = prediction(predict_data,data$V12)
tprfpr <- performance(prob, "tpr", "fpr")
par(mfrow=c(1,1))
plot(tprfpr)
summary(tprfpr)
plot(tprfpr,colorize=T,print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))

cutoffs = data.frame(cut=tprfpr@alpha.values[[1]],fpr = tprfpr@x.values[[1]],tpr = tprfpr@y.values[[1]])


head(cutoffs[order(cutoffs$tpr,decreasing = T),])
head(subset(cutoffs[order(cutoffs$tpr,decreasing = T),], fpr <0.6))

#I choose the cutoff value to be 0.1 the reason for me to choose this value is that 
#to have more true positive rate even after sacrificing the false poistive rate as
#this will impact the business less. Predictive those customers who will not 
#turned out as the customers who will turn up will make the business to have more 
#stocks this loss is comapritively less when there is risk of lossing customers 
#when there are more number of customers but less stock


#d

#e

predict_data = predict(LogReg,type="response",newdata= data)
prob_class = ifelse(predict_data > 0.1 ,1,0)
data_mat = table(data$V12,prob_class)


accuracy = sum(diag(data_mat))/sum(data_mat)
precision = data_mat[2,2]/sum(data_mat[,2])
recall = data_mat[2,2]/sum(data_mat[2,]) 

c(accuracy,precision,recall)

#recall is the parameter I will be looking at as it indicates that 
#the true positive rate increases

