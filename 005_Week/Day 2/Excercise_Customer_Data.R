customer = read.csv("/Users/suresh/Desktop/Insofe/005_Week/Day 2/CustomerData_Classification.csv",header=T)
head(customer)
summary(customer)
str(customer)

customer = customer[,-which(colnames(customer)=="CustomerID")]
sum(is.na(customer))

customer[,c("City","Churned")] = data.frame(apply(customer[,c("City","Churned")],2,FUN=function(x){as.factor(as.character(x))}))

library(vegan)
library(dplyr)
head(customer[,sapply(customer,is.numeric)])
#customer[,sapply(customer,is.numeric)] = decostand(customer[,sapply(customer,is.numeric)],'standardize')

unique(customer[,'Churned'])
unique(customer[customer["MinAgeOfChild"]==113,"MinAgeOfChild"])
customer[customer["MinAgeOfChild"]==113,]

library(caTools)
set.seed(10234)
trainid = sample.split(customer$Churned,SplitRatio = 0.7)
train = customer[trainid,]
test = customer[-trainid,]

logreg <- glm(Churned ~ ., data=train, family=binomial)
summary(logreg)

prob = predict(logreg,type="response")
prob_class = ifelse(prob > 0.5 ,1,0)
train_mat = table(train$Churned,prob_class)

prob_test = predict(logreg,newdata = test,type="response")
prob_class_test = ifelse(prob_test > 0.5 ,1,0)
test_mat = table(test$Churned,prob_class_test)


train_acc = sum(diag(train_mat))/sum(train_mat)
train_preci = train_mat[2,2]/sum(train_mat[,2])
train_recall = train_mat[2,2]/sum(train_mat[2,]) 


test_acc = sum(diag(test_mat))/sum(test_mat)
test_preci = test_mat[2,2]/sum(test_mat[,2])
test_recall = test_mat[2,2]/sum(test_mat[2,]) 

c(train_acc,train_preci,train_recall)
c(test_acc,test_preci,test_recall)

library(MASS)
library(car)
vif(stepout)
stepout = stepAIC(logreg,direction= "both")
summary(stepout)
summary(logreg)

logreg2 = glm(Churned ~ ., data=train, family=binomial)
