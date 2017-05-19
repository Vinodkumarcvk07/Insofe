#rm(list=ls())

#Question 4

q1 <- matrix(c(20,21,22,20,21,30,15,12,36,28), nrow = 2, ncol = 5, byrow = T,
                          dimnames = list(c("Philip","Mathews"),c("Mon","Tue","Wed","Thu","Fri")));q1
mean_Philip <- mean(q1[1,],na.rm = T)
median_Philip <- median(q1[1,],na.rm = T)
range_Philip <- range(q1[1,])[2] - range(q1[1,])[1]; range_Philip

range <- max(q1[1,])-min(q1[1,])

mean_Mathews <- mean(q1[2,],na.rm = T)
median_Mathews <- median(q1[2,],na.rm = T)
range_Mathews <- range(q1[2,])[2] - range(q1[2,])[1]

stdev_Philip <- sd(q1[1,],na.rm = T); stdev_Philip
var_Philip <- var(q1[1,],na.rm = T); var_Philip
mad_Philip <- mad(q1[1,],na.rm = T); mad_Philip

stdev_Mathews <- sd(q1[2,],na.rm = T); stdev_Mathews
var_Mathews <- var(q1[2,],na.rm = T); var_Mathews
mad_Mathews <- mad(q1[2,],na.rm = T); mad_Mathews

#c(stdev_Philip, stdev_Mathews)

# Question 5

numVec <- c(8,6,2,4,6,8,10,8)
which.max(table(numVec))

#Question 6

scores <- c(11,7.5,8.5,10,10,10.5,5.5,10,9,9.5,5.25,8,
            6.5,10.5,8.75,0,6,6,6.75,7.5,8.5,7)

scores_range <- range(scores)[2] - range(scores)[1]
scores_stdev <- sd(scores)
scores_var <- var(scores)
#hist(scores)
mean(scores)
median(scores)

#Detection of outliers
dev.off()
bplot <- boxplot(scores,col="blue", ylab="values", xlab="scores")
abline(h=mean(scores), col="orange")
points(mean(scores))
bplot$

iqr <- IQR(x = scores, na.rm=T)
quartiles <- quantile(x = scores, probs = c(0.25,0.5,0.75,1.0))
quartiles[[1]] - 1.5*iqr
quartiles[[3]] + 1.5*iqr

#Question 7

city_temp <- matrix(c(29,32,36,40,43,37,36,33,32,37,31,29,
                      20,24,31,37,40,38,37,34,34,33,28,23,
                      23,26,32,38,41,40,35,33,35,37,30,25,
                      20,24,29,34,37,36,32,30,33,32,27,23,
                      19,24,29,38,43,38,33,34,36,34,29,23), nrow = 5, ncol = 12, byrow = T,
                    dimnames = list(c("City1","City2","City3","City4","City5"),
                                    c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10",
                                      "D11","D12")))

df <- data.frame("Mean" = apply(city_temp, 1, mean), "Median" = apply(city_temp, 1, median),
           "Stdev" = apply(city_temp, 1, sd))
df

#Question 8

supplier <- matrix(c(17,22,22,22,27,17,19,20,27,27), nrow = 2, ncol = 5, byrow = T,
                   dimnames = list(c("Supplier1","Supplier2")))

range_sup1 <- range(supplier[1,])[2] - range(supplier[1,])[1]
range_sup2 <- range(supplier[2,])[2] - range(supplier[2,])[1]
mean_sup1 <- mean(supplier[1,], na.rm = T)
mean_sup2 <- mean(supplier[2,], na.rm = T)
sd_sup1 <- sd(supplier[1,], na.rm = T)
sd_sup2 <- sd(supplier[2,], na.rm = T)
data.frame("Range" = c(range_sup1, range_sup2), "Mean" = c(mean_sup1,mean_sup2),
           "Stdev" = c(sd_sup1,sd_sup2))

#Question 10

newMat <- matrix(c(19,41,60,12,28,40,31,69, 100), nrow = 3, ncol = 3, byrow = T, 
                 dimnames = list(c("Male", "Female", "Total"), c("Graduate","Post Graduate", "Total")))

#Joint Probability of being male and graduate
newMat[1,1]/newMat[3,3]
#Randomly selected individual is a male - marginal probability
newMat[1,3]/newMat[3,3]
#Randomly selected individual is a graduate - marginal probability
newMat[3,1]/newMat[3,3]
#Randomly selected person is a female given that selected person is post graduate
newMat[2,2]/newMat[3,2]

#Question 11

newMat <- matrix(c(115,206,345,334), nrow = 2, ncol = 2, byrow = T, 
                 dimnames = list(c("People died of RF", "People ! dies of RF"), 
                                 c("Parent had RF", "Parent ! had RF")))

#Person died of RF if neither of the parents had RF
newMat[1,2]/sum(newMat[,2])

#QUestion 

#Prob. of test giving positive result = Prob. (given disease, positive result) * Prob. (disease) +
#                                       Prob. (given ~disease, positive result) * Prob. (~disease)

(0.98)*(0.005) + (0.03)*(0.995)

#Prob.(disease given positive) - bayes' theorem

(0.98)*(0.005)/((0.98)*(0.005) + (0.03)*(0.995))

#Exercise - Problem 2

newMat <- matrix(c(8000,900,100,1000), nrow = 2, ncol = 2, byrow = T, 
                 dimnames = list(c("Truly credit worthy", "Truly not credit worthy"), 
                                 c("Predicted credit worthy", "Predicted not credit worthy")))

Accuracy <- sum(diag(newMat))/sum(newMat)
Accuracy
Precision <- newMat[1,1]/sum(newMat[,1])
Precision
Recall <- newMat[1,1]/sum(newMat[1,])
Recall
F1 <- (2*Precision*Recall)/(Precision + Recall)
F1

#Monte Hall Simulation
doors <- c("A","B","C")
xdata <- c()
for (i in 1:1000){
  prize <- sample(doors)[1]
  pick <- sample(doors)[1]
  open_door <- sample(doors[which(doors!=prize & doors!=pick)])[1]
  switchyes <- doors[which(doors!=open_door & doors!=pick)]
  if(pick==prize){xdata <- c(xdata,"noswitchwin")}
  if(switchyes==prize){xdata <- c(xdata,"switchwin")}
}
noswitch <- length(which(xdata=="noswitchwin")); noswitch
switch <- length(which(xdata=="switchwin")); switch
prob_switch <- switch/(switch + noswitch);prob_switch


set.seed(12)
x <- runif(1000,0,1)
randomSample <- sample(x,10)
randomSample

