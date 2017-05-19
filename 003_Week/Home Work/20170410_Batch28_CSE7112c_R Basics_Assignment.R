#Problem 1:
#a
27*(38-17)
#b
14^7
#c
sqrt(436/12)

#Problem 2:
a = seq(from=5,to = 160,by = 5)
a
b = seq(from = 87,to = 56,by = -1)
b

d = a*b
d[19:21]
d[d<2000]
d[d>6000]

#Problem 3

sum(d)
median(d)
sd(d)

#Problem 4
m1 <- matrix(c(7,9,12,2,4,13),nrow = 2,ncol = 3,byrow = T)
m1
m2 <- matrix(c(1,7,12,19,2,8,13,20,3,9,14,21),nrow = 3,ncol = 4,byrow = T)
m2

m1%*%m2

#Problem 5

write.table(longley,"/Users/suresh/Desktop/Insofe/003_Week/Home Work/longley.xls",sep = "\t")
lg = read.table("/Users/suresh/Desktop/Insofe/003_Week/Home Work/longley.xls",header = T,sep="\t")
lg
str(lg)

lg[,"Year"] = as.factor(as.character(lg[,'Year']))
str(lg)

library(dplyr)

summarize(lg,"GNP.Deflator.Mean" = mean(GNP.deflator),"GNP.deflator.Sd"=sd(GNP.deflator)
          ,"GNP.Mean" = mean(GNP),"GNP.sd" = sd(GNP),"Unemployed.Mean"=mean(Unemployed)
          ,"Unemployed.sd"=sd(Unemployed),"Armed.Forces.Mean"=mean(Armed.Forces)
          ,"Armed.Forces.sd"=sd(Armed.Forces),"Population.Mean"=mean(Population)
          ,"Population.sd"=sd(Population),"Employed.Mean"=mean(Employed)
          ,"Employed.sd" = sd(Employed)
          )

#Would you get these means and standard deviations for all the variables?
#we won't get mean and standard deviation for year because it is a categorical variable

#Problem 6
plot(lg$Year,lg$GNP.deflator)

hist(lg$GNP.deflator,xlab = "GNP.Deflator",main = 'Histogram for lg')
hist(lg$GNP,xlab = "GNP",main = 'Histogram for lg')
hist(lg$Unemployed,xlab = "Unemployed",main = 'Histogram for lg')
hist(lg$Armed.Forces,xlab = "Armed.Forces",main = 'Histogram for lg')
hist(lg$Population,xlab = "Population",main = 'Histogram for lg')
hist(lg$Employed,xlab = "Employed",main = 'Histogram for lg')

boxplot(lg$GNP.deflator,lg$GNP,lg$Unemployed,lg$Armed.Forces,lg$Population,lg$Employed
        ,names = c("GNP.Deflator","GNP","Unemployed","Armed.Forces","Population","Employed"))


#Problem 7
#H0 : all years will have common probability of unemployed population

emp = matrix(c(lg[,c("Employed")],lg[,c("Unemployed")]),nrow = 16,ncol = 2)
chisq.test(emp)
qchisq(.99,15)

#We can reject the null hypothesis that the unemployed population over the years will not vary.

plot(Unemployed~Year,lg)
