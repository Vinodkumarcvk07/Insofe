

#1

pnorm(100,mean=90,sd=15) - pnorm(80,mean=90,sd=15)
gen_ran = rnorm(1000,mean=90,sd=15)
dnorm(gen_ran,mean=90,sd=15)  
dnorm(50,mean=90,sd=15)
qnorm(.50,mean=90,sd=15)
quantile(gen_ran)


#2

1-pnorm(59.5,mean=30,sd=20)
29.5/20
1-pnorm(1.475)


pnorm(40.5,mean=30,sd=20)
10.5/20
pnorm(0.525)


#3

lambda = 3
exp(-3)*(27/8)
exp(-3)*(16.375)

dpois(4,3)
ppois(4,3)


#4

1-pbinom(10,size=15,prob=1/4)

#5

1-.28
(.72^7)*(.28)
1-(.72^5)

dgeom(7,.28)
pgeom(4,.28)

#6

lambda = 3

pexp(2,rate=1/3)

#7

sm = .84
pm = .90
sqrt((pm*.1)/144)
qnorm(pnorm(sm,mean = .9,sd = .025))
qnorm(0.008197536)
qnorm(0.0025)
qnorm(0.975)

pnorm(1.96)



mean=90; sd=15
dnorm(50,90,15)
# Generating sample of random numbers with given mean and SD
x <- round(sort(rnorm(1000,mean,sd)),2)
lb=min(x); ub=60
# dnorm gives the probability values for each x
y <- dnorm(sort(x),mean,sd)
#Plotting distribution curve
plot(x, y, type="l", xlab="Work Hours", ylab="",main="Normal Distribution", axes=FALSE)
i <- x >= lb & x <= ub
lines(x, y)
polygon(c(lb,x[i],ub), c(0,y[i],0), col="red")
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd) #pnorm gives the cumulative probability upto x
mtext(paste("P(",lb,"< Work Hours <",ub,") =", signif(area, digits=3)),3)
axis(1, at=seq(min(x), max(x), sd), pos=0)
