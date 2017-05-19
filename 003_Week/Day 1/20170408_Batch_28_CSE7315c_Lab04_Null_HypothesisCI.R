49+qnorm(0.05)*(4.49/sqrt(100))
49+qnorm(0.95)*(4.49/sqrt(100))

2.364+qnorm(0.05)*(.81/sqrt(35))
2.364+qnorm(0.95)*(0.81/sqrt(35))


(22-25)/(1.5/sqrt(40))
qnorm(0.05)
pnorm(-12.64911)
6.35*10^-37

pbinom(73,size=100,prob = 0.8)
pnorm(73.5,mean = 80,sd=4)


qnorm(pnorm(28999.5,mean = 29321,sd=2120/sqrt(100)))
pnorm(-1.51)


9800/49
15/sqrt(49)
-5/(15/sqrt(49))

qnorm(pnorm(200,mean=205,sd = 15/sqrt(49)))
qnorm(0.3694413)


(1/2)*50
50*0.1875
1-pnorm(qnorm(pnorm(19.5,25,sqrt(12.5))))
pnorm(19.5,12.5,sqrt(9.375))


arrivals = c(68,42,51,57,56,80,45,39,36,79)
mean(arrivals)
sd(arrivals)
mean(arrivals) + (qt(0.975,9)*(sd(arrivals)/sqrt(10)) )
mean(arrivals) - (qt(0.975,9)*(sd(arrivals)/sqrt(10)) )

qt(1-pnorm(70.5,68,4/sqrt(25)),9)
qt(0.95,24)

t.test(x=c(45,38,52,48,25,39,51,46,55,46),y= c(34,22,15,27,37,41,24,19,26,36),alternative = "two.sided")
