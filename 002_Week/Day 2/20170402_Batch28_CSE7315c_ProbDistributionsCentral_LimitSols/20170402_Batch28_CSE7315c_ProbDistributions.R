rnorm(20)
rnorm(10, 2,1.5)
dnorm(0,0,1)
dnorm(2, mean = 5, sd = 3)
#graphing the normal distribution using dnorm

# First make a vector of Z-scores
z_scores <- seq(-3, 3, by = .1)
# Let's print the vector
z_scores
# Let's make a vector of the values the function takes given those Z-scores.
# Remember for dnorm the default value for mean is 0 and for sd is 1.
dvalues <- dnorm(z_scores)
# Let's examine those values
dvalues

====================================================================
#plot  
=================================================================
  dev.off()
  plot(dvalues, # Plot where y = values and x = index of the value in the vector
       xaxt = "n", # Don't label the x-axis
       type = "l", # Make it a line plot
       main = "pdf of the Standard Normal",
       xlab= "Z-score")   
# These commands label the x-axis
axis(1, at=which(dvalues == dnorm(0)), labels=c(0))
axis(1, at=which(dvalues == dnorm(1)), labels=c(-1, 1))
axis(1, at=which(dvalues == dnorm(2)), labels=c(-2, 2))

## dnorm will give height of the pdf.

pnorm(0)
1-pnorm(0)
qnorm(0.98)
qnorm(pnorm(0))

============================================================================
#Problem 1

ans <- dbinom(4,10,0.6)

#Problem 2
geom_distrib = dgeom (x = 0:10,prob = 0.15); 
barplot (geom_distrib ,col = 'blue', xlab = 'Number of trials', ylab = 'Probability')

avg <- 1/0.15; avg

#Problem 3

arrivals <- dpois(x =3,lambda=5);arrivals

#Problem 4

birth <- dpois(x=5, lambda = 1.8*2);birth

#Problem 5
# time exceeds 2 hrs  P(x>2) - pexp(q, rate)

prob <- pexp(2, 0.5)
cdf <- 1-prob; cdf

#Problem 6
z <- c(82, 72, 85, 14, 66, 15, 23, 78, 16, 38, 92, 17)
M <- mean(z);M
SD <- sd(z); SD
score <- (z-M)/SD ; score

# Problem 7
ZA <- (.75- .7)/.2; ZA
ZB <-  (.55- .4)/.1; ZB
ZB > ZA

# Problem 8
sd = sqrt(29.34)
prb <- pnorm(72,67.2, sd); prb
ans <- 1-prb; ans

#Problem 9
lAv <- 500
lSd <- 100
N <- 10000

prb1 <- 1-pnorm(500,500,100); prb1
NumBulbs <- prb1*N; NumBulbs

prb2 <- pnorm(500,500,100); prb2
NumBulbs2 <- prb2*N; NumBulbs2

#P(between 350-???550) 
Z = (350-500)/100 = -1.5; 
P (Z=-1.5) = pnorm (-1.5, 0, 1) = 0.066
a <- pnorm (350,500,100)
b <- pnorm (550,500,100)

ans9 <- b-a; ans9
NumBulb3 <- ans9*N; round(NumBulb3)

# Problem 10
dbinom (3, 12, 0.5)

