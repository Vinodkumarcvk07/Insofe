##Introduction to R
x <- 5
x
print(x)

x<- 1:20
x
X # 'object X not found' <- R is case sensitive.
x[5]
x[10:15]
#Note, that every object created is stored as a col vector unless specified
# [1] in the output indicates index of the value stored in 'x'
# Try x[2], x[1:15] to understand the indexing types 'Numeric' and 'range'

#Creating a vector
# 'c' is the concatenate function to combine values
x <- c(0.5, 0.6) ## numeric
x <- c(TRUE, FALSE) ## logical
x <- c(T, F) ## logical
x <- c("a", "b", "c") ## character
x <- 9:29 ## integer
x <- c(1+0i, 2+4i) ## complex
x
#Creating a blank vector
x <- vector("numeric", length = 10)
x
#Matrix
# tabular strucuture, holds values of same datatype
m <- matrix(nrow = 2, ncol = 3)
m
dim(m)

m <- matrix(1:6, nrow = 2, ncol = 3)
m

#Cbind and Rbind
x <- 1:3
y <- 10:12

z <- cbind(x,y)
z
V <- rbind(x,y)
V

## Dataframes - tabular structure
## cols can hold different data types but of same dimensions
x <- data.frame(Cid = 1:4, Purchase = c(T, T, F, F))
x
nrow(x)
ncol(x)
dim(x)
#find data subsets
# from matrix
m
m[1,2]
m[2,3]
m[,2]
c(m[1,2],m[2,1])
# from dataframe
x
x[1,2]
x[1,]
x[,2]
x[x>2]
x[x>2,]

## creating Lists; can hold objects of diffent type and of different dimensions
L <- list(x,m, Cid = 1:4, Purchase = 0.6, Product = "PP1")
L
L[c(1,2)] # retrieve 1st and 3rd object
L$Cid
L$Purchase
L[[1]]
L[[3]]

## Vectorized operations
p <- 1:4; q <- 6:9
p
p + q
p > 2
p * q

## Reading data
# read inbuilt R datasets
d1 <- mtcars
dim(d1)
names(d1)
str(d1)
summary(d1)

#find more datasets
library(datasets)
data()

# reading data from folders
setwd("C:\\Users\\Classroom 4\\Desktop\\20170326")
dir()

GradesDate <- read.csv("Grade.csv", header=T)
class(GradesDate)
names(GradesDate)
str(GradesDate)
summary(GradesDate)
head(GradesDate)
tail(GradesDate)

## '$' used for reference

head(GradesDate$Student.id)
GradesDate[GradesDate$Student.id>5,]

#write the file
write.csv(GradesDate, "GradesDate.csv")

## what's in the environment?
ls()
# freeup the environment
rm(list=ls())
rm(x)


