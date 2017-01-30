##Data analysis for life science:  Introduction to Linear Models and Matrix Algebra
##Codes for excersizes
##Programm by Md Moinuddin
##February 2016
##------------------------------------------------

setwd("F:/Moin2/moin/Academic/Online courses/edX_Data analysis for life science/Data")
getwd()

##-----------------------------------------------

## Week 1
# Introduction
#Ex 1
library(UsingR)
data("father.son",package="UsingR")

mean(father.son$sheight)

#Ex 2
library(dplyr)

fsh_filter <- filter(father.son, round(fheight, digits = 0)=="71")
head(fsh_filter)
mean(fsh_filter$sheight)

#Intro to mat algebra
#Session 1
#Ex 1
X <- matrix(1:1000,100,10)
X
dim(X)
X[25, 3]

#Ex 2
XX <- c(1:10)
XX
XX2 <- XX*2
XX3 <- XX*3
XX4 <- XX*4
XX5 <- XX*5
XX <-cbind(XX, XX2, XX3, XX4, XX5)
XX
sum <- sum(XX[7,])
sum

#Ex 3
Y <-matrix(1:160,20,3,byrow = TRUE)

#Session 2
#Ex 1
X <- matrix(1:9, 3,3)
print(X)
Y <- X%*%matrix(1,ncol(X))
Y

#Ex 2
X <- matrix(c(3,4,-5,1,2,2,2,-1,1,-1,5,-5,5,0,0,1),4,4,byrow = TRUE)
Y <- matrix(c(10,5,7,4),4,1)
beta <- solve(X)%*%Y
beta
print(X%*%beta) #For checking

#Ex 3
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)

ab <- a%*%b
print(ab)
print(ab[3,2])

#Ex 4

##Week 2: Data analysis with matrix algebra
#Ex 1 & 2
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
print(X)

beta <- c(5, 2)
fitted <- X %*% beta
fitted

#Ex 3 & 4
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
X
beta <- c(10,3,-3)

fitted <- X %*% beta
fitted

##Week 2: Inference
#Session 1
#Ex 1
g <- 9.8 ## meters per second
h0 <- 56.67
v0 <- 0
n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
X <- cbind(1, tt, tt^2)
colnames(X) <- c("1","tt","tt2")
A <- solve(crossprod(X)) %*% t(X)

y <- h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
AY <- A%*%y
glse <- -2*(AY[3])
glse

#Ex 2
g <- 9.8 ## meters per second
h0 <- 56.67
v0 <- 0
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
X <- cbind(1, tt, tt^2)
colnames(X) <- c("1","tt","tt2")
A <- solve(crossprod(X)) %*% t(X)

N <- 100000
set.seed(1)
  grep <- function(n){
    y <- h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
    AY <- A%*%y
    glse <- -2*(AY[3])
  }
  bet <- replicate(N, grep(25))
 sd(bet)
  
 rm(list = ls())
 
#Ex 3
 library(UsingR)
 x <- father.son$fheight
 y <- father.son$sheight
 length(x)
 length(y)
 plot(x,y)
 n <- length(y)

 B <- 10000 
 set.seed(1)
  slope <- function(N){
      index <- sample(n,N)
      sampledat <- father.son[index,]
      x <- sampledat$fheight
      y <- sampledat$sheight
      betahat <- lm(y~x)$coef
      bsl <- betahat[2]
    }
 
slop <- replicate(B, slope(50))
sd(slop)

 #Ex 4
x <- father.son$fheight
y <- father.son$sheight

mean((x-mean(x))*(y-mean(y)))

#Session 2
#Ex 1
rm(list = ls())
Y <- father.son$sheight
n <- length(Y)
N <- 50
set.seed(1)
index <- sample(n,N)
sampledat <- father.son[index,]
x <- sampledat$fheight
y <- sampledat$sheight
fit <- lm(y~x)
yhat <- fit$fitted.values
yhat
r <- y-yhat
RSS <- t(r)%*%r
RSS

#Ex 2
sigma2 <- RSS / 48

X <- cbind(1, x)
mat <- solve(t(X)%*%X)
mat[1,1]

#Ex 3
diag <- diag(mat)
varb <- diag*sigma2
sdb <- sqrt(varb)
sdb

##Week 3: linear models
nx <- 5
ny <- 7

gr <- c(rep(0,nx), rep(1,ny))
gr
X <- model.matrix(~ gr)
X
desmat <- t(X)%*%X
desmat

##Week 4
#Session 1: Interactions and contrasts
#Ex 1
install.packages("contrast")
library(contrast)

species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))

model.matrix(~ species + condition)

#Ex 2
install.packages("downloader")
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)
head(spider)

fit2 <- lm(friction~leg+type, data = spider)
summary(fit2)

L4vsL2 <- contrast(fit2, list(leg = "L4",type = "pull"), list(leg = "L2", type = "pull"))
L4vsL2

L4vsL2p <- contrast(fit2, list(leg = "L4",type = "push"), list(leg = "L2", type = "push"))
L4vsL2p

fit2$coefficients[4]-fit2$coefficients[2]

#Ex 3
X <- model.matrix(~ type + leg, data=spider)
Sigma <- sum(fit2$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X)
Sigma

C <- matrix(c(0, 0, -1, 0,1), 1,5)
varC <- C%*%Sigma%*%t(C)
varC
seC <- sqrt(varC)
seC

##Session 2
#Ex 1




