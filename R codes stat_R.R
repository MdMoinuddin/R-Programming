##Data analysis for life science: Statistics and R
##Codes for excersizes
##Programm by Md Moinuddin
##October 2015
##------------------------------------------------

setwd("F:/Moin2/moin/Academic/Online courses/edX_Data analysis for life science/Data")
getwd()

##-----------------------------------------------

## Week 1

##Excercise on getting started session 1
install.packages("swirl")
library(swirl)

##Ex 2
vec <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
summary(vec)

##Ex 3
seq <- c(seq(1,25, by=1))
seq_squar <- NULL
for (i in 1:25) {
  seq_squar[i] <- seq[i]^2
}
sum <- sum(seq_squar) 

##Ex 4-8
cars
class(cars)
dim(cars)
head(cars)
summary(cars)

pos<- cars[ which(cars$dist== 85), ]

##Excercise on getting started session 2

list.files()

dat <- read.csv("femaleMiceWeights.csv")
head(dat)

dat[12,2]

vec <- c(dat$Bodyweight[11])
vec

length(dat$Diet)

table(dat$Diet)
select <- which(dat$Diet == "hf")
select

sel_wt <- dat[which(dat$Diet=="hf"),2]
sel_wt
mean(sel_wt)


sam<- dat[13:24,]
set.seed(1)
sample(sam$Bodyweight, size=1, replace = FALSE, prob=NULL)

##Exercise session 3 on dplyr

library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

install.packages("dplyr")
library(dplyr)

list.files()
dat2 <- read.csv("msleep_ggplot2.csv")
head(dat2)
class(dat2)

table(dat2$order)
dat3 <- filter(dat2, order=="Primates")
dat3
nrow(dat3)
class(dat3)

sleep_tot <- select(dat3, sleep_total)
sleep_tot
class(sleep_tot)

mean(sleep_tot$sleep_total)
summarize(sleep_tot$sleep_total)

datnew <- filter(dat2, order=="Primates")%>%select(sleep_total)
datnew
summarize(datnew, mean(sleep_total))


## Week 2

list.files()
dat <- read.csv("femaleControlsPopulation.csv")
view(dat)
dat
mean(dat$Bodyweight)

set.seed(1)
sam <- sample(dat$Bodyweight, size=5, replace = FALSE, prob=NULL)

meandiff<-abs(mean(dat$Bodyweight)-mean(sam))

set.seed(5)
sam5 <- sample(dat$Bodyweight, size=5, replace = FALSE, prob=NULL)

meandiff2<-abs(mean(dat$Bodyweight)-mean(sam5))


##Exercise session 2
library(dplyr)

dat <- read.csv("femaleControlsPopulation.csv")
head(dat)
dat <- unlist(dat)
n <- 1000
mean <- mean(dat)
sam_mean <- vector("numeric",1000)
mean_diff <- vector("numeric",1000)
set.seed(1)

for (i in 1:n) {
  sam_mean[i] <- mean( sample(dat, size=5))
  mean_diff[i] <- abs(sam_mean[i]-mean)
}

p <- mean_diff>1
p
mean(p)

##Ex 2
m <- 10000
mean2 <- mean(dat)
sam_mean2 <- vector("numeric",10000)
mean_diff2 <- vector("numeric",10000)
set.seed(1)

for (i in 1:m) {
  sam_mean2[i] <- mean( sample(dat, size=5))
  mean_diff2[i] <- abs(sam_mean2[i]-mean2)
}

p2 <- mean_diff2>1
mean(p2)

##Ex 3
a <- 1000
mean3 <- mean(dat)
sam_mean3 <- vector("numeric",1000)
mean_diff3 <- vector("numeric",1000)
set.seed(1)

for (i in 1:a) {
  sam_mean3[i] <- mean( sample(dat, size=50))
  mean_diff3[i] <- abs(sam_mean3[i]-mean3)
}

p3 <- mean_diff3>1
mean(p3)

##Session 3
install.packages("gapminder")
library(gapminder)
data("gapminder")
head(gapminder)
dim(gapminder)
table(gapminder$country)

x <- filter(gapminder, year==1952)%>%select(lifeExp)
View(x)

hist(x)
is.numeric(x)
x <-unlist(x)

##Ex1
p40 <- mean(x<=40)
p40

p60 <- mean(x<=60)

p60_40 <- p60-p40
p60_40

#function practice

prop = function(q) {
  mean(x <= q)
}

  qs = seq(from=min(x), to=max(x), length=20)
  
  props = sapply(qs, prop)
  
  plot(qs, props)
  

  props = sapply(qs, function(q) mean(x <= q))


plot(ecdf(x))

##Week 2, session 2.1, Normal distribution, Ex 1 

dat <- read.csv("femaleControlsPopulation.csv")
dat <- unlist(dat)

a <- 1000
sam_mean5 <- vector("numeric",1000)
sam_mean50 <- vector("numeric",1000)
set.seed(1)

for (i in 1:a) {
  sam_mean5[i] <- mean( sample(dat, size=5))
}

set.seed(1)
for (i in 1:a) {
  sam_mean50[i] <- mean( sample(dat, size=50))
}

hist(sam_mean5)
hist(sam_mean50)

set.seed(1)
for (i in 1:a) {
  sam_mean50[i] <- mean( sample(dat, size=50))
}

## Ex 2


if23_25 <- (sam_mean50 >=23) & (sam_mean50 <=25)

mean(if23_25)

## Ex 3
prob23 <- pnorm(23, mean=23.9, sd=0.43)
prob25 <- pnorm(25, mean=23.9, sd=0.43)

diff <- prob25-prob23
diff

##Week 2, session 2.2, population,sample, parameter, Ex 1 

dat <- read.csv("mice_pheno.csv")
library(dplyr)

x <- select( filter(dat, Sex=="M" & Diet=="chow"), Bodyweight)
x <-na.omit(x)

x_mean <- mean(x$Bodyweight)

##Ex 2
install.packages("rafalib")
library(rafalib)

popsd(x$Bodyweight)

##Ex 3

set.seed(1)
sam25 <-sample(x$Bodyweight, size = 25)
meanx <- mean(sam25)


##Ex 4
y <- select( filter(dat, Diet=="hf" & Sex=="M"), Bodyweight)
mean(y$Bodyweight)
y_mean <- mean(y$Bodyweight)

##Ex 5
y <- na.omit(y)
popsd(y$Bodyweight)

##Ex 6
set.seed(1)
samy <- sample(y$Bodyweight, size =25)
meany <- mean(samy)
meany

##Ex 7

meandiff_pop <- abs(x_mean-y_mean)
meandiff_pop
meandiff_sam <- abs(meanx-meany)
meandiff_sam

diff <- abs(meandiff_sam-meandiff_pop)
diff



##Ex 8

xf <- select( filter(dat, Sex=="F" & Diet=="chow"), Bodyweight)
xf <-na.omit(xf)

xf_mean <- mean(xf$Bodyweight)

set.seed(1)
fsam25 <-sample(xf$Bodyweight, size = 25)
meanxf <- mean(fsam25)


yf <- select( filter(dat, Diet=="hf" & Sex=="F"), Bodyweight)
yf <- na.omit(yf)
yf_mean <- mean(yf$Bodyweight)

set.seed(1)
fsamy <- sample(yf$Bodyweight, size =25)
meanyf <- mean(fsamy)
meanyf


fmeandiff_pop <- abs(xf_mean-yf_mean)
fmeandiff_pop
fmeandiff_sam <- abs(meanxf-meanyf)
fmeandiff_sam

fdiff <- abs(fmeandiff_sam-fmeandiff_pop)
fdiff

##Ex 9

popsd(xf$Bodyweight)
popsd(yf$Bodyweight)

popsd(x$Bodyweight)
popsd(y$Bodyweight)


##Week 2, session 2.3, population,sample, parameter

## Ex 4 

x <- select( filter(dat, Sex=="M" & Diet=="chow"), Bodyweight)
x <-na.omit(x)

x_mean <- mean(x$Bodyweight)

meanx
sd <- popsd(x$Bodyweight)
sd
ll <- x_mean-sd
ul <- x_mean+sd

if1sd <- (x$Bodyweight >ll) & (x$Bodyweight <ul)
if1sd
pp <-mean(if1sd)
pp

## Ex 5
ll <- x_mean-sd*2
ul <- x_mean+sd*2


if2sd <- (x$Bodyweight >ll) & (x$Bodyweight <ul)
if2sd
pp2 <-mean(if2sd)
pp2


## Ex 6

ll <- x_mean-sd*3
ul <- x_mean+sd*3


if3sd <- (x$Bodyweight >ll) & (x$Bodyweight <ul)
if3sd
pp3 <-mean(if3sd)
pp3

# Ex 7

qqnorm(x$Bodyweight, main = "Normal Q-Q plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)


## Ex 8

mypar(2,2)
y <- na.omit(filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist())
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- na.omit(filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist())
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- na.omit(filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist())
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- na.omit(filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist())
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

# Ex 9

y <- na.omit(filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist)
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)
popsd(avgs)


##Week 2, session 2.4, population,sample, parameter

##Ex 1

dat <- read.csv("femaleMiceWeights.csv")

x <-sample(1:6, 100, replace = TRUE)
mean(x==6)

set.seed(1)
props <- replicate(10000, mean(sample(1:6, 100, replace = TRUE)==6))
z <- (props-1/6)/sqrt(1/6*(1-1/6)/100)

mean(abs(z)>2)

qqnorm(z); abline(0,1)

# Ex 2
library(rafalib)

mypar(2,2)
p <- 0.5
n <- 30
set.seed(1)
props <- replicate(10000, mean(sample(0,1, n, replace = TRUE)==1))
z <- (props-p)/sqrt(p*(1-p)/n)

qqnorm(z)
qqline(z)

#Ex 3

x <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist
y <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist

mean(x)
popsd(x)


mean(means)
var <- (popsd(x))^2
var

sd1 <- sqrt((var*12)/11)
sd1

#Ex 7
library(dplyr)
x <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist

mu <- mean(x)
popsd(x)

se <- 3.022541/sqrt(12)
se
cv <- 2/se
cv
2*(1-pnorm(cv))

#Ex 8
x <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist
y <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist

varx <- var(x)
vary <- var(y)

se <- sqrt(vary/12+varx/12)

#Ex 10
tstat <- (mean(y)-mean(x))/se
tstat

#Ex 11
2*(1-pnorm(tstat))

#Ex 12
pval <- t.test(x, y, alternative = c("two.sided"),paired = FALSE, mu = 0, var.equal = FALSE, conf.level = 0.95)
or
pval2 <- 2*(1-pt(tstat, df=22))

###Week 3 Inference I####
rm(list = ls())

#Ex 1
babies <- read.table("babies.txt", header=TRUE)

bwt.ns <- filter(babies, smoke==0)%>% select(bwt)%>% unlist
bwt.s <- filter(babies, smoke==1)%>% select(bwt)%>% unlist

set.seed(1)
dat.ns <- sample(bwt.ns, size =25)
dat.s <- sample(bwt.s, size =25)

diff <- mean(dat.ns)-mean(dat.s)

se <- sqrt(var(dat.ns)/25+var(dat.s)/25)
se
tval <- diff/se
tval
#or
t.test(dat.ns, dat.s, alternative = c("two.sided"),paired = FALSE, mu = 0, var.equal = FALSE, conf.level = 0.95)

#Ex 2
pval <- 2*(1-pnorm(tval))
pval

#Ex 4
diff
ci99 <- se*qnorm(.995)
ci99

## Session 2, confidence interval

# Ex 1
ci99 <- se*qt(.995, df=48)
ci99

# Ex 4
set.seed(1)
d.ns <- sample(bwt.ns, size =5)
d.s <- sample(bwt.s, size =5)

t.test(d.ns, d.s, alternative = c("two.sided"),paired = FALSE, mu = 0, var.equal = FALSE, conf.level = 0.95)

# CI plot
N <- 30
d.ns <- sample(bwt.ns, size =N)

se.ns <- sd(d.ns)/sqrt(N)

Q <- qnorm(1-0.05/2)
#Q <- qt(1-0.05/2, df=4)
interval <- c(mean(d.ns)-Q*se.ns, mean(d.ns)+Q*se.ns)
interval
interval[1] < mean(bwt.ns) & interval[2] < mean(bwt.ns)

B <- 20
mypar()
plot(mean(d.ns)+c(-7,7),c(1,1),type="n", xlab = "B weight", ylab = "interval", ylim=c(1,B))
abline(v=mean(bwt.ns))

for (i in 1:B){
  d.ns <- sample(bwt.ns, size =N)
  se.ns <- sd(d.ns)/sqrt(N)
  interval <- c(mean(d.ns)-Q*se.ns, mean(d.ns)+Q*se.ns)
  covered <- mean(bwt.ns)<= interval[2]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
  
}

mypar()

## Session 3, power calculation

#Ex 2
N <- 90
B <- 10000
set.seed(1)

reject <- function(N, alpha=0.01){
  dat.ns <- sample(bwt.ns, size =N)
  dat.s <- sample(bwt.s, size =N)
  pval <- t.test(dat.ns, dat.s)$p.value
  pval < alpha
}

rejections <- replicate(B,reject(N))
mean(rejections)

#Ex 3
Ns <- seq(30, 120,30)

power <- sapply(Ns, function(N){
  rejections <- replicate(B, reject(N))
  mean(rejections)
})

plot(Ns, power, type="b")

N <- 30

## Inference II, Monte carlo simulation

##Session 1

# Ex 1
set.seed(1)
rsamp <- rnorm(5, mean = 0, sd = 1)
tstat <- sqrt(5)*mean(rsamp)/sd(rsamp)
tstat

# Ex 2
B <- 1000
set.seed(1)
ttestgenerator <- function(N, alpha=0.05){
  rsamp <- rnorm(N, mean = 0, sd = 1)
  tstat <- sqrt(N)*mean(rsamp)/sd(rsamp)
  statt <- tstat>2
}

prop <- replicate(B, ttestgenerator(5))
mean(prop)

# Ex 3
B <- 1000
set.seed(1)
ttestgenerator <- function(N, alpha=0.05){
  rsamp <- rnorm(N, mean = 0, sd = 1)
  tstat <- sqrt(N)*mean(rsamp)/sd(rsamp)
}

ns <- seq(5,30,5)
M <- 100
ps <- seq(1/(M+1), 1-1/(M+1), len=M)
qts <- qt(ps, df=N-1)
hist(qts)

mypar(3,2)
LIM <- c(-4.5, 4.5)
for(N in ns){
  tt <- replicate(B,  ttestgenerator(N))
  qqplot(qts, tt, xlab = "theoritical", ylab = "observed", xlim = LIM, ylim = LIM)
  abline(0, 1)
}

# Ex 4

B <- 1000
set.seed(1)
ttestgenerator <- function(N){
  rsamp1 <- rnorm(N, mean = 0, sd = 1)
  rsamp2 <- rnorm(N, mean = 0, sd = 1)
  tt <- t.test(rsamp1, rsamp2, alternative = c("two.sided"),paired = FALSE, mu = 0, var.equal = TRUE, conf.level = 0.95)$statistic
}
tt2 <- replicate(B,  ttestgenerator(N))
hist(tt2)
M <- 100
ps <- seq(1/(M+1), 1-1/(M+1), len=M)
qts <- qt(ps, df=2*N-2)

mypar(3,2)
LIM <- c(-4.5, 4.5)
for(N in ns){
  tt <- replicate(B,  ttestgenerator(N))
  qqplot(qts, tt, xlab = "theoritical", ylab = "observed", xlim = LIM, ylim = LIM)
  abline(0, 1)
}

# Ex 5

B <- 10000
ttestgenerator <- function(N, alpha=0.05){
  x <- sample(c(0,1), N, replace = TRUE)
  tstat <- sqrt(N)*mean(x)/sd(x)
}
tt <- replicate(B,  ttestgenerator(15))

M <- 100
ps <- seq(1/(M+1), 1-1/(M+1), len=M)
qts <- qt(ps, df=14)
mypar()
qqplot(qts, tt, xlab = "theoritical", ylab = "observed")
abline(0, 1)

# Ex 6

tt <- replicate(B,  ttestgenerator(1000))

M <- 100
ps <- seq(1/(M+1), 1-1/(M+1), len=M)
qts <- qt(ps, df=999)
mypar()
qqplot(qts, tt, xlab = "theoritical", ylab = "observed")
abline(0, 1)

# Ex 7

B <- 10000
meddist <- function(N){
  x <- rnorm(10, 0, 1)
  med <- median(x)
}

me <- replicate(B,  meddist(15))

mypar(3,2)
for(N in ns){
  tt <- replicate(B,  meddist(N))
  qqnorm(me)
  abline(0, 1)
}

rm(list=ls())

##Session 2

#Ex 1
babies <- read.table("babies.txt", header=TRUE)

bwt.ns <- filter(babies, smoke==0)%>% select(bwt)%>% unlist
bwt.s <- filter(babies, smoke==1)%>% select(bwt)%>% unlist

N <- 10
set.seed(1)
nsmoker <- sample(bwt.ns, N)
smoker <- sample(bwt.s, N)
obs <- mean(smoker)-mean(nsmoker)

avgdiff <- replicate(1000, {
  all <- sample(c(smoker, nsmoker))
  nsmk <- all[1:N]
  smk <- all[(N+1):(2*N)]
  return(mean(smk) - mean(nsmk))
  
})
mypar()
hist(avgdiff)
abline(v=obs, col="red", lwd=2)

pv <- (sum(abs(avgdiff) >= abs(obs))+1)/(length(avgdiff)+1)
pv

# Ex 2

N <- 10
set.seed(1)
nsmoker <- sample(bwt.ns, N)
smoker <- sample(bwt.s, N)
obs <- median(smoker)-median(nsmoker)

avgdiff <- replicate(1000, {
  all <- sample(c(smoker, nsmoker))
  nsmk <- all[1:N]
  smk <- all[(N+1):(2*N)]
  return(median(smk) - median(nsmk))
  
})
mypar()
hist(avgdiff)
abline(v=obs, col="red", lwd=2)

pv <- (sum(abs(avgdiff) >= abs(obs))+1)/(length(avgdiff)+1)
pv

#Session 3
# Ex 1
dat <- read.csv("assoctest.csv")

table(dat$allele, dat$case)

chisq.test(dat$allele, dat$case)

# Ex 2
fisher.test(dat$allele, dat$case)

### Week 4, EDA

## Session 2
load("skew.RData")

# Ex 1,2
par(mfrow = c(3,3))
for (i in 1:9) {
  #hist(dat[,i], main = paste("Histogram of" , i))
  qqnorm(dat[,i])
}

# Session 3
# Ex 1
dat <- InsectSprays

## Simulation for SCCB cost effectiveness
#B <- 10000
#dat <- function(N){
#x <- runif(N, min = 0, max = 1)
  #sds <- sd(x)
# men <- mean(x)
#}
  
#y <- replicate(B, dat(30))
#hist(y)
#max(y)
#mean(y)
#median(y)
mypar()
boxplot(dat$count~dat$spray)
boxplot(split(dat$count,dat$spray))

# Ex 2
library(dplyr)
library(rafalib)
install.packages("UsingR")
library(UsingR)
data(nym.2002, package="UsingR")
dim(nym.2002)
head(nym.2002)


boxplot(split(nym.2002$time,nym.2002$gender))


ma <- filter(nym.2002, gender=="Male")%>% select(time)%>% unlist
fe <- filter(nym.2002, gender=="Female")%>% select(time)%>% unlist

par(mfrow = c(1,2))
hist(ma)
hist(fe)
mean(ma)-mean(fe)

#Session 4, Ex 1
ma <- filter(nym.2002, gender=="Male")
fe <- filter(nym.2002, gender=="Female")
par(mfrow = c(1,1))
scatter.smooth(ma$age, ma$time)
cor(ma$age, ma$time)

#Ex 2
scatter.smooth(fe$age, fe$time)
cor(fe$age, fe$time)

#Ex 3

agecat <- cut(nym.2002$age, c(20,25,30,35,40,45,50,55,60,65,70,75,81))
agecat1 <- cut(nym.2002$age, seq(20,90,5)) 

scatter.smooth(nym.2002$age, nym.2002$time)
boxplot(split(nym.2002$time,agecat))

#Session 5, Ex 1
time <- sort(nym.2002$time)
min(time)/median(time)

#Ex 2
max(time)/median(time)

par(mfrow=c(1,2))
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

##Week 4, Robust summaries
#Session 1, Ex 1
rm(list = ls())
data(ChickWeight)

par(mfrow=c(1,1))
plot( ChickWeight$Time)
plot( ChickWeight$Time, ChickWeight$weight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

chick <- reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",direction="wide")
chick <- na.omit(chick)

wt4 <- c(chick$weight.4, 3000)
mean(wt4)/mean(chick$weight.4)

# Ex 2
median(wt4)/median(chick$weight.4)

# Ex 3
sd(wt4)/sd(chick$weight.4)

# Ex 4
mad(wt4)/mad(chick$weight.4)

# Ex 5
wt21 <- c(chick$weight.21, 3000)

plot(chick$weight.4, chick$weight.21)
plot(wt4, wt21)

cor(chick$weight.4, chick$weight.21)
cor(wt4, wt21)
cor(wt4, wt21)/cor(chick$weight.4, chick$weight.21)

#Session 2, Ex 1
x <- filter(chick, chick$Diet==1)%>% select(weight.4)%>% unlist 
y <- filter(chick, chick$Diet==4)%>% select(weight.4)%>% unlist 
x2 <- c(x,200)

t.test(x2,y)

#Ex 2
wilcox.test(x2,y)

#Ex 3
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y+10)$statistic-t.test(x,y+100)$statistic

#Ex 4
m <- c(1,2,3)
n <- c(400,500,600)
wilcox.test(m,n)






