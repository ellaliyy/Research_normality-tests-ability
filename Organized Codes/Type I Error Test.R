


library(tidyverse)
library(ggpubr)
library(rstatix)

#Null hypothesis~H0:mean1 = mean2

#x,y have same means and violate the normality
alpha <- 0.05
N <- 1000
rejectH0 <- pval <-  numeric(N)

for(i in 1:N){
  x <- rchisq(25, 30)
  y <- rchisq(25, 30)
  out <- t.test(x,y)
  pval[i] <- out$p.value
  if(pval[i] < alpha){
    rejectH0[i] <- 1
  }
}
TypeIRate1 <- mean(rejectH0)

#x,y have different means and violate the normality
for(i in 1:N){
  x <- rchisq(25, 30)
  y <- rchisq(25, 32)
  out <- t.test(x,y)
  pval[i] <- out$p.value
  if(pval[i] < alpha){
    rejectH0[i] <- 1
  }
}
TypeIRate2 <- mean(rejectH0)

##############################################
##############################################
#TypeI Error Rate---df=3#
nvec <- c(5,10,15,20,25)
powervec3 <- numeric(length(nvec))

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(j in 1:N){
    x <- rchisq(n,3)
    y <- rchisq(n,3)
    out <- t.test(x,y)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  powervec3[i] <- mean(rejectH0)
}
powervec3
barplot(powervec3, main="TypeI Error Rate---df=3")

#########################################
#TypeI Error Rate---df=5#
nvec <- c(5,10,15,20,25)
powervec5 <- numeric(length(nvec))

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(j in 1:N){
    x <- rchisq(n,3)
    y <- rchisq(n,3)
    out <- t.test(x,y)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  powervec5[i] <- mean(rejectH0)
}
powervec5
barplot(powervec5, main="TypeI Error Rate---df=5")

##########################################
#TypeI Error Rate---df=8#
nvec <- c(5,10,15,20,25)
powervec8 <- numeric(length(nvec))

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(j in 1:N){
    x <- rchisq(n,3)
    y <- rchisq(n,3)
    out <- t.test(x,y)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  powervec8[i] <- mean(rejectH0)
}
powervec8
barplot(powervec8, main="TypeI Error Rate---df=8")

############################################
#TypeI Error Rate---df=10#
powervec10 <- numeric(length(nvec))
for(i in 1:length(nvec)){
  n <- nvec[i]
  for(j in 1:N){
    x <- rchisq(n,10)
    y <- rchisq(n,10)
    out <- t.test(x,y)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  powervec10[i] <- mean(rejectH0)
}
powervec10
barplot(powervec10, main="TypeI Error Rate---df=10")

