###Plots for downstream effects

library("nortest")
library("dgof")
library("dplyr")
library(moments)

setwd("/Users/echo/Desktop")
source("Organized User-defined functions.R")

#Same Mean & Violated Normality
alpha <- 0.05
N <- 10000
nvec <- c(5,10,15,20,25)
dfvec <- c(3,5,8,10,15)
powervec <- numeric(length(nvec)*length(dfvec))
powermatrixs <- matrix(powervec,
                       nrow=length(nvec),
                       ncol=length(dfvec),byrow=TRUE)

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    rejectH0 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square",df)
      y <- generate_data(n,"Chi-square",df)
      out <- t.test(x,y)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixs[i,k] <- mean(rejectH0)
  }
}
powermatrixs
#rows:i--n, columns: k--df
matplot(powermatrixs, type='l', 
        xlab='n', ylab='P(type I)', 
        xaxt = "n",
        main = "P(typeI)-Normality Violated",
        col=1:5,
        lty = 1:5)
abline(h = 0.05, col = "purple",pch = 3)
legend("bottomright",
       cex = 0.5,
       legend = c("df=3","df=5","df=8","df=10","df=15"),
       col = 1:5,
       lty = 1:5,
       title = "Degree of Freedom")


#Different Mean & Violated Normality
powermatrixd <- matrix(powervec,
                       nrow=length(nvec),
                       ncol=length(dfvec),byrow=TRUE)

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    rejectH0 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square",df)
      y <- generate_data(n,"Chi-square",df)
      out <- t.test(x,y+1)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixd[i,k] <- mean(rejectH0)
  }
}
powermatrixd
#rows:i--n, columns: k--df
matplot(powermatrixd,
        type='l', 
        xlab='n', ylab='Power', 
        xaxt = "n",
        main = "Power-Normality Violated",
        col=1:5,
        lty = 1:5)
abline(h = 0.05, col = "purple",pch = 3)
legend("topleft",
       cex = 0.5,
       legend = c("df=3","df=5","df=8","df=10","df=15"),
       col = 1:5,
       lty = 1:5,
       title = "Degree of Freedom")


#Same mean & Normality
powermatrixN <- matrix(powervec,
                       nrow=length(nvec),
                       ncol=length(dfvec),byrow=TRUE)

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    rejectH0 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Normal",df, 5)
      y <- generate_data(n,"Normal",df, 10)#different sd-not two exactly same distribution
      out <- t.test(x,y)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixN[i,k] <- mean(rejectH0)
  }
}
powermatrixN
#rows:i--n, columns: k--df
matplot(powermatrixN, 
        type='l', 
        xlab='n', ylab='P(type I)', 
        xaxt = "n",
        main = "P(typeI)-Normality Met",
        col=1:5,
        lty = 1:5)
abline(h = 0.05, col = "purple",pch = 3)
legend("bottomleft",
       cex = 0.4,
       legend = c("mean=3","mean=5","mean=8",
                  "mean=10","mean=15"),
       col = 1:5,
       lty = 1:5,
       title = "Mean")
