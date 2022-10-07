#rm(list = ls())
###Plots for downstream effects
setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")

alpha <- 0.05
N <- 10000
nvec <- c(5,10,15,20,25,30,40,50,75,100)
dfvec <- c(3,5,8,10,15)
powervec <- numeric(length(nvec)*length(dfvec))

#Same Mean & Violated Normality
powermatrixs <- matrix(powervec,
                       nrow=length(nvec),
                       ncol=length(dfvec),byrow=TRUE)
row.names(powermatrixs) <- nvec
colnames(powermatrixs) <- dfvec

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    rejectH0 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square",df)
      y <- generate_data(n,"Chi-square",df)
      out <- t.test(x/sqrt(2*df),y/sqrt(2*df))
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixs[i,k] <- mean(rejectH0)
  }
}

plot(nvec, powermatrixs[,1],
     xlab = 'sample size',
     ylab = 'P(type I)',
     col = 'red',
     pch=18,
     lty=1,
     type = 'b',
     main = "P(type I)-Normality violated")
lines(nvec, powermatrixs[,2],
      col = 'blue',
      pch=18,
      lty=2,
      type = 'b')
lines(nvec, powermatrixs[,3],
      col = 'orange',
      pch=18,
      lty=3,
      type = 'b')
lines(nvec, powermatrixs[,4],
      col = 'green',
      pch=18,
      lty=4,
      type = 'b')
lines(nvec, powermatrixs[,5],
      col = 'yellow',
      pch=18,
      lty=5,
      type = 'b')
abline(h = 0.05, col = "black",pch = 35)
legend("bottomright",
       legend = c("df=3","df=5","df=8","df=10","df=15"),
       col=c("red", "blue",'orange','green','yellow'),
       lty = c(1:5))

#Different Mean & Violated Normality
powermatrixd <- matrix(powervec,
                       nrow=length(nvec),
                       ncol=length(dfvec),byrow=TRUE)
row.names(powermatrixd) <- nvec
colnames(powermatrixd) <- dfvec

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    rejectH0 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square", df)
      y <- generate_data(n,"Chi-square", df)+1
      out <- t.test(x/sqrt(2*df),y/sqrt(2*df))
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixd[i,k] <- mean(rejectH0)
  }
}
plot(nvec, powermatrixd[,1],
     xlab = 'sample size',
     ylab = 'Power',
     col = 'red',
     pch=18,
     lty=1,
     type = 'b',
     main = "Power-Normality violated")
lines(nvec, powermatrixd[,2],
      col = 'blue',
      pch=18,
      lty=2,
      type = 'b')
lines(nvec, powermatrixd[,3],
      col = 'orange',
      pch=18,
      lty=3,
      type = 'b')
lines(nvec, powermatrixd[,4],
      col = 'green',
      pch=18,
      lty=4,
      type = 'b')
lines(nvec, powermatrixd[,5],
      col = 'yellow',
      pch=18,
      lty=5,
      type = 'b')
legend("topleft",
       legend = c("df=3","df=5","df=8","df=10","df=15"),
       col=c("red", "blue",'orange','green','yellow'),
       lty = c(1:5))


#Same mean & Normality
powermatrixN <- matrix(powervec,
                       nrow=length(nvec),
                       ncol=length(dfvec),byrow=TRUE)
row.names(powermatrixN) <- nvec
colnames(powermatrixN) <- dfvec

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    rejectH0 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Normal",df)
      y <- rnorm(n, mean = df, sd = (df+2))
      #different sd-not two exactly same distribution
      out <- t.test(x,y)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixN[i,k] <- mean(rejectH0)
  }
}
plot(nvec, powermatrixN[,1],
     xlab = 'sample size',
     ylab = 'P(Type I)',
     col = 'red',
     pch=18,
     lty=1,
     type = 'b',
     main = "P(type I)-Normality met")
lines(nvec, powermatrixN[,2],
      col = 'blue',
      pch=18,
      lty=2,
      type = 'b')
lines(nvec, powermatrixN[,3],
      col = 'orange',
      pch=18,
      lty=3,
      type = 'b')
lines(nvec, powermatrixN[,4],
      col = 'green',
      pch=18,
      lty=4,
      type = 'b')
lines(nvec, powermatrixN[,5],
      col = 'yellow',
      pch=18,
      lty=5,
      type = 'b')
legend("topleft",
       legend = c("df=3","df=5","df=8","df=10","df=15"),
       col=c("red", "blue",'orange','green','yellow'),
       lty = c(1:5),cex=0.5)
abline(h = 0.05, col = "black",pch = 35)



#Different mean & Normality
powermatrixND <- matrix(powervec,
                       nrow=length(nvec),
                       ncol=length(dfvec),byrow=TRUE)
row.names(powermatrixND) <- nvec
colnames(powermatrixND) <- dfvec

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    rejectH0 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Normal",df)
      y <- rnorm(n, mean = df+1, sd = df)
      out <- t.test(x/sqrt(df),y/sqrt(df))
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixND[i,k] <- mean(rejectH0)
  }
}
plot(nvec, powermatrixND[,1],
     xlab = 'sample size',
     ylab = 'Power',
     col = 'red',
     pch=18,
     lty=1,
     type = 'b',
     main = "Power-Normality met")
lines(nvec, powermatrixND[,2],
      col = 'blue',
      pch=18,
      lty=2,
      type = 'b')
lines(nvec, powermatrixND[,3],
      col = 'orange',
      pch=18,
      lty=3,
      type = 'b')
lines(nvec, powermatrixND[,4],
      col = 'green',
      pch=18,
      lty=4,
      type = 'b')
lines(nvec, powermatrixND[,5],
      col = 'yellow',
      pch=18,
      lty=5,
      type = 'b')
legend("topleft",
       legend = c("df=3","df=5","df=8","df=10","df=15"),
       col=c("red", "blue",'orange','green','yellow'),
       lty = c(1:5),cex=0.5)



