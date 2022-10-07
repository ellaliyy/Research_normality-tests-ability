
#X-AXIS: sample size(n) keep increasing
#Y-AXIS: Normal Power - Chi-square power

setwd("/Users/zyj/Desktop")
source("Organized User-defined functions.R")

alpha <- 0.05
N <- 10000
smpn <- c(5,10,15,20,25,30,35,40,45,50,75,100)
#powerlis <- numeric(length(smpn))

######Chi(3) vs Normal
#Chi(3)
powerchi3 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square", 3)
      out <- shapiro.test(x)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powerchi3[i] <- mean(rejectH0)
}
powerchi3
#Normal(3,6)
TypeIN3 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    y <- rnorm(n, mean = 3, sd = sqrt(6))
    out <- shapiro.test(y)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  TypeIN3[i] <- mean(rejectH0)
}
TypeIN3
#diff between Chi(3) and N(3)
dif3 <- TypeIN3 - powerchi3
dif3
plot(smpn, dif3,
     xlab='Power of normality test', 
     ylab='Power loss for downstream',
     col = 'red',
     main = "Chi(3) vs Normal")

######Chi(5) vs Normal
#Chi(5)
powerchi5 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    x <- generate_data(n,"Chi-square", 5)
    out <- shapiro.test(x)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  powerchi5[i] <- mean(rejectH0)
}
powerchi5
#Normal(5,10)
TypeIN5 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    y <- rnorm(n, mean = 5, sd = sqrt(10))
    out <- shapiro.test(y)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  TypeIN5[i] <- mean(rejectH0)
}
TypeIN5
#diff between Chi(5) and N(5)
dif5 <- TypeIN5 - powerchi5
dif5
plot(smpn, dif5,
     xlab='Power of normality test', 
     ylab='Power loss for downstream',
     col = 'red',
     main = "Chi(5) vs Normal")

######Chi(15) vs Normal
#Chi(15)
powerchi15 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    x <- generate_data(n,"Chi-square", 5)
    out <- shapiro.test(x)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  powerchi15[i] <- mean(rejectH0)
}
powerchi15
#Normal(15,30)
TypeIN15 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    y <- rnorm(n, mean = 15, sd = sqrt(30))
    out <- shapiro.test(y)
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  TypeIN15[i] <- mean(rejectH0)
}
TypeIN15
#diff between Chi(15) and N(15)
dif15 <- TypeIN15 - powerchi15
dif15
plot(smpn, dif15,
     xlab='Power of normality test', 
     ylab='Power loss for downstream',
     col = 'red',
     main = "Chi(15) vs Normal")








