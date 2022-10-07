setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")

alpha <- 0.05
N <- 10000
smpn <- c(5,6,7,8,9,10,11,12,13,15,17,20,22,25,30,35,40,45,50,100)

###########Chi(3) vs Normal
#Power1-Power of Normalty Test
power1 <- numeric(length(smpn))
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
  power1[i] <- mean(rejectH0)
}
power1

#Power2-power of downstream test(two sample t-test here) for Chi-square
power2 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    x <- generate_data(n,"Chi-square", 3)
    y <- generate_data(n,"Chi-square", 3)+1
    out <- t.test(x/sqrt(6),y/sqrt(6))
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  power2[i] <- mean(rejectH0)
}
power2

#Power3-power of downstream test(two sample t-test here) for Normal
power3 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    x <- rnorm(n, mean=3, sd=sqrt(6))
    y <- rnorm(n, mean=3, sd=sqrt(6))+1
    out <- t.test(x/sqrt(6),y/sqrt(6))
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  power3[i] <- mean(rejectH0)
}
power3

dif <- power3 - power2
dif
plot(power1, dif,
     xlab='Power of normality test', 
     ylab='Power loss for downstream test',
     col = 'green',
     main = "Chi(3) vs Normal")
abline(h = 0, col = "red",pch = 5)

###########Chi(5) vs Normal
#Power1-Power of Normalty Test
power1 <- numeric(length(smpn))
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
  power1[i] <- mean(rejectH0)
}
power1

#Power2-power of downstream test(two sample t-test here) for Chi-square
power2 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    x <- generate_data(n,"Chi-square", 5)
    y <- generate_data(n,"Chi-square", 5)+1
    out <- t.test(x/sqrt(10),y/sqrt(10))
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  power2[i] <- mean(rejectH0)
}
power2

#Power3-power of downstream test(two sample t-test here) for Normal
power3 <- numeric(length(smpn))
for(i in 1:length(smpn)){
  n <- smpn[i]
  rejectH0 <- numeric(N)
  for(j in 1:N){
    x <- rnorm(n, mean=5, sd=sqrt(10))
    y <- rnorm(n, mean=5, sd=sqrt(10))+1
    out <- t.test(x/sqrt(10),y/sqrt(10))
    pval <- out$p.value
    if(pval < alpha){
      rejectH0[j] <- 1
    }
  }
  power3[i] <- mean(rejectH0)
}
power3

dif <- power3 - power2
dif
plot(power1, dif,
     xlab='Power of normality test', 
     ylab='Power loss for downstream test',
     col = 'green',
     main = "Chi(5) vs Normal")
abline(h = 0, col = "red",pch = 5)

