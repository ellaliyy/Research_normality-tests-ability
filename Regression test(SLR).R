#Regression Test (SLR)
setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")
rm(list = ls())

#y=b0+b1x+e，e~N(0,var)
#testing: H0: b1=0
#p-value of b1 < alpha --- reject H0 --- power

b0 <- 1
b1 <- 0.7#change b1 values to find good power

N <- 10000
alpha <- 0.05
nvec <- c(10,20,30,50,100)
#dfvec <- c(5,10,15,30)

power_norm <- power_alt <- power_normtest <- numeric(length(nvec))


for(i in 1:length(nvec)){
  print(i)
  n <- nvec[i]
  rejectH0_e_norm <- rejectH0_e_alt <- rejectH0_e_normtest <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square", 5)#set df=5
      
      #e ~ Normal(0, sd=5)
       e <- rnorm(n, mean = 0, sd=5)#var=25
       y <- b0+b1*x+e
       out <- summary(lm(formula = y~x))
       pval <- out$coefficients[,4][2]
       if(pval < alpha){
          rejectH0_e_norm[j] <- 1
       }#reject b1=0---power
       
       #e ~ non-Normal
       e <- rchisq(n, 25/2)#var=25, so df=25/2
       y <- b0+b1*x+e
       out <- summary(lm(formula = y~x))
       pval <- out$coefficients[,4][2]
       if(pval < alpha){
         rejectH0_e_alt[j] <- 1
       }
       
       #e ~ normality test
       out <- shapiro.test(e)
       pval <- out$p.value
       if(pval < alpha){
         rejectH0_e_normtest[j] <- 1
       }
    }
  power_norm[i] <- mean(rejectH0_e_norm)
  power_alt[i] <- mean(rejectH0_e_alt)
  power_normtest[i] <- mean(rejectH0_e_normtest)
}

#plot norm vs alt
plot(nvec,power_norm, 
     type = "l", ylim = c(0, 1),lty=1, 
     ylab = "Power Loss", xlab = "Sample Size", 
     main = "plot norm vs alt")
lines(nvec, power_alt, col = "red", lty = 5)

#plot normtest vs power difference
diff <- power_norm - power_alt
plot(power_normtest, diff, type = 'l', ylim = c(-0.2, 0.2),
     xlab = "Power of Normality test", ylab = "Power difference",
     main = "plot normtest vs power difference")
abline(h = 0, col = 2)


save.image(paste0("SLR",".RData"))

