#Regression Test (SLR)
setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")
rm(list = ls())

#y=b0+b1x+e，e~N(0,var)
#testing: H0: b1=0
#p-value of b1 < alpha --- reject H0 --- power

b0 <- 1
b1vec <- c(0.2, 0.5, 0.7, 1.0, 1.5)#change b1 values to find good power

N <- 10000
alpha <- 0.05
nvec <- c(10,20,30,50,100)
varvec <- c(1,3,5,8,10)


altmatrixc <- power_normt_matrix <- normmatrix <-
  array(NA, dim = c(length(b1vec), length(nvec), length(varvec)))

####################################################################
for(s in 1:length(b1vec)){
  b1 <- b1vec[s]
  for(i in 1:length(nvec)){
    print(c(s,i))
    n <- nvec[i]
    for(k in 1:length(varvec)){
      v <- varvec[k]
      rejectH0_e_norm <- rejectH0_e_alt <- rejectH0_e_normtest <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square", 5)#set df=5, var = 10
      
      #e ~ Normal
       e <- rnorm(n, mean = 0, sd= sqrt(v))#var=v,mean=0
       y <- b0+b1*x+e
       out <- summary(lm(formula = y~x))
       pval <- out$coefficients[,4][2]
       if(pval < alpha){
          rejectH0_e_norm[j] <- 1
       }#reject b1=0---power
       
       #e ~ non-Normal
       e <- rchisq(n, df = v/2)- (v/2) #var=v, so df=v/2=mean
       ###set mean to zero!!!!!!
       ###make variance smaller
       ###t: e <- rt(n, df = 25/12) var = d/d-2 =25
       ###exponential,beta,uniform
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
  normmatrix[s,i,k] <- mean(rejectH0_e_norm)
  altmatrixc[s,i,k] <- mean(rejectH0_e_alt)
  power_normt_matrix[s,i,k] <- mean(rejectH0_e_normtest)
  }
  }
}
save.image(paste0("SLR",".RData"))
#####################################################################

normmatrix
altmatrixc

########################3Find b1 value with good power
#Normal
plot(nvec,normmatrix[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power", xlab = "Sample Size", col = 1,
     main = "Normal, Variance = 1, power ~ b1")
for (j in c(2:5)){
  lines(nvec, normmatrix[j,,1], col = j)
}
legend("bottomright", legend = paste("b1 = ", b1vec),
       col=c(1:5),cex = 0.7,lty = 1)

#Observation: when b1=0.2, the power trend is more clear 

#Chi-sq(variance = 1, very skewed)
plot(nvec,altmatrixc[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power", xlab = "Sample Size", col = 1,
     main = "Chi-sq, Variance = 1, power ~ b1")
for (j in c(2:5)){
  lines(nvec, altmatrixc[j,,1], col = j)
}
legend("bottomright", legend = paste("b1 = ", b1vec),
       col=c(1:5),cex = 0.7,lty = 1)


#########################b1 fixed, variance value change
#Normal
plot(nvec,normmatrix[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power", xlab = "Sample Size", 
     main = "Normal, b1=0.2, power ~ variance")
for (j in c(2:5)){
  lines(nvec, normmatrix[1,,j], col = j)
}
legend("bottomright", legend = paste("var = ", varvec),
       col=c(1:5),cex = 0.7, lty = 1)

#Observation: when b1 is fixed, power decreases while variance increases

#Chi-sq
plot(nvec,altmatrixc[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power", xlab = "Sample Size", 
     main = "Chi-sq, b1=0.2, power ~ variance")
for (j in c(2:5)){
  lines(nvec, altmatrixc[1,,j], col = j)
}
legend("bottomright", legend = paste("var = ", varvec),
       col=c(1:5),cex = 0.7, lty = 1)


#########################Utility of normality test
j <- 1
powerdif <- normmatrix[1,,j] - altmatrixc[1,,j]
plot(power_normt_matrix[1,,j], powerdif, typ = "l", col = "1",
     ylab = "Power loss", 
     xlab = "Power of normality test",
     #ylim = c(-0.2, 0.15), xlim = c(0.05, 1),
     main = "Utility of normality test")
for (j in c(2:5)){
  powerdif <- normmatrix[2,,j] - altmatrixc[2,,j]
  lines(power_normt_matrix[2,,j], powerdif, typ = "l", col = j)
}
abline(h = 0, lty = 2, lwd = 0.6)

legend("bottomright", legend = paste("var = ", varvec),
      col=c(1:5),cex = 0.7, lty = 1)




save.image(paste0("SLR-Chisq",".RData"))





#power and type I error summary for normality tests
#create similar plotes as in ppt
#all 4 plots as for uniform for all dists (in ppt)

#also for regression tests (also 4 plots for each b1 and dist)

#also for different normality tests








