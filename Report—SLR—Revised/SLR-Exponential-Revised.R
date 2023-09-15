#Regression Test (SLR)
setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")
rm(list = ls())

#y=b0+b1x+e，e~N(0,var)
#testing: H0: b1=0
#p-value of b1 < alpha --- reject H0 --- power

b0 <- 1
b1vec <- c(0.2, 0.5, 0.7, 1.0, 1.5,0)#change b1 values to find good power, add 0 for typeI error rate

N <- 1000
alpha <- 0.05
nvec <- c(10,20,30,50,100)
dfvec <- c(1,2,3,4,5)


altmatrixc <- power_normt_matrix <- normmatrix <-
  array(NA, dim = c(length(b1vec), length(nvec), length(dfvec)))

####################################################################
for(s in 1:length(b1vec)){
  b1 <- b1vec[s]
  for(i in 1:length(nvec)){
    print(c(s,i))
    n <- nvec[i]
    for(k in 1:length(dfvec)){
      d <- dfvec[k]
      rejectH0_e_norm <- rejectH0_e_alt <- rejectH0_e_normtest <- numeric(N)
      for(j in 1:N){
        x <- generate_data(n,"Chi-square", 5)#set df=5, var = 10
        
        #e ~ Normal
        e <- rnorm(n, mean = 0, sd = d)#var=d^2,mean=0
        y <- b0+b1*x+e
        out <- summary(lm(formula = y~x))
        pval <- out$coefficients[,4][2]
        if(pval < alpha){
          rejectH0_e_norm[j] <- 1
        }#reject b1=0---power
        
        #e ~ non-Normal
        e1 <- generate_data(n, "Expotential", d)
        #rate = 1/df, so mean = df, sd = df
        e <- e1 - d
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

library(Cairo)
########################Find b1 value with good power
Cairo(file = "chiplot-power.pdf",typ = "pdf",dpi=95,
      height = 500, width = 1100)
par(mfrow = c(1,2))

#Normal
plot(nvec,normmatrix[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power", xlab = "Sample Size", col = 1,
     main = "Normal, Variance = 1, power ~ b1")
for (j in c(2:5)){
  lines(nvec, normmatrix[j,,1], col = j)
}
legend("bottomright", legend = paste("b1 = ", b1vec[1:5]),
       col=c(1:5),cex = 0.7,lty = 1)



#Exponential(variance = 2, very skewed)
plot(nvec,altmatrixc[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power", xlab = "Sample Size", col = 1,
     main = "Exponential, Variance = 1, power ~ b1")
for (j in c(2:5)){
  lines(nvec, altmatrixc[j,,1], col = j)
}
legend("bottomright", legend = paste("b1 = ", b1vec[1:5]),
       col=c(1:5),cex = 0.7,lty = 1)
dev.off()




############################Type I error rates
Cairo(file = "chiplot-typeI.pdf",typ = "pdf",dpi=95,
      height = 500, width = 1100)
par(mfrow = c(1,2))

#Normal
plot(nvec,normmatrix[6,,1], 
     type = "l", ylim = c(0, 0.2), 
     ylab = "TypeI error rate", xlab = "Sample Size", col = 1,
     main = "Normal, Variance = 1, TypeI error rate")
abline(h = 0.05, col = "red")


#Exponential(variance = 2, very skewed)
plot(nvec,altmatrixc[6,,1], 
     type = "l", ylim = c(0, 0.2), 
     ylab = "TypeI error rate", xlab = "Sample Size", col = 1,
     main = "Exponential, Variance = 1, TypeI error rate")
abline(h = 0.05, col = "red")

dev.off()


#########################b1 fixed, variance value change
Cairo(file = "chiplot-power vs var.pdf",typ = "pdf",dpi=95,
      height = 500, width = 1100)
par(mfrow = c(1,2))


#Normal
plot(nvec,normmatrix[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power", xlab = "Sample Size", 
     main = "Normal, b1=0.2, power ~ variance")
for (j in c(2:5)){
  lines(nvec, normmatrix[1,,j], col = j)
}
legend("bottomright", legend = paste("df = ", dfvec),
       col=c(1:5),cex = 0.7, lty = 1)

#Exponential
plot(nvec,altmatrixc[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power", xlab = "Sample Size", 
     main = "Exponential, b1=0.2, power ~ variance")
for (j in c(2:5)){
  lines(nvec, altmatrixc[1,,j], col = j)
}
legend("bottomright", legend = paste("df = ", dfvec),
       col=c(1:5),cex = 0.7, lty = 1)

dev.off()

##########################Normality test
Cairo(file = "chiplot-normality.pdf",typ = "pdf",dpi=95,
      height = 500, width = 1100)
par(mfrow = c(1,1))


plot(nvec,power_normt_matrix[1,,1], 
     type = "l", ylim = c(0, 1), 
     ylab = "Power of Normality test", xlab = "Sample Size", 
     main = "Normal, b1=0.2, power of Normality test ~ variance")
for (j in c(2:5)){
  lines(nvec, power_normt_matrix[1,,j], col = j)
}
legend("bottomright", legend = paste("df = ", dfvec),
       col=c(1:5),cex = 0.7, lty = 1)

dev.off()


#########################Utility of normality test
Cairo(file = "chiplot-utility.pdf",typ = "pdf",dpi=95,
      height = 500, width = 1100)
par(mfrow = c(1,2))


#Power
j <- 1
powerdif <- normmatrix[1,,j] - altmatrixc[1,,j]
plot(power_normt_matrix[1,,j], powerdif, typ = "l", col = "1",
     ylab = "Power loss", 
     xlab = "Power of normality test",
     #ylim = c(-0.2, 0.2), xlim = c(0.05, 1),
     main = "Utility of normality test-power")
for (j in c(2:5)){
  powerdif <- normmatrix[1,,j] - altmatrixc[1,,j]
  lines(power_normt_matrix[1,,j], powerdif, typ = "l", col = j)
}
abline(h = 0, lty = 2, lwd = 0.6)

legend("bottomright", legend = paste("df = ", dfvec),
       col=c(1:5),cex = 0.7, lty = 1)


#Type I error rates
j <- 1
inflatedtypeI <- altmatrixc[6,,j] - normmatrix[6,,j]
plot(power_normt_matrix[6,,j], inflatedtypeI, typ = "l", col = "1",
     ylab = "Inflated TypeI error rate", 
     xlab = "Power of normality test",
     #ylim = c(-0.2, 0.2), xlim = c(0.05, 1),
     main = "Utility of normality test-typeI")
for (j in c(2:5)){
  inflatedtypeI <- altmatrixc[6,,j] - normmatrix[6,,j]
  lines(power_normt_matrix[6,,j], inflatedtypeI, typ = "l", col = j)
}
abline(h = 0, lty = 2, lwd = 0.6)
legend("bottomright", legend = paste("df = ", dfvec),
       col=c(1:5),cex = 0.7, lty = 1)

dev.off()


##############################################

save.image(paste0("SLR-Exponential-Revised",".RData"))

