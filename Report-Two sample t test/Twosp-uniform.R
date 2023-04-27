
setwd("/Users/ellali/Desktop")
#rm(list = ls())

source("Organized User-defined functions.R")

alpha <- 0.05
N <- 1e5
smpn <- c(5,6,7,8,10,12,15,20,25,30,40,50,75,100)
dfvec <- c(3,5,10,15,30)
dvec <- c(0, 0.5, 1)

set.seed(111)

altmatrixc <- power_normt_matrix <- normmatrix <-
  array(NA, dim = c(length(dvec), length(smpn), length(dfvec)))

##########################################################################################
##########################################################################################

################### We run a loop on d. d=0 ~ type I error, d > 0 power.

for (s in 1:length(dvec)){
  d <- dvec[s]
  for(i in 1:length(smpn)){
    print(c(s,i))
    n <- smpn[i]
    for(k in 1:length(dfvec)){
      df <- dfvec[k]
      rejectH0_alt <- rejectH0_normtest <- rejectH0_norm <- numeric(N)
      for(j in 1:N){
        x <- runif(n,min = 0 , max = df)####mean=df/2
        y <- (x-df/2)/sqrt((df^2)/12) #####var=(df)^2/12
        z <- y + d
        
        x1 <- runif(n,min = 0 , max = df)####mean=df/2
        y1 <- (x1-df/2)/sqrt((df^2)/12) #####var=(df)^2/12
        z1 <- y1 + d
        
        out <- t.test(z, z1)
        pval <- out$p.value
        if(pval <= alpha){
          rejectH0_alt[j] <- 1
        }
        
        out <- shapiro.test(z)
        pval <- out$p.value
        if(pval < alpha){
          rejectH0_normtest[j] <- 1
        }
        
        y <- rnorm(n, mean = 0, sd = 1)
        z <- y + d
        y1 <- rnorm(n, mean = 0, sd = 1)
        z1 <- y1 + d
        out <- t.test(z, z1)
        pval <- out$p.value
        if(pval <= alpha){
          rejectH0_norm[j] <- 1
        }
      }
      altmatrixc[s,i,k] <- mean(rejectH0_alt)
      power_normt_matrix[s,i,k] <- mean(rejectH0_normtest)
      normmatrix[s,i,k] <- mean(rejectH0_norm)
    }
  }
}


save.image(paste0("Twosampt-Uniform-TypeI-Power",".RData"))


##################################################################################

library(Cairo)

##################################################################################
#Type I Error Rate
#Adverse effect in downstream test
Cairo(file = "unifplot-TypeI.pdf",typ = "pdf",dpi=95,
      height = 1100, width = 1100)
par(mfrow = c(2,1))

plot(smpn, altmatrixc[1,,1], typ = "l", ylim = c(0.02, 0.08),
     xlab = "Sample size", ylab = "P(Type-I Error)", col = 1,
     main = "Adverse effect in downstream test")
for (j in 2:length(dfvec)){
  lines(smpn, altmatrixc[1,,j], col = j)
}
abline(h = 0.05, lty = 2, lwd = 0.6)
legend("topright", 
       legend = c("Uniform(0, 3)", "Uniform(0, 5)","Uniform(0, 10)",
                  "Uniform(0, 15)","Uniform(0, 30)"),
       lty = 1, col = c(1:5), cex = 0.7)

#Utility of normality test - Type-I error
j <- 1
type1inf <- altmatrixc[1,,j] - 0.05
plot(power_normt_matrix[1,,j], type1inf, typ = "l", col = 1,
     ylab = "Inflation of Type-I error", 
     xlab = "Power of normality test",
     ylim = c(-0.01, 0.05), xlim = c(0.05, 1),
     main = "Utility of normality test")
for (j in 2:length(dfvec)){
  type1inf <- altmatrixc[1,,j] - 0.05
  lines(power_normt_matrix[1,,j], type1inf, typ = "l", col = j,
        ylab = "Inflation of Type-I error")
}
abline(h = 0, lty = 2, lwd = 0.6)
legend("topright", 
       legend = c("Uniform(0, 3)", "Uniform(0, 5)","Uniform(0, 10)",
                  "Uniform(0, 15)","Uniform(0, 30)"),
       lty = 1, col = c(1:5), cex = 0.7)
dev.off()

#################################################################################
#Power-d=0.5
Cairo(file = "unifplot-Power-0.5.pdf",typ = "pdf",dpi=95,
      height = 1100, width = 1100)
par(mfrow = c(2,1))
#Adverse effect in downstream test
plot(smpn, altmatrixc[2,,1], typ = "l", ylim = c(0, 0.1), col = 2,
     ylab = "Power",
     xlab = "Sample size", main = "Adverse effect in downstream test")
for (j in 2:length(dfvec)){
  lines(smpn, altmatrixc[2,,j], col = j+1)
}
lines(smpn, apply(normmatrix[2,,], 1, mean), lty = 2, col = "black")
legend("topright", 
       legend = c("Uniform(0, 3)", "Uniform(0, 5)","Uniform(0, 10)",
                  "Uniform(0, 15)","Uniform(0, 30)","Normal"),
       lty = c(1,1,1,1,1,2), col = c(2:6, "black"), cex = 0.7)

#Utility of normality test - Power
j <- 1
powerloss <- apply(normmatrix[2,,], 1, mean) - altmatrixc[2,,j]
plot(power_normt_matrix[2,,j], powerloss, typ = "l", col = 2,
     ylab = "Loss of power", 
     xlab = "Power of normality test",
     ylim = c(-0.01, 0.05), xlim = c(0.05, 1),
     main = "Utility of normality test")
for (j in 2:length(dfvec)){
  powerloss <- apply(normmatrix[2,,], 1, mean) - altmatrixc[2,,j]
  lines(power_normt_matrix[2,,j], powerloss, typ = "l", col = j+1)
}
abline(h = 0, lty = 2, lwd = 0.6)
legend("topright", 
       legend = c("Uniform(0, 3)", "Uniform(0, 5)","Uniform(0, 10)",
                  "Uniform(0, 15)","Uniform(0, 30)"),
       lty = 1, col = c(2:6), cex = 0.7)
dev.off()

####################################################################################
#Power-d=1
Cairo(file = "unifplot-Power-1.pdf",typ = "pdf",dpi=95,
      height = 1100, width = 1100)
par(mfrow = c(2,1))
#Adverse effect in downstream test
plot(smpn, altmatrixc[3,,1], typ = "l", col = 2, ylim = c(0.045,0.055),
     ylab = "Power",
     xlab = "Sample size", main = "Adverse effect in downstream test")
for (j in 2:length(dfvec)){
  lines(smpn, altmatrixc[3,,j], col = j+1)
}
lines(smpn, apply(normmatrix[3,,], 1, mean), lty = 2, col = "black")
legend("topright", 
       legend = c("Uniform(0, 3)", "Uniform(0, 5)","Uniform(0, 10)",
                  "Uniform(0, 15)","Uniform(0, 30)","Normal"),
       lty = c(1,1,1,1,1,2), col = c(2:6, "black"), cex = 0.7)

#Utility of normality test - Power
j <- 1
powerloss <- apply(normmatrix[3,,], 1, mean) - altmatrixc[3,,j]
plot(power_normt_matrix[3,,j], powerloss, typ = "l", col = 2,
     ylab = "Loss of power", 
     xlab = "Power of normality test",
     ylim = c(-0.03, 0.05), xlim = c(0.05, 1),
     main = "Utility of normality test")
for (j in 2:length(dfvec)){
  powerloss <- apply(normmatrix[3,,], 1, mean) - altmatrixc[3,,j]
  lines(power_normt_matrix[3,,j], powerloss, typ = "l", col = j+1)
}
abline(h = 0, lty = 2, lwd = 0.6)
legend("topright", 
       legend = c("Uniform(0, 3)", "Uniform(0, 5)","Uniform(0, 10)",
                  "Uniform(0, 15)","Uniform(0, 30)"),
       lty = 1, col = c(2:6), cex = 0.7)
dev.off()



