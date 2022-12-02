#Power in One sample t-test
setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")
#rm(list = ls())
alpha <- 0.05
N <- 10000
smpn <- c(5,6,7,8,9,10,11,12,13,15,17,20,22,25,30,35,40,45,50,100)
dfvec <- c(1,2,5,10,15,30)
dvec <- c(0.5,1,1.5,2,2.5)

set.seed(222)

alt <- power_normtest <- norm <- numeric(length(smpn)*length(dfvec)*length(dvec))
altarray <- array(alt,dim = c(20,8,5))
power_normt_array <- array(power_normtest, dim = c(20,8,5))
normarray <- array(norm, dim = c(20,8,5))
dimnames(altarray) <- dimnames(power_normt_array) <- dimnames(normarray) <- list(smpn,dfvec,dvec)

##########################################################################################
##########################################################################################

for(i in 1:length(smpn)){
  print(i)
  n <- smpn[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    for(l in 1:length(dvec)){
      d <- dvec[l]
      rejectH0_alt <- rejectH0_normtest <- rejectH0_norm <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square", df)
      y <- x - df + d
      z <- y/sqrt(2*df)
      out <- t.test(z)
      pval <- out$p.value
      if(pval <= alpha){
        rejectH0_alt[j] <- 1
      }
      
      out <- shapiro.test(z)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0_normtest[j] <- 1
      }
      
      x <- rnorm(n, mean = d, sd = sqrt(2*df))
      out <- t.test(x/sqrt(2*df))
      pval <- out$p.value
      if(pval <= alpha){
        rejectH0_norm[j] <- 1
      }
    }
    altarray[i,k,l] <- mean(rejectH0_alt)
    power_normt_array[i,k,l] <- mean(rejectH0_normtest)
    normarray[i,k,l] <- mean(rejectH0_norm)
  }
  }
}

#Power comparison of normal (black) vs chisq (red) data
#d=0.5
####################################################
par(mfrow=c(2,4))
#df=0.5
plot(smpn,normarray[,1,1], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=0.5---df=0.5')
lines(smpn, altarray[,1,1], col = "red", lty = 5)
#df=1
plot(smpn,normarray[,2,1], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=0.5---df=1')
lines(smpn, altarray[,2,1], col = "red", lty = 5)
#df=1.5
plot(smpn,normarray[,3,1], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=0.5---df=1.5')
lines(smpn, altarray[,3,1], col = "red", lty = 5)
#df=2
plot(smpn,normarray[,4,1], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=0.5---df=2')
lines(smpn, altarray[,4,1], col = "red", lty = 5)
#df=5
plot(smpn,normarray[,5,1], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=0.5---df=5')
lines(smpn, altarray[,5,1], col = "red", lty = 5)
#df=10
plot(smpn,normarray[,6,1], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=0.5---df=10')
lines(smpn, altarray[,6,1], col = "red", lty = 5)
#df=15
plot(smpn,normarray[,7,1], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=0.5---df=15')
lines(smpn, altarray[,7,1], col = "red", lty = 5)
#df=30
plot(smpn,normarray[,8,1], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=0.5---df=30')
lines(smpn, altarray[,8,1], col = "red", lty = 5)

#d=1
####################################################
par(mfrow=c(2,4))
#df=0.5
plot(smpn,normarray[,1,2], 
    type = "l", ylim = c(0, 1),
    ylab = 'Power of One sample t-test',
    main = 'd=1---df=0.5')
lines(smpn, altarray[,1,2], col = "red", lty = 5)
#df=1
plot(smpn,normarray[,2,2], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1---df=1')
lines(smpn, altarray[,2,2], col = "red", lty = 5)
#df=1.5
plot(smpn,normarray[,3,2], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1---df=1.5')
lines(smpn, altarray[,3,2], col = "red", lty = 5)
#df=2
plot(smpn,normarray[,4,2], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1---df=2')
lines(smpn, altarray[,4,2], col = "red", lty = 5)
#df=5
plot(smpn,normarray[,5,2], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1---df=5')
lines(smpn, altarray[,5,2], col = "red", lty = 5)
#df=10
plot(smpn,normarray[,6,2], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1---df=10')
lines(smpn, altarray[,6,2], col = "red", lty = 5)
#df=15
plot(smpn,normarray[,7,2], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1---df=15')
lines(smpn, altarray[,7,2], col = "red", lty = 5)
#df=30
plot(smpn,normarray[,8,2], 
    type = "l", ylim = c(0, 1),
    ylab = 'Power of One sample t-test',
    main = 'd=1---df=30')
lines(smpn, altarray[,8,2], col = "red", lty = 5)

#d=1.5
####################################################
par(mfrow=c(2,4))
#df=0.5
plot(smpn,normarray[,1,3], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1.5---df=0.5')
lines(smpn, altarray[,1,3], col = "red", lty = 5)
#df=1
plot(smpn,normarray[,2,3], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1.5---df=1')
lines(smpn, altarray[,2,3], col = "red", lty = 5)
#df=1.5
plot(smpn,normarray[,3,3], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1.5---df=1.5')
lines(smpn, altarray[,3,3], col = "red", lty = 5)
#df=2
plot(smpn,normarray[,4,3], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1.5---df=2')
lines(smpn, altarray[,4,3], col = "red", lty = 5)
#df=5
plot(smpn,normarray[,5,3], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1.5---df=5')
lines(smpn, altarray[,5,3], col = "red", lty = 5)
#df=10
plot(smpn,normarray[,6,3], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1.5---df=10')
lines(smpn, altarray[,6,3], col = "red", lty = 5)
#df=15
plot(smpn,normarray[,7,3], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1.5---df=15')
lines(smpn, altarray[,7,3], col = "red", lty = 5)
#df=30
plot(smpn,normarray[,8,3], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=1.5---df=30')
lines(smpn, altarray[,8,3], col = "red", lty = 5)

#d=2
####################################################
par(mfrow=c(2,4))
#df=0.5
plot(smpn,normarray[,1,4], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2---df=0.5')
lines(smpn, altarray[,1,4], col = "red", lty = 5)
#df=1
plot(smpn,normarray[,2,4], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2---df=1')
lines(smpn, altarray[,2,4], col = "red", lty = 5)
#df=1.5
plot(smpn,normarray[,3,4], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2---df=1.5')
lines(smpn, altarray[,3,4], col = "red", lty = 5)
#df=2
plot(smpn,normarray[,4,4], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2---df=2')
lines(smpn, altarray[,4,4], col = "red", lty = 5)
#df=5
plot(smpn,normarray[,5,4], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2---df=5')
lines(smpn, altarray[,5,4], col = "red", lty = 5)
#df=10
plot(smpn,normarray[,6,4], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2---df=10')
lines(smpn, altarray[,6,4], col = "red", lty = 5)
#df=15
plot(smpn,normarray[,7,4], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2---df=15')
lines(smpn, altarray[,7,4], col = "red", lty = 5)
#df=30
plot(smpn,normarray[,8,4], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2---df=30')
lines(smpn, altarray[,8,4], col = "red", lty = 5)

#d=2.5
####################################################
par(mfrow=c(2,4))
#df=0.5
plot(smpn,normarray[,1,5], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2.5---df=0.5')
lines(smpn, altarray[,1,5], col = "red", lty = 5)
#df=1
plot(smpn,normarray[,2,5], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2.5---df=1')
lines(smpn, altarray[,2,5], col = "red", lty = 5)
#df=1.5
plot(smpn,normarray[,3,5], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2.5---df=1.5')
lines(smpn, altarray[,3,5], col = "red", lty = 5)
#df=2
plot(smpn,normarray[,4,5], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2.5---df=2')
lines(smpn, altarray[,4,5], col = "red", lty = 5)
#df=5
plot(smpn,normarray[,5,5], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2.5---df=5')
lines(smpn, altarray[,5,5], col = "red", lty = 5)
#df=10
plot(smpn,normarray[,6,5], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2.5---df=10')
lines(smpn, altarray[,6,5], col = "red", lty = 5)
#df=15
plot(smpn,normarray[,7,5], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2.5---df=15')
lines(smpn, altarray[,7,5], col = "red", lty = 5)
#df=30
plot(smpn,normarray[,8,5], 
     type = "l", ylim = c(0, 1),
     ylab = 'Power of One sample t-test',
     main = 'd=2.5---df=30')
lines(smpn, altarray[,8,5], col = "red", lty = 5)


#normtest power vs Power loss
#d=0.5
####################################################
plot(power_normt_array[,1,1],normarray[,1,1]-altarray[,1,1], 
     type = "l", ylim = c(-0.4, 0.4),
     ylab = 'Power_norm - Power_alt',
     xlab = 'Power of normality test',
     col = 'red',
     main="Normality test power vs Power Loss----d=0.5", lty=5)
lines(power_normt_array[,2,1],normarray[,2,1]-altarray[,2,1], 
      col = 'green',lty=6)
lines(power_normt_array[,3,1],normarray[,3,1]-altarray[,3,1], 
      col = 'blue',lty=7)
lines(power_normt_array[,4,1],normarray[,4,1]-altarray[,4,1], 
      col = 'gold',lty=8)
lines(power_normt_array[,5,1],normarray[,5,1]-altarray[,5,1], 
      col = 'purple',lty=9)
lines(power_normt_array[,6,1],normarray[,6,1]-altarray[,6,1], 
      col = 'orange',lty=10)
lines(power_normt_array[,7,1],normarray[,7,1]-altarray[,7,1], 
      col = 'brown',lty=11)
lines(power_normt_array[,8,1],normarray[,8,1]-altarray[,8,1], 
      col = 'pink',lty=12)
abline(h=0)
legend("bottomleft", legend=c("df=0.5", "df=1", 'df=1.5','df=2','df=5','df=10',"df=15",'df=30'),
       col=c("red", "green",'blue','gold','purple','orange','brown','pink'), 
       lty = c(5:12),cex = 0.5)

#d=1
####################################################
plot(power_normt_array[,1,2],normarray[,1,2]-altarray[,1,2], 
     type = "l", ylim = c(-0.4, 0.4),
     ylab = 'Power_norm - Power_alt',
     xlab = 'Power of normality test',
     col = 'red',
     main="Normality test power vs Power Loss----d=1", lty=5)
lines(power_normt_array[,2,2],normarray[,2,2]-altarray[,2,2], 
      col = 'green',lty=6)
lines(power_normt_array[,3,2],normarray[,3,2]-altarray[,3,2], 
      col = 'blue',lty=7)
lines(power_normt_array[,4,2],normarray[,4,2]-altarray[,4,2], 
      col = 'gold',lty=8)
lines(power_normt_array[,5,2],normarray[,5,2]-altarray[,5,2], 
      col = 'purple',lty=9)
lines(power_normt_array[,6,2],normarray[,6,2]-altarray[,6,2], 
      col = 'orange',lty=10)
lines(power_normt_array[,7,2],normarray[,7,2]-altarray[,7,2], 
      col = 'brown',lty=11)
lines(power_normt_array[,8,2],normarray[,8,2]-altarray[,8,2], 
      col = 'pink',lty=12)
abline(h=0)
legend("topright", legend=c("df=0.5", "df=1", 'df=1.5','df=2','df=5','df=10',"df=15",'df=30'),
       col=c("red", "green",'blue','gold','purple','orange','brown','pink'), 
       lty = c(5:12),cex = 0.5)

#d=1.5
####################################################
plot(power_normt_array[,1,3],normarray[,1,3]-altarray[,1,3], 
     type = "l", ylim = c(-0.4, 0.4),
     ylab = 'Power_norm - Power_alt',
     xlab = 'Power of normality test',
     col = 'red',
     main="Normality test power vs Power Loss----d=1.5", lty=5)
lines(power_normt_array[,2,3],normarray[,2,3]-altarray[,2,3], 
      col = 'green',lty=6)
lines(power_normt_array[,3,3],normarray[,3,3]-altarray[,3,3], 
      col = 'blue',lty=7)
lines(power_normt_array[,4,3],normarray[,4,3]-altarray[,4,3], 
      col = 'gold',lty=8)
lines(power_normt_array[,5,3],normarray[,5,3]-altarray[,5,3], 
      col = 'purple',lty=9)
lines(power_normt_array[,6,3],normarray[,6,3]-altarray[,6,3], 
      col = 'orange',lty=10)
lines(power_normt_array[,7,3],normarray[,7,3]-altarray[,7,3], 
      col = 'brown',lty=11)
lines(power_normt_array[,8,3],normarray[,8,3]-altarray[,8,3], 
      col = 'pink',lty=12)
abline(h=0)
legend("topright", legend=c("df=0.5", "df=1", 'df=1.5','df=2','df=5','df=10',"df=15",'df=30'),
       col=c("red", "green",'blue','gold','purple','orange','brown','pink'), 
       lty = c(5:12),cex = 0.5)

#d=2
####################################################
plot(power_normt_array[,1,4],normarray[,1,4]-altarray[,1,4], 
     type = "l", ylim = c(-0.4, 0.4),
     ylab = 'Power_norm - Power_alt',
     xlab = 'Power of normality test',
     col = 'red',
     main="Normality test power vs Power Loss----d=2", lty=5)
lines(power_normt_array[,2,4],normarray[,2,4]-altarray[,2,4], 
      col = 'green',lty=6)
lines(power_normt_array[,3,4],normarray[,3,4]-altarray[,3,4], 
      col = 'blue',lty=7)
lines(power_normt_array[,4,4],normarray[,4,4]-altarray[,4,4], 
      col = 'gold',lty=8)
lines(power_normt_array[,5,4],normarray[,5,4]-altarray[,5,4], 
      col = 'purple',lty=9)
lines(power_normt_array[,6,4],normarray[,6,4]-altarray[,6,4], 
      col = 'orange',lty=10)
lines(power_normt_array[,7,4],normarray[,7,4]-altarray[,7,4], 
      col = 'brown',lty=11)
lines(power_normt_array[,8,4],normarray[,8,4]-altarray[,8,4], 
      col = 'pink',lty=12)
abline(h=0)
legend("topright", legend=c("df=0.5", "df=1", 'df=1.5','df=2','df=5','df=10',"df=15",'df=30'),
       col=c("red", "green",'blue','gold','purple','orange','brown','pink'), 
       lty = c(5:12),cex = 0.5)

#d=2.5
####################################################
plot(power_normt_array[,1,5],normarray[,1,5]-altarray[,1,5], 
     type = "l", ylim = c(-0.4, 0.4),
     ylab = 'Power_norm - Power_alt',
     xlab = 'Power of normality test',
     col = 'red',
     main="Normality test power vs Power Loss----d=2.5", lty=5)
lines(power_normt_array[,2,5],normarray[,2,5]-altarray[,2,5], 
      col = 'green',lty=6)
lines(power_normt_array[,3,5],normarray[,3,5]-altarray[,3,5], 
      col = 'blue',lty=7)
lines(power_normt_array[,4,5],normarray[,4,5]-altarray[,4,5], 
      col = 'gold',lty=8)
lines(power_normt_array[,5,5],normarray[,5,5]-altarray[,5,5], 
      col = 'purple',lty=9)
lines(power_normt_array[,6,5],normarray[,6,5]-altarray[,6,5], 
      col = 'orange',lty=10)
lines(power_normt_array[,7,5],normarray[,7,5]-altarray[,7,5], 
      col = 'brown',lty=11)
lines(power_normt_array[,8,5],normarray[,8,5]-altarray[,8,5], 
      col = 'pink',lty=12)
abline(h=0)
legend("topright", legend=c("df=0.5", "df=1", 'df=1.5','df=2','df=5','df=10',"df=15",'df=30'),
       col=c("red", "green",'blue','gold','purple','orange','brown','pink'), 
       lty = c(5:12),cex = 0.5)

#######################################################################################
#######################################################################################

save.image(paste0("Power",".RData"))




