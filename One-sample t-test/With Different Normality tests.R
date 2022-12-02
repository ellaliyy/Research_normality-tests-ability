#WIth Different Normality tests
setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")
#rm(list = ls())
alpha <- 0.05
N <- 10000
smpn <- c(10,11,12,13,15,17,20,22,25,30,35,40,45,50,100)
dfvec <- c(1,2,5,10,15,30)
normTvec <- c("KS","SW","JB","DAP")

set.seed(225)

alt <- power_normtest <- norm <- numeric(length(smpn)*length(dfvec)*length(normTvec))
altarray <- array(alt,dim = c(15,6,4))
power_normt_array <- array(power_normtest, dim = c(15,6,4))
normarray <- array(norm, dim = c(15,6,4))
dimnames(altarray) <- dimnames(power_normt_array) <- dimnames(normarray) <- list(smpn,dfvec,normTvec)

d <- 0
##########################################################################################
##########################################################################################

for(i in 1:length(smpn)){
  print(i)
  n <- smpn[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    for(l in 1:length(normTvec)){
      test <- normTvec[l]
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
        
        out <- generate_tests(z,test)
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

difarray <- altarray - normarray

######KS.Test
#Adverse effect in downstream test
plot(smpn,normarray[,1,1], 
     type = "l", ylim = c(0, 0.2),lty=1,
     ylab = 'Type I error rate',
     xlab = 'sample size',
     main = 'KS test---Chi-square---Adverse effect in downstream test')
lines(smpn, altarray[,1,1], col = "red", lty = 5)#df=1
lines(smpn, altarray[,3,1], col = "green",lty=6)#df=5
lines(smpn, altarray[,4,1], col = "blue",lty=7)#df=10
lines(smpn, altarray[,6,1], col = "gold",lty=8)#df=30
legend("topright", legend=c("normal","df=1", 'df=5','df=10','df=30'),
       col=c("black","red", "green",'blue','gold'), 
       lty = c(1,5:8), cex=0.5)

#Utility of normality test
plot(difarray[,1,1]~power_normt_array[,1,1], 
     type = "l", lty=5,col = "red",ylim = c(-0.05, 0.15),
     xlab = 'Power of normality test',
     ylab = 'Inflation of Type-I error',
     main = 'KS test---Chi-square---Utility of normality test')#df=1
lines(difarray[,2,1]~power_normt_array[,2,1], lty=6,col = "green")#df=2
lines(difarray[,3,1]~power_normt_array[,3,1], lty=7,col = "blue")#df=5
legend("topright", legend=c("df=1", 'df=2','df=5'),
       col=c("red", "green",'blue'), lty = c(5:7),cex=0.5)
abline(h = 0, col = "black", lty = 2)


######SW.Test
#Adverse effect in downstream test
plot(smpn,normarray[,1,2], 
     type = "l", ylim = c(0, 0.2),lty=1,
     ylab = 'Type I error rate',
     xlab = 'sample size',
     main = 'SW test---Chi-square---Adverse effect in downstream test')
lines(smpn, altarray[,1,2], col = "red", lty = 5)#df=1
lines(smpn, altarray[,3,2], col = "green",lty=6)#df=5
lines(smpn, altarray[,4,2], col = "blue",lty=7)#df=10
lines(smpn, altarray[,6,2], col = "gold",lty=8)#df=30
legend("topright", legend=c("normal","df=1", 'df=5','df=10','df=30'),
       col=c("black","red", "green",'blue','gold'), 
       lty = c(1,5:8), cex=0.5)

#Utility of normality test
plot(difarray[,1,2]~power_normt_array[,1,2], 
     type = "l", lty=5,col = "red",ylim = c(-0.05, 0.15),
     xlab = 'Power of normality test',
     ylab = 'Inflation of Type-I error',
     main = 'SW test---Chi-square---Utility of normality test')#df=1
lines(difarray[,2,2]~power_normt_array[,2,2], lty=6,col = "green")#df=2
lines(difarray[,3,2]~power_normt_array[,3,2], lty=7,col = "blue")#df=5
legend("topright", legend=c("df=1", 'df=2','df=5'),
       col=c("red", "green",'blue'), lty = c(5:7),cex=0.5)
abline(h = 0, col = "black", lty = 2)

######JB.Test
#Adverse effect in downstream test
plot(smpn,normarray[,1,3], 
     type = "l", ylim = c(0, 0.2),lty=1,
     ylab = 'Type I error rate',
     xlab = 'sample size',
     main = 'JB test---Chi-square---Adverse effect in downstream test')
lines(smpn, altarray[,1,3], col = "red", lty = 5)#df=1
lines(smpn, altarray[,3,3], col = "green",lty=6)#df=5
lines(smpn, altarray[,4,3], col = "blue",lty=7)#df=10
lines(smpn, altarray[,6,3], col = "gold",lty=8)#df=30
legend("topright", legend=c("normal","df=1", 'df=5','df=10','df=30'),
       col=c("black","red", "green",'blue','gold'), 
       lty = c(1,5:8), cex=0.5)

#Utility of normality test
plot(difarray[,1,3]~power_normt_array[,1,3], 
     type = "l", lty=5,col = "red",ylim = c(-0.05, 0.15),
     xlab = 'Power of normality test',
     ylab = 'Inflation of Type-I error',
     main = 'JB test---Chi-square---Utility of normality test')#df=1
lines(difarray[,2,3]~power_normt_array[,2,3], lty=6,col = "green")#df=2
lines(difarray[,3,3]~power_normt_array[,3,3], lty=7,col = "blue")#df=5
legend("topright", legend=c("df=1", 'df=2','df=5'),
       col=c("red", "green",'blue'), lty = c(5:7),cex=0.5)
abline(h = 0, col = "black", lty = 2)


######DAP.Test
#Adverse effect in downstream test
plot(smpn,normarray[,1,4], 
     type = "l", ylim = c(0, 0.2),lty=1,
     ylab = 'Type I error rate',
     xlab = 'sample size',
     main = 'DAP test---Chi-square---Adverse effect in downstream test')
lines(smpn, altarray[,1,4], col = "red", lty = 5)#df=1
lines(smpn, altarray[,3,4], col = "green",lty=6)#df=5
lines(smpn, altarray[,4,4], col = "blue",lty=7)#df=10
lines(smpn, altarray[,6,4], col = "gold",lty=8)#df=30
legend("topright", legend=c("normal","df=1", 'df=5','df=10','df=30'),
       col=c("black","red", "green",'blue','gold'), 
       lty = c(1,5:8), cex=0.5)

#Utility of normality test
plot(difarray[,1,4]~power_normt_array[,1,4], 
     type = "l", lty=5,col = "red",ylim = c(-0.05, 0.15),
     xlab = 'Power of normality test',
     ylab = 'Inflation of Type-I error',
     main = 'DAP test---Chi-square---Utility of normality test')#df=1
lines(difarray[,2,4]~power_normt_array[,2,4], lty=6,col = "green")#df=2
lines(difarray[,3,4]~power_normt_array[,3,4], lty=7,col = "blue")#df=5
legend("topright", legend=c("df=1", 'df=2','df=5'),
       col=c("red", "green",'blue'), lty = c(5:7),cex=0.5)
abline(h = 0, col = "black", lty = 2)

save.image(paste0("With Different Normality tests",".RData"))
