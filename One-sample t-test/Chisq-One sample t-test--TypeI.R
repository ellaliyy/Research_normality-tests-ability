setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")
#rm(list = ls())
alpha <- 0.05
N <- 10000
smpn <- c(5,6,7,8,9,10,11,12,13,15,17,20,22,25,30,35,40,45,50,100)
dfvec <- c(1,2,5,10,15,30)
d <- 0

set.seed(111)

alt <- power_normtest <- norm <- numeric(length(smpn)*length(dfvec))
altmatrixc <- matrix(alt,
                    nrow=length(smpn),
                    ncol=length(dfvec),byrow=TRUE)
power_normt_matrix <- matrix(power_normtest,
                    nrow=length(smpn),
                    ncol=length(dfvec),byrow=TRUE)
normmatrix <- matrix(norm,
                     nrow=length(smpn),
                     ncol=length(dfvec),byrow=TRUE)

row.names(altmatrixc) <- row.names(power_normt_matrix) <- row.names(normmatrix) <- smpn
colnames(altmatrixc) <- colnames(power_normt_matrix) <- colnames(normmatrix) <- dfvec
##########################################################################################
##########################################################################################

###################d=0 ~ type I error
for(i in 1:length(smpn)){
  print(i)
  n <- smpn[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
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
        
        x <- rnorm(n, mean = df, sd = sqrt(2*df))
        y <- x - df + d
        out <- t.test(y/sqrt(2*df))
        pval <- out$p.value
        if(pval <= alpha){
          rejectH0_norm[j] <- 1
        }
    }
    altmatrixc[i,k] <- mean(rejectH0_alt)
    power_normt_matrix[i,k] <- mean(rejectH0_normtest)
    normmatrix[i,k] <- mean(rejectH0_norm)
  }
}

#Size(TypeI error) comparison of normal (black) vs chisq (red) data
####Seperate plots for different df
#df=1
plot(smpn,normmatrix[,1], 
     type = "l", ylim = c(0, 0.2),
     ylab = 'Type I error rate',
     xlab = 'sample size',
     main = 'df=0.5')
lines(smpn, altmatrixc[,1], col = "red")
#df=2
plot(smpn,normmatrix[,2], 
     type = "l", ylim = c(0, 0.2),
     ylab = 'Type I error rate',
     main = 'df=1')
lines(smpn, altmatrixc[,2], col = "red")
#df=5
plot(smpn,normmatrix[,3], 
     type = "l", ylim = c(0, 0.2),
     ylab = 'Type I error rate',
     main = 'df=1.5')
lines(smpn, altmatrixc[,3], col = "red")
#df=10
plot(smpn,normmatrix[,4], 
     type = "l", ylim = c(0, 0.2),
     ylab = 'Type I error rate',
     main = 'df=2')
lines(smpn, altmatrixc[,4], col = "red")
#df=15
plot(smpn,normmatrix[,5], 
     type = "l", ylim = c(0, 0.2),
     ylab = 'Type I error rate',
     main = 'df=5')
lines(smpn, altmatrixc[,5], col = "red")
#df=30
plot(smpn,normmatrix[,6], 
     type = "l", ylim = c(0, 0.2),
     ylab = 'Type I error rate',
     main = 'df=10')
lines(smpn, altmatrixc[,6], col = "red")

#Organized plot for df in [1,30]
plot(smpn,normmatrix[,1], 
     type = "l", ylim = c(0, 0.2),lty=1,
     ylab = 'Type I error rate',
     xlab = 'sample size',
     main = 'Chi-square----TypeI error rate inflation under different df')
lines(smpn, altmatrixc[,1], col = "red", lty = 5)
lines(smpn, altmatrixc[,2], col = "green",lty=6)
lines(smpn, altmatrixc[,3], col = "blue",lty=7)
lines(smpn, altmatrixc[,4], col = "gold",lty=8)
lines(smpn, altmatrixc[,5], col = "purple",lty=9)
lines(smpn, altmatrixc[,6], col = "pink",lty=10)
legend("topright", legend=c("normal","df=1", "df=2", 'df=5','df=10','df=15','df=30'),
       col=c("black","red", "green",'blue','gold','purple','pink'), lty = c(1,5:10), cex=0.5)
###Observation: 
#1.as the increase in df, the degree of inflation of typeI error rate is decreasing.
#2.Generally, as the increase in the sample size, the degree of inflation of typeI error rate is decreasing.


#normtest power vs Type-I error inflation
#df=0.5
plot(power_normt_matrix[,1], altmatrixc[,1]-normmatrix[,1],
      pch = 15, ylim = c(0, 0.3),col = 'red',
     main="Normality test power vs TypeI error inflation")
#df=1
points(power_normt_matrix[,2], altmatrixc[,2]-normmatrix[,2],
     pch = 16, ylim = c(0, 0.3), col = 'green')
#df=1.5
points(power_normt_matrix[,3], altmatrixc[,3]-normmatrix[,3],
     pch = 17, ylim = c(0, 0.3),col='blue')
#df=2
points(power_normt_matrix[,4], altmatrixc[,4]-normmatrix[,4],
     pch = 18, ylim = c(0, 0.3),col='gold')
#df=5
points(power_normt_matrix[,5], altmatrixc[,5]-normmatrix[,5],
       pch = 19, ylim = c(0, 0.3),col='purple')
abline(h=0)
legend("topright", legend=c("df=0.5", "df=1", 'df=1.5','df=2','df=5'),
       col=c("red", "green",'blue','gold','purple'), pch = c(15:19),cex = 0.8)

###Observation: 
#1.as the increase in df, the difference of the typeI error rate between Chi-square and normal is decreasing.
#2.As the increase in the power of normality test(increase in the sample size),
#the typeI error inflation is decreasing and approaching to 0.

#########################################################################################
#########################################################################################

save.image(paste0("Chisq-Onesampt-TypeI",".RData"))











