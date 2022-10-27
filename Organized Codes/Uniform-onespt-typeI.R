setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")
#rm(list = ls())
alpha <- 0.05
N <- 10000
smpn <- c(5,6,7,8,9,10,11,12,13,15,17,20,22,25,30,35,40,45,50,100)
dfvec <- c(3,5,10,15,30)
d <- 0

set.seed(115)

alt <- power_normtest <- norm <- numeric(length(smpn)*length(dfvec))
altmatrixu <- matrix(alt,
                    nrow=length(smpn),
                    ncol=length(dfvec),byrow=TRUE)
power_normt_matrix <- matrix(power_normtest,
                             nrow=length(smpn),
                             ncol=length(dfvec),byrow=TRUE)
normmatrix <- matrix(norm,
                     nrow=length(smpn),
                     ncol=length(dfvec),byrow=TRUE)

row.names(altmatrixu) <- row.names(power_normt_matrix) <- row.names(normmatrix) <- smpn
colnames(altmatrixu) <- colnames(power_normt_matrix) <- colnames(normmatrix) <- dfvec
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
      x <- runif(n,min = 0 , max = df)####mean=df/2
      y <- x - df/2 + d
      z <- y/sqrt((df^2)/12)#####var=(df)^2/12
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
      
      x <- rnorm(n, mean = df/2, sd = sqrt((df)^2/12))
      y <- x - df/2 + d
      out <- t.test(y/sqrt((df)^2/12))
      pval <- out$p.value
      if(pval <= alpha){
        rejectH0_norm[j] <- 1
      }
    }
    altmatrixu[i,k] <- mean(rejectH0_alt)
    power_normt_matrix[i,k] <- mean(rejectH0_normtest)
    normmatrix[i,k] <- mean(rejectH0_norm)
  }
}


plot(smpn,normmatrix[,1], 
     type = "l", ylim = c(0, 0.2),lty=1,
     ylab = 'Type I error rate',
     xlab = 'sample size',
     main = 'Uniform---TypeI error rate inflation under different df')
lines(smpn, altmatrixu[,1], col = "red", lty = 5)
lines(smpn, altmatrixu[,2], col = "green",lty=6)
lines(smpn, altmatrixu[,3], col = "blue",lty=7)
lines(smpn, altmatrixu[,4], col = "gold",lty=8)
lines(smpn, altmatrixu[,5], col = "purple",lty=9)
legend("topright", legend=c("normal","(0,3)",'(0,5)','(0,10)','(0,15)','(0,30)'),
       col=c("black","red", "green",'blue','gold','purple'), lty = c(1,5:9), cex=0.5)

save.image(paste0("Uniform-Onesampt-TypeI",".RData"))


