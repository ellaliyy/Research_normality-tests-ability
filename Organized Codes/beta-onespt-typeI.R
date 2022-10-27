setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")
#rm(list = ls())
alpha <- 0.05
N <- 10000
smpn <- c(5,6,7,8,9,10,11,12,13,15,17,20,22,25,30,35,40,45,50,100)
dfvec <- c(4,6,8,10,20,30)
d <- 0

set.seed(113)

alt <- power_normtest <- norm <- numeric(length(smpn)*length(dfvec))
altmatrixb <- matrix(alt,
                    nrow=length(smpn),
                    ncol=length(dfvec),byrow=TRUE)
power_normt_matrix <- matrix(power_normtest,
                             nrow=length(smpn),
                             ncol=length(dfvec),byrow=TRUE)
normmatrix <- matrix(norm,
                     nrow=length(smpn),
                     ncol=length(dfvec),byrow=TRUE)

row.names(altmatrixb) <- row.names(power_normt_matrix) <- row.names(normmatrix) <- smpn
colnames(altmatrixb) <- colnames(power_normt_matrix) <- colnames(normmatrix) <- dfvec
##########################################################################################
##########################################################################################

#I set the second parameter equals to half of the first 
#in order to make the beta distribution negatively-skewed.

###################d=0 ~ type I error
for(i in 1:length(smpn)){
  print(i)
  n <- smpn[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    rejectH0_alt <- rejectH0_normtest <- rejectH0_norm <- numeric(N)
    for(j in 1:N){
      x <- rbeta(n,shape1 = df,shape2 = df/2)#mean=df/(df+df/2)
      y <- x - (df/(df+df/2)) + d
      z <- y/sqrt(4/(9*(3*df+2)))##############var=(df*df/2)/(df+df/2)^2*(df+df/2+1)
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
      
      x <- rnorm(n, mean = df/(df+df/2), sd = sqrt(4/(9*(3*df+2))))
      y <- x - df/(df+df/2) + d
      out <- t.test(y/sqrt(4/(9*(3*df+2))))
      pval <- out$p.value
      if(pval <= alpha){
        rejectH0_norm[j] <- 1
      }
    }
    altmatrixb[i,k] <- mean(rejectH0_alt)
    power_normt_matrix[i,k] <- mean(rejectH0_normtest)
    normmatrix[i,k] <- mean(rejectH0_norm)
  }
}


plot(smpn,normmatrix[,1], 
     type = "l", ylim = c(0, 0.2),lty=1,
     ylab = 'Type I error rate',
     xlab = 'sample size',
     main = 'Beta----TypeI error rate inflation under different df')
lines(smpn, altmatrixb[,1], col = "red", lty = 5)
lines(smpn, altmatrixb[,2], col = "green",lty=6)
lines(smpn, altmatrixb[,3], col = "blue",lty=7)
lines(smpn, altmatrixb[,4], col = "gold",lty=8)
lines(smpn, altmatrixb[,5], col = "purple",lty=9)
lines(smpn, altmatrixb[,6], col = "pink",lty=10)
legend("topright", legend=c("normal","(4,2)", "(6,3)", '(8,4)','(10,5)','(20,10)','(30,15'),
       col=c("black","red", "green",'blue','gold','purple','pink'), lty = c(1,5:10), cex=0.5)


save.image(paste0("Beta-Onesampt-TypeI",".RData"))








