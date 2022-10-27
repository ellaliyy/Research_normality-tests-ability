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
