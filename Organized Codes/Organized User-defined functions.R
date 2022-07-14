
#Organized TypeI Error Test

#Same Mean
alpha <- 0.05
N <- 1000
rejectH0 <- pval <-  numeric(N)

nvec <- c(5,10,15,20,25)
dfvec <- c(3,5,8,10,15)
powervec <- numeric(length(nvec)*length(dfvec))

#matrix instead[i,k]
m <- 1

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    powermatrixs <- matrix(powervec,
                          nrow=length(nvec),ncol=length(dfvec),byrow=TRUE)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square",df)
      y <- generate_data(n,"Chi-square",df)
      out <- t.test(x,y)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixs[i,k] <- mean(rejectH0)
  }
}
powermatrixs


#Different Mean
for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    powermatrixd <- matrix(powervec,
                          nrow=length(nvec),ncol=length(dfvec),byrow=TRUE)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square",df)
      y <- generate_data(n,"Chi-square",20)
      out <- t.test(x,y)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrixd[i,k] <- mean(rejectH0)
  }
}
powermatrixd
