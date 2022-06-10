

library("nortest")
library("dgof")
library("dplyr")
library(moments)

dist_sum <- c("Normal", "Standard Normal", "Chi-square", "Gama", "Beta","Expotential", "t", "Uniform")
nvec <- c(8,9,10,11,12,13,14,15,20,25,30,35,40,45,50,75,100,150,175,200)
testvec <- c("KS", "SW", "JB", "DAP")

N <- 1000
alpha <- 0.05
rejectNorm <- numeric(N)


powervec <- numeric(length(nvec)*length(dist_sum)*length(testvec))
powermatrix <- matrix(powervec,nrow=length(dist_sum),
                      ncol1=length(nvec),
                      ncol2=length(testvec),
                      byrow=TRUE)

for(i in 1:length(dist_sum)){
  dist <- dist_sum[i]
  for(j in 1:length(nvec)){
    n <- nvec[j]
    for(z in 1:length(testvec)){
      test <- testvec[z]
      for(k in 1:N){
        x <- generate_data(n,dist)
        output <- generate_tests(x, test)
        pval <- output$p.value
        if(pval < alpha){
          rejectNorm[k] <- 1
        }
      }
      powermatrix[i,j,z] <- mean(rejectNorm)
    }
  }
}








