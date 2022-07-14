
library("nortest")
library("dgof")
library("dplyr")
library(moments)
rm(list=ls())

dist_sum <- c("Normal", "Standard Normal", "Chi-square", "Gama", "Beta","Expotential", "t", "Uniform")
nvec <- c(8,9,10,11,12,13,14,15,20,25,30,35,40,45,50,75,100,150,175,200)
testvec <- c("KS", "SW", "JB", "DAP")

N <- 1000
alpha <- 0.05

powervec <- numeric(length(nvec)*length(dist_sum)*length(testvec))
powerarr <- array(powervec, dim = c(20, 8, 4))


for(i in 1:length(dist_sum)){
  dist <- dist_sum[i]
  for(j in 1:length(nvec)){
    n <- nvec[j]
    for(z in 1:length(testvec)){
      test <- testvec[z]
      rejectNorm <- numeric(N)
      for(k in 1:N){
        x <- generate_data(n,dist, 10, 2)
        output <- generate_tests(x, test)
        pval <- output$p.value
        if(pval < alpha){
          rejectNorm[k] <- 1
        }
      }
      powerarr[j,z,i] <- mean(rejectNorm)
    }
  }
}
powerarr







