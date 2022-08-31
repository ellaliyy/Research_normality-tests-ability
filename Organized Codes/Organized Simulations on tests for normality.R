
library("nortest")
library("dgof")
library("dplyr")
library(moments)

setwd("/Users/echo/Desktop")
source("Organized User-defined functions.R")

dist_sum <- c("Normal", "Standard Normal", "Chi-square", "Gamma", "Beta","Expotential", "t", "Uniform")
nvec <- c(8,9,10,11,12,13,14,15,20,25,30,35,40,45,50,75,100,150,175,200)
testvec <- c("KS", "SW", "JB", "DAP")

N <- 100
alpha <- 0.05

powervec <- numeric(length(nvec)*length(testvec)*length(dist_sum))
powerarr <- array(powervec, dim = c(20, 4, 8))

set.seed(12)

system.time({

for(i in 1:length(dist_sum)){
  dist <- dist_sum[i]
  for(j in 1:length(nvec)){
    n <- nvec[j]
    for(z in 1:length(testvec)){
      test <- testvec[z]
      rejectNorm <- numeric(N)
      for(k in 1:N){
        if (floor(k/10000) == k/10000){
          print(paste0(k, " sims complete for ", i, "th distribution, ",
                       z, "th test", " and ", j, "th sample size"))
        }
        x <- generate_data(n,dist, 10, 2) #Here not fix yet--using same parameters here can not simulate similiar data from different distributions
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
  
})
powerarr







