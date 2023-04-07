#Paper/Literature Review
rm(list = ls())
library("nortest")
library("dgof")
library("dplyr")
library(moments)
setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")

dist_sum <- c("Normal", "Standard Normal","Gamma","Expotential", "t","Beta","Chi-square","Uniform")
nvec <- c(8,9,10,11,12,13,14,15,20,25,30,35,40,45,50,75,100,150,175,200)
testvec <- c("KS", "SW","DAP","JB")

N <- 1000
alpha <- 0.05

powervec <- numeric(length(nvec)*length(dist_sum)*length(testvec))
powerarr <- array(powervec, dim = c(20,8, 4), dimnames = list(nvec, dist_sum,testvec))


set.seed(12)

system.time({
  
  for(i in 1:length(nvec)){
    n <- nvec[i]
  
      for(j in 1:length(dist_sum)){
        dist <- dist_sum[j]
        
        for(z in 1:length(testvec)){
          test <- testvec[z]
          rejectNorm <- numeric(N)
          
        for(k in 1:N){
          if (floor(k/1000) == k/1000){
            print(paste0(k, " sims complete for ", i, "th sample size, ",
                         j, "th distribution", " and ", z, "th test"))
          }
          x <- data_simulate(n,dist)
          output <- generate_tests(x, test)
          pval <- output$p.value
          if(pval < alpha){
            rejectNorm[k] <- 1
          }
        }
        powerarr[i,j,z] <- mean(rejectNorm)*100
      }
    }
  }
  
})

powerarr
d <- as.data.frame(powerarr)

#export data into excel
library("writexl")
write_xlsx(d,"/Users/ellali/Desktop/Book1.xlsx")

save.image(paste0("PaperReview",".RData"))




