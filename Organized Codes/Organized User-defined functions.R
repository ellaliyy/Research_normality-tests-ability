library("nortest")
library("dgof")
library("dplyr")
library(moments)

#User-defined functions

#Generate data from different distribution
generate_data <- function(n, dist,par1, par2){
  if(dist == "Normal"){
    x <- rnorm(n, mean = par1, sd = par2)
  }
  if(dist == "Standard Normal"){ 
    #This becomes redundant when you have par1 and par2 above 
    x <- rnorm(n, mean = 0, sd = 1)
  }
  if(dist == "Chi-square"){
    x <- rchisq(n, par1) #may be set a default for par1?
  }
  if(dist == "Gamma"){
    x <- rgamma(n, shape = par1, rate = par2)
  }
  if(dist == "Beta"){
    x <- rbeta(n, shape1 = par1, shape2 = par2)
  }
  if(dist == "Expotential"){
    x <- rexp(n, rate = par1)
  }
  if(dist == "t"){
    x <- rt(n,df = par1) #may be set a default for par1?
  }
  if(dist == "Uniform"){
    x <- runif(n, min = par1,max = par2)
  }
  
  return(x)
}

#Apply different tests
generate_tests <- function(x, test){
  if(test == "KS"){
    output <- lillie.test(x)
  }
  if(test == "SW"){
    output <- shapiro.test(x)
  }
  if(test == "JB"){
    output <- jarque.test(x)
  }
  if(test == "DAP"){
    output <- agostino.test(x)
  }
  return(output)
}
