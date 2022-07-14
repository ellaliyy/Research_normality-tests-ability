library("nortest")
library("dgof")
library("dplyr")
library(moments)
rm(list=ls())

#User-defined functions
#Generate data from different distribution
generate_data <- function(n, dist,par1, par2){
  if(dist == "Normal"){
    x <- rnorm(n, mean = par1, sd = par2)
  }
  if(dist == "Standard Normal"){
    x <- rnorm(n, mean = 0, sd = 1)
  }
  if(dist == "Chi-square"){
    x <- rchisq(n,par1)
  }
  if(dist == "Gama"){
    x <- rgamma(n, shape = 2, rate = 1)
  }
  if(dist == "Beta"){
    x <- rbeta(n, shape1 = 2, shape2 = 5)
  }
  if(dist == "Expotential"){
    x <- rexp(n, rate = 1)
  }
  if(dist == "t"){
    x <- rt(n,df = par1)
  }
  if(dist == "Uniform"){
    x <- runif(n, min = 40,max = 60)
  }
  
  return(x)
}

x <- generate_data(5, "Normal", 100, 2)
x
y <- generate_data(5, "Standard Normal")
y


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
output <- generate_tests(x,"KS")
output
