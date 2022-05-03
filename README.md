# Normality-Tests
Codes related to Normality tests research

```{r}
library(tidyverse)
library(ggpubr)
library(rstatix)
rm(list=ls())
```

```{r}
#Null hypothesis~H0:mean1 = mean2
#x,y have same means(df=3) and violate the normality
#rm(list=ls())
alpha <- 0.05
N <- 1000
rejectH0 <- pval <-  numeric(N)

for(i in 1:N){
  x <- rchisq(25, 30)
  y <- rchisq(25, 30)
  out <- t.test(x,y)
  pval[i] <- out$p.value
  if(pval[i] < alpha){
    rejectH0[i] <- 1
  }
}
TypeIRate1 <- mean(rejectH0)
```

```{r}
#Null hypothesis~H0:mean1 = mean2
#x,y have same means (df=5) and violate the normality
#rm(list=ls())
alpha <- 0.05
N <- 1000
rejectH0 <- pval <-  numeric(N)

for(i in 1:N){
  x <- rchisq(25, 30)
  y <- rchisq(25, 32)
  out <- t.test(x,y)
  pval[i] <- out$p.value
  if(pval[i] < alpha){
    rejectH0[i] <- 1
  }
}
TypeIRate2 <- mean(rejectH0)
```

```{r}
alpha <- 0.05
N <- 1000
rejectH0 <- pval <-  numeric(N)

nvec <- c(5,10,15,20,25)
dfvec <- c(3,5,8,10,15)
powervec <- numeric(length(nvec)*length(dfvec))
powermatrix <- matrix(powervec,
                      nrow=length(nvec),ncol=length(dfvec),byrow=TRUE)

for(i in 1:length(nvec)){
  n <- nvec[i]
  for(k in 1:length(dfvec)){
    df <- dfvec[k]
    for(j in 1:N){
      x <- rchisq(n,df)
      y <- rchisq(n,df)
      out <- t.test(x,y)
      pval <- out$p.value
      if(pval < alpha){
        rejectH0[j] <- 1
      }
    }
    powermatrix[i,k] <- mean(rejectH0)
  }
}
powermatrix
barplot(powermatrix, main="TypeI Error Rate")

```
