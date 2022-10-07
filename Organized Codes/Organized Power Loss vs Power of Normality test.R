setwd("/Users/ellali/Desktop")
source("Organized User-defined functions.R")

alpha <- 0.05
N <- 10000
smpn <- c(5,6,7,8,9,10,11,12,13,15,17,20,22,25,30,35,40,45,50,100)
smdf <- c(3,5,10,15,30)

#power 1
power1 <- numeric(length(smpn)*length(smdf))
power1matrix <- matrix(power1,
                         nrow=length(smpn),
                         ncol=length(smdf),byrow=TRUE)
row.names(power1matrix) <- smpn
colnames(power1matrix) <- smdf

#power 2
power2 <- numeric(length(smpn)*length(smdf))
power2matrix <- matrix(power2,
                       nrow=length(smpn),
                       ncol=length(smdf),byrow=TRUE)
row.names(power2matrix) <- smpn
colnames(power2matrix) <- smdf

#power 3
power3 <- numeric(length(smpn)*length(smdf))
power3matrix <- matrix(power3,
                       nrow=length(smpn),
                       ncol=length(smdf),byrow=TRUE)
row.names(power3matrix) <- smpn
colnames(power3matrix) <- smdf

#dif
powerdif <- numeric(length(smpn)*length(smdf))
powerdifmatrix <- matrix(powerdif,
                       nrow=length(smpn),
                       ncol=length(smdf),byrow=TRUE)
row.names(powerdifmatrix) <- smpn
colnames(powerdifmatrix) <- smdf

for(i in 1:length(smpn)){
  n <- smpn[i]
  for(k in 1:length(smdf)){
    df <- smdf[k]
    rejectH0_1 <- numeric(N)
    rejectH0_2 <- numeric(N)
    rejectH0_3 <- numeric(N)
    for(j in 1:N){
      x <- generate_data(n,"Chi-square", df)
      out1<- shapiro.test(x)
      pval1 <- out1$p.value
      if(pval1 < alpha){
        rejectH0_1[j] <- 1
      }
      y <- generate_data(n,"Chi-square", df)+1
      out2 <- t.test(x/sqrt(2*df),y/sqrt(2*df))
      pval2 <- out2$p.value
      if(pval2 < alpha){
        rejectH0_2[j] <- 1
      }
      l <- rnorm(n, mean=df, sd=sqrt(2*df))
      z <- rnorm(n, mean=df, sd=sqrt(2*df))+1
      out3 <- t.test(l/sqrt(2*df),z/sqrt(2*df))
      pval3 <- out3$p.value
      if(pval3 < alpha){
        rejectH0_3[j] <- 1
      }
    }
    power1matrix[i,k] <- mean(rejectH0_1)
    power2matrix[i,k] <- mean(rejectH0_2)
    power3matrix[i,k] <- mean(rejectH0_3)
    powerdifmatrix[i,k] <- power3matrix[i,k] - power2matrix[i,k]
  }
}
plot(power1matrix[,1], powerdifmatrix[,1],
     col = 'red',
     pch=18,
     lty=1,
     type = 'b',
     xlab = 'Power of normality test',
     ylab = 'Power loss for downstream test',
     main = 'Power Loss v.s. Power of Normality test')
lines(power1matrix[,2],powerdifmatrix[,2],
      type = 'b',
      lty=2,
      pch=18,
      col = 'blue')
lines(power1matrix[,3],powerdifmatrix[,3],
      type = 'b',
      pch=18,
      lty=3,
      col = 'orange')
lines(power1matrix[,4],powerdifmatrix[,4],
      type = 'b',
      pch=18,
      lty=4,
      col = 'green')
lines(power1matrix[,5],powerdifmatrix[,5],
      type = 'b',
      pch=18,
      lty=5,
      col = 'yellow')
legend("bottomleft", legend=c("df=3", "df=5", 'df=10','df=15','df=30'),
       col=c("red", "blue",'orange','green','yellow'), 
       lty=c(1:5),
       cex=0.8)
