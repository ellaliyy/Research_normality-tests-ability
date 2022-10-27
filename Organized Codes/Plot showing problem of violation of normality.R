#plot showing problems of violation of normality
######################################################################################
#Chisq
plot(c(1,2,5,10,15),altmatrixc[6,c(1:5)],type = 'p',
     xaxt = 'n',
     xlab = 'df',ylab = 'P(typeI)',
     ylim = c(0.045,0.145),
     main = 'Chi-square')
axis(side = 1, c(1,2,5,10,15))
abline(h=0.05,col = 'red')
#t
plot(c(3,5,10,15,30),altmatrixt[6,c(1:5)],type = 'p',
     xaxt = 'n',
     xlab = 'df',ylab = 'P(typeI)',
     main = 't')
axis(side = 1, c(3,5,10,15,30))
abline(h=0.05,col = 'red')
#Beta
plot(c(4,6,8,10,20),altmatrixb[6,c(1:5)],type = 'p',
     xaxt = 'n',
     xlab = 'alpha',ylab = 'P(typeI)',ylim =c(0.045,0.065),
     main = 'Beta')
axis(side = 1, c(4,6,8,10,20))
abline(h=0.05,col = 'red')
#Uniform
plot(c(3,5,10,15,30),altmatrixu[6,c(1:5)],type = 'p',
     xaxt = 'n',
     xlab = 'max--b',ylab = 'P(typeI)',ylim =c(0.045,0.065),
     main = 'uniform')
axis(side = 1, c(3,5,10,15,30))
abline(h=0.05,col = 'red')

#sample size = 10 ----- row 6th
combined <- c(altmatrixc[6,c(1:5)], altmatrixt[6,c(1:5)],
              altmatrixb[6,c(1:5)],altmatrixu[6,c(1:5)])

combmatrix <- matrix(combined)
row.names(combmatrix) <- c("chisq-1","chisq-2",'chisq-5',"chisq-10","chisq-15",
                           "t-3","t-5","t-10","t-15","t-30",
                           "beta-4","beta-6","beta-8","beta-10","beta-20",
                           "uniform-3","uniform-5","uniform-10","uniform-15","uniform-30")
colnames(combmatrix) <- c("P(type I")
