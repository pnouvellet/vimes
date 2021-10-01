# meanlog and sdlog are the parameters of a lognormal distribution.
# dmean and dsigma are the mean and sd of the data. 

rm(list = ls())

dmean <- 26.3
dsigma <- 25.4

sdlog <- sqrt(log(dsigma^2*exp(-2*log(dmean))+1))
meanlog <- log(dmean) - ((sdlog^2)/2)


##Lets check the equation.

a <- rlnorm(100000, 15, 1.5)
#a
range(a)

dmean <- mean(a)
dsigma <- sd(a)

sqrt(log(dsigma^2*exp(-2*log(dmean))+1))
log(dmean) - (sdlog^2/2)


b <- rlnorm(500000, 15, 1.4)
range(b)

dmean <- mean(b)
dsigma <- sd(b)

sqrt(log(dsigma^2*exp(-2*log(dmean))+1))
log(dmean) - (sdlog^2/2)

