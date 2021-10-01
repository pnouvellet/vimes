## k = shape and theta = scale in a gamma distribution.

## dmean and dsigma are the mean and sd of the data

g <- rgamma(100000, shape = 90, scale = 700)

dmean <- mean(g)
dsigma <- sd(g)

theta <- dsigma^2/dmean
k <- dmean/theta
theta
k


install.packages("epitrix")
library(epitrix)

gamma_mucv2shapescale(dmean, dsigma/dmean)
