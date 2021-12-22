
si_mean <- 27.8175438596491
si_sd <- 26.8565433014125
rayleigh_mean <- 0.88

params_s1s1 <- c(si_mean, si_sd)

## what I'm currently using
meanlog_s1s1 <- log(params_s1s1[1]/(sqrt(1 + params_s1s1[2]^2/params_s1s1[1]^2)))
sdlog_s1s1 <- sqrt(log(1 + params_s1s1[2]^2/params_s1s1[1]^2))
meanlog_s1s1
sdlog_s1s1


# if I take out the 1 in the meanlog we get
log(params_s1s1[1]/(sqrt(params_s1s1[2]^2/params_s1s1[1]^2)))

#install.packages("lognorm")

library(lognorm)
getParmsLognormForMoments(mean = si_mean, var = si_sd^2)

getParmsLognormForMoments

ray_sig_s1s1 <- params_s1s1 / sqrt(acos(-1)/2)


si_sd2 <- 36.8565433014125
rayleigh_mean2 <- 0.87


getParmsLognormForMoments(mean = si_mean, var = si_sd2^2)

rayleigh_mean2/ sqrt(acos(-1)/2)


## with new sd in gamma
epitrix::gamma_mucv2shapescale(mu = si_mean,
                               cv = si_sd/si_mean)


epitrix::gamma_mucv2shapescale(mu = si_mean,
                               cv = si_sd2/si_mean)

