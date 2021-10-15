si_mean <- 27.8175438596491
si_sd <- 26.8565433014125

mu_ln <- log(si_mean/(sqrt(1 + si_sd^2/si_mean^2)))
sd_ln <- sqrt(log(1 + si_sd^2/si_mean^2))

# compare my way and the wiki way

si_mean <- 125
si_sd <- 2.3

params <- c(si_mean, si_sd)

sdlog <- sqrt(log(params[2]^2*exp(-2*log(params[1]))+1))
meanlog <- log(params[1]) - ((sdlog^2)/2)

meanlog_wiki <- log(params[1]/(sqrt(1 + params[2]^2/params[1]^2)))
sdlog_wiki <- sqrt(log(1 + params[2]^2/params[1]^2))


meanlog
meanlog_wiki

sdlog
sdlog_wiki
