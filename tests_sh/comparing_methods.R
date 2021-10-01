### Compare the two methods to produce the cut-off values. 

rm(list = ls())

library(tidyverse)
library(vimes)

# vimes results
vimes_ln <- read.csv("tests_sh/vimes_si_ln.csv", row.names = 1, check.names = F)

vimes_gamma <- read.csv("tests_sh/vimes_si_gamma.csv", row.names = 1, check.names = F)

# simulation results
sim_ln_1mil <- read.csv("tests_sh/sim_si_ln_1mill.csv", row.names = 1, check.names = F)
sim_ln_5mil <- read.csv("tests_sh/sim_si_ln_5mill.csv", row.names = 1, check.names = F)
sim_ln_10mil <- read.csv("tests_sh/sim_si_ln_10mill.csv", row.names = 1, check.names = F)

sim_gamma_1mil <- read.csv("tests_sh/sim_si_gamma_1mill.csv", row.names = 1, check.names = F)
sim_gamma_5mil <- read.csv("tests_sh/sim_si_gamma_5mill.csv", row.names = 1, check.names = F)
sim_gamma_10mil <- read.csv("tests_sh/sim_si_gamma_10mill.csv", row.names = 1, check.names = F)


# the comparison tables that compare the two runs with the same number of simulations.

# 1 million
comp_vimes_sim_1mil_ln <- vimes_ln/sim_ln_1mil
comp_vimes_sim_1mil_ln <- comp_vimes_sim_1mil_ln*100
comp_vimes_sim_1mil_ln <- comp_vimes_sim_1mil_ln - 100
comp_vimes_sim_1mil_ln <- round(comp_vimes_sim_1mil_ln, 2)
range(comp_vimes_sim_1mil_ln)

# 5 million
comp_vimes_sim_5mil_ln <- vimes_ln/sim_ln_5mil
comp_vimes_sim_5mil_ln <- comp_vimes_sim_5mil_ln*100
comp_vimes_sim_5mil_ln <- comp_vimes_sim_5mil_ln - 100
comp_vimes_sim_5mil_ln <- round(comp_vimes_sim_5mil_ln, 2)
range(comp_vimes_sim_5mil_ln)

#10 million
comp_vimes_sim_10mil_ln <- vimes_ln/sim_ln_10mil
comp_vimes_sim_10mil_ln <- comp_vimes_sim_10mil_ln*100
comp_vimes_sim_10mil_ln <- comp_vimes_sim_10mil_ln - 100
comp_vimes_sim_10mil_ln <- round(comp_vimes_sim_10mil_ln, 2)
range(comp_vimes_sim_10mil_ln)


#### Comparing the gamma distribution 
# 1 million
comp_vimes_sim_1mil_gamma <- vimes_gamma/sim_gamma_1mil
comp_vimes_sim_1mil_gamma <- comp_vimes_sim_1mil_gamma*100
comp_vimes_sim_1mil_gamma <- comp_vimes_sim_1mil_gamma - 100
comp_vimes_sim_1mil_gamma <- round(comp_vimes_sim_1mil_gamma, 2)
range(comp_vimes_sim_1mil_gamma)

# 5 million
comp_vimes_sim_5mil_gamma <- vimes_gamma/sim_gamma_5mil
comp_vimes_sim_5mil_gamma <- comp_vimes_sim_5mil_gamma*100
comp_vimes_sim_5mil_gamma <- comp_vimes_sim_5mil_gamma - 100
comp_vimes_sim_5mil_gamma <- round(comp_vimes_sim_5mil_gamma, 2)
range(comp_vimes_sim_5mil_gamma)

#10 million
comp_vimes_sim_10mil_gamma <- vimes_gamma/sim_gamma_10mil
comp_vimes_sim_10mil_gamma <- comp_vimes_sim_10mil_gamma*100
comp_vimes_sim_10mil_gamma <- comp_vimes_sim_10mil_gamma - 100
comp_vimes_sim_10mil_gamma <- round(comp_vimes_sim_10mil_gamma, 2)
range(comp_vimes_sim_10mil_gamma)

## Save the output tables

#write.csv(comp_vimes_sim_1mil_gamma, "tests_sh/comp_vimes_sim_1mil_gamma.csv")
#write.csv(comp_vimes_sim_5mil_gamma, "tests_sh/comp_vimes_sim_5mil_gamma.csv")
#write.csv(comp_vimes_sim_10mil_gamma, "tests_sh/comp_vimes_sim_10mil_gamma.csv")

#write.csv(comp_vimes_sim_1mil_ln, "tests_sh/comp_vimes_sim_1mil_ln.csv")
#write.csv(comp_vimes_sim_5mil_ln, "tests_sh/comp_vimes_sim_5mil_ln.csv")
#write.csv(comp_vimes_sim_10mil_ln, "tests_sh/comp_vimes_sim_10mil_ln.csv")


##### Now repeat for the spatial kernel.

vimes_dist <- read.csv("tests_sh/vimes_distance.csv", row.names = 1, check.names = F)

sim_dist_1mil <- read.csv("tests_sh/sim_dist_1mill.csv", row.names = 1, check.names = F)
sim_dist_5mil <- read.csv("tests_sh/sim_dist_5mill.csv", row.names = 1, check.names = F)
sim_dist_10mil <- read.csv("tests_sh/sim_dist_10mill.csv", row.names = 1, check.names = F)


#### Comparing the simulation and vimes results 
# 1 million
comp_vimes_sim_1mil_dist <- vimes_dist/sim_dist_1mil
comp_vimes_sim_1mil_dist <- comp_vimes_sim_1mil_dist*100
comp_vimes_sim_1mil_dist <- comp_vimes_sim_1mil_dist - 100
comp_vimes_sim_1mil_dist <- round(comp_vimes_sim_1mil_dist, 2)
range(comp_vimes_sim_1mil_dist)

# 5 million
comp_vimes_sim_5mil_dist <- vimes_dist/sim_dist_5mil
comp_vimes_sim_5mil_dist <- comp_vimes_sim_5mil_dist*100
comp_vimes_sim_5mil_dist <- comp_vimes_sim_5mil_dist - 100
comp_vimes_sim_5mil_dist <- round(comp_vimes_sim_5mil_dist, 2)
range(comp_vimes_sim_5mil_dist)

#10 million
comp_vimes_sim_10mil_dist <- vimes_dist/sim_dist_10mil
comp_vimes_sim_10mil_dist <- comp_vimes_sim_10mil_dist*100
comp_vimes_sim_10mil_dist <- comp_vimes_sim_10mil_dist - 100
comp_vimes_sim_10mil_dist <- round(comp_vimes_sim_10mil_dist, 2)
range(comp_vimes_sim_10mil_dist)

## Save the output tables

#write.csv(comp_vimes_sim_1mil_dist, "tests_sh/comp_vimes_sim_1mil_dist.csv")
#write.csv(comp_vimes_sim_5mil_dist, "tests_sh/comp_vimes_sim_5mil_dist.csv")
#write.csv(comp_vimes_sim_10mil_dist, "tests_sh/comp_vimes_sim_10mil_dist.csv")
