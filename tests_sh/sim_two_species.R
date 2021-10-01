### Multi-species script

### The idea of this script is to create a simulation chain that includes two species
### with different reporting for each of the species. 


## First phase is to use the data that we have to work out what proportion of cases are
## occurring in each species. 

rm(list = ls())

library(tidyverse)


# bring in the data
rabies_cases <- read.csv("tests_sh/animal_cases.csv")
table(rabies_cases$Species)

## We have 303 dog cases 221 jackal cases
## Just using jackals in this instance rather than all wildlife. 

total_cases <- 303+221
s1_obs <- 303 # no of cases observed for dogs/species 1
s2_obs <- 221 # no of cases observed for jackals/species 2

#prop_dogs <- 303/total_cases
#prop_jackals <- 221/total_cases

#prop_dogs + prop_jackals # check that sums to one. 

## Specify the reporting rates of the different species. 
s1_rr <- 1.0 # rr for dogs/species 1
s2_rr <- 1.0 # rr for jackals/species 2

## Use this information to work out the actual number of cases for each species
s1_n <- s1_obs/s1_rr
s2_n <- s2_obs/s2_rr

# now use this to calculate the proportions of each species 

s1_prop <- s1_n/(s1_n+s2_n)
s2_prop <- s2_n/(s1_n+s2_n)

s1_prop + s2_prop # ensure sums to 1. Should make sure there is an error if doesn't when write function

### Now we have the proportion of this species we can use this within the simulation
### We want to have whole numbers of animals - no decimals. 
### So we might want to use one of the proportions to estimate the number of one species 
### and then use n_sims - nsp1 to calculate the other number 

### Start with simulating 1000 animals and finding a cut off for the serial interval
### using the gamma distribution 

q <- 0.95 # set the level of the quantile we want to use later

n = 1000000 # set number of simulations

s1_sim_n <- round(n*s1_prop) 
s2_sim_n <- n - s1_sim_n

s1_sim_n + s2_sim_n # generate error if doesn't = n

# generate the parameters for use within the simulation
si_mean <- 27.8175438596491
si_sd <- 26.8565433014125

params <- c(si_mean, si_sd)

gam_parms <- epitrix::gamma_mucv2shapescale(mu = params[1], cv = params[2]/params[1])
gam_parms[[1]]
gam_parms[[2]]

set.seed(24)

all_ind <- data.frame(Case_no = seq(1, n, 1),
                      distance = c(0,rgamma(n-1, shape = gam_parms$shape, scale = gam_parms$scale))) # adding location of (0,0) for first dog
                      
#assign the species
all_ind$species <- c(rep("s1", s1_sim_n), rep("s2", s2_sim_n))

# randomly reorder the species 
#all_ind$species <- sample(all_ind$species, size = n, replace = F)
all_ind[c(2:n),"species"] <- sample(all_ind[c(2:n), "species"], size = n-1, replace = F)
table(all_ind$species)

#------- Would be good to have a way to shuffle so that we can specify a degree of assortative mixing
#------- Although to a degree they are more likely to be near each other if there are more of that species

# Now need to assign whether they are detected or not

all_ind$det_prob <- runif(n, 0,1)

all_ind[which(all_ind$species == "s1" & all_ind$det_prob <= s1_rr), "detected"] <- TRUE
all_ind[which(all_ind$species == "s2" & all_ind$det_prob <= s2_rr), "detected"] <- TRUE

#table(all_ind$detected)

# work out the cumulative SI
all_ind$cum = cumsum(all_ind$distance)

#Extract the case numbers of those observed
Observed_ind <- which(all_ind$detected)

observed_dist <- diff(all_ind$cum[Observed_ind])

Output <- data.frame(Missing_gens = diff(Observed_ind), # more like k
                     observed_dist = observed_dist) # observed distances between observed animals


threshold_sim <- quantile(Output$observed_dist, q)
density_sim <- density(Output$observed_dist, from = 0, n = round(max(Output$observed_dist)))

plot(density_sim)

range(Output$observed_dist)
hist(Output$observed_dist, breaks = seq(0,500, 10), freq = FALSE)
lines(density_sim, col = "hot pink", lwd = 2)


### Look at the proportion of transmissions that are 1-1, 1-2, 2-1 and 2-2
### make sure using rr=1 for both species to do this initially

#all_ind2 <- all_ind[1:100,]

# initially ran a loop but it takes approximately forever! 

# all_ind$trans <- NA
# for (i in 2:n) {
#   if(all_ind[i-1,"species"] == "s1" && all_ind[i,"species"] == "s1"){
#     all_ind[i,"trans"] = "11"
# } else {
#   if(all_ind[i-1,"species"] == "s1" && all_ind[i,"species"] == "s2"){
#     all_ind[i,"trans"] = "12"
# } else {
#   if(all_ind[i-1,"species"] == "s2" && all_ind[i,"species"] == "s1"){
#     all_ind[i,"trans"] = "21"
# } else {
#   if(all_ind[i-1,"species"] == "s2" && all_ind[i,"species"] == "s2"){
#     all_ind[i,"trans"] = "22"
# }}}}}

#write.csv(all_ind, "tests_sh/chain_with_trans.csv")

# so here is an alternative

all_ind[1,"lag"] <- NA

for(i in 2:n) {
  all_ind[i, "lag"] <- all_ind[i-1, "species"]
}

all_ind$trans <- paste(all_ind$species, all_ind$lag, sep = "")

# Turns out this also takes ages!! 

totes_trans <- n-1 # cos first one doesn't count.
prop_s1s1 <- round(nrow(all_ind[which(all_ind$trans == "s1s1"),])/totes_trans)

