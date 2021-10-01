### Multi-species function first look

## numbner of observed cases for each species
s1_obs <- 303 # no of cases observed for dogs/species 1
s2_obs <- 221 # no of cases observed for jackals/species 2

## Specify the reporting rates of the different species. 
s1_rr <- 1.0 # rr for dogs/species 1
s2_rr <- 0.5 # rr for jackals/species 2

n = 1000 # set number of simulations

q <- 0.95 # set the level of the quantile we want to use later

# generate the parameters for use within the simulation
si_mean <- 27.8175438596491
si_sd <- 26.8565433014125

params <- c(si_mean, si_sd)


get_quantiles_multi <- function(s1_obs, s2_obs, s1_rr, s2_rr, n, params, q) {
  
## Calculate the actual number of cases for each species
s1_n <- s1_obs/s1_rr
s2_n <- s2_obs/s2_rr

# now use this to calculate the proportions of each species 
s1_prop <- s1_n/(s1_n+s2_n)
s2_prop <- s2_n/(s1_n+s2_n)

if(s1_prop + s2_prop != 1){ 
  msg <- "Proportions do not sum to 1"
  stop(msg) # ensure sums to 1. 
}

# 
s1_sim_n <- round(n*s1_prop) 
s2_sim_n <- n - s1_sim_n  # use this way to avoid fractions of animals and it not adding to n

if(s1_sim_n + s2_sim_n != n) {
  msg2 <- "Cases do not sum to specified value of n"
  stop(msg2)
}

gam_parms <- epitrix::gamma_mucv2shapescale(mu = params[1], cv = params[2]/params[1])

all_ind <- data.frame(Case_no = seq(1, n, 1),
                      distance = c(0,rgamma(n-1, shape = gam_parms$shape, scale = gam_parms$scale))) # adding location of (0,0) for first dog

#assign the species
all_ind$species <- c(rep("s1", s1_sim_n), rep("s2", s2_sim_n))

# randomly reorder the species 
#all_ind$species <- sample(all_ind$species, size = n, replace = F)
all_ind[c(2:n),"species"] <- sample(all_ind[c(2:n), "species"], size = n-1, replace = F)


# assign value between 0 and 1 to use for detection
all_ind$det_prob <- runif(n, 0, 1)

# assign detected status to each animal based on species reporting rate
all_ind[which(all_ind$species == "s1" & all_ind$det_prob <= s1_rr), "detected"] <- TRUE
all_ind[which(all_ind$species == "s2" & all_ind$det_prob <= s2_rr), "detected"] <- TRUE

# work out the cumulative SI
all_ind$cum = cumsum(all_ind$distance)

#Extract the case numbers of those observed
Observed_ind <- which(all_ind$detected)

observed_dist <- diff(all_ind$cum[Observed_ind])

Output <- data.frame(Missing_gens = diff(Observed_ind), # more like k
                     observed_dist = observed_dist) # observed distances between observed animals


threshold_sim <- quantile(Output$observed_dist, q)
density_sim <- density(Output$observed_dist, from = 0, n = round(max(Output$observed_dist)))

return(res = list(threshold_sim = threshold_sim, density_sim = density_sim))

}


## Now try the function

out1 <-  get_quantiles_multi(s1_obs = s1_obs, s2_obs = s2_obs, s1_rr = s1_rr, s2_rr = s2_rr,
                    n = n, params = params, q = q)

out1$threshold_sim
plot(out1$density_sim)


## Extract the values at different quantiles and reporting rates. 
## Have the rr the same for both species

pi_range <- c(1.0, 0.8, 0.6, 0.4, 0.2)
quants <- c(.50, .75, .90, .95, .99)
n = 1000000

si_multi_sim_gamma <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(quants)))
colnames(si_multi_sim_gamma) <- quants
rownames(si_multi_sim_gamma) <- pi_range
#Just getting the quantiles

set.seed(434)

for(i in 1:length(pi_range)){
  si_multi_sim_gamma[i,] <- get_quantiles_multi(s1_obs = s1_obs, s2_obs = s2_obs, 
                                                s1_rr = pi_range[i], s2_rr = pi_range[i],
                                                n = n, params = params, 
                                                q = quants)$threshold_sim
}


## Produce the same table using the single species function

si_table_sim_gamma <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(quants)))
colnames(si_table_sim_gamma) <- quants
rownames(si_table_sim_gamma) <- pi_range
#Just getting the quantiles


source("R/get_quantiles_sim.R")

set.seed(434)

for(i in 1:length(pi_range)){
  si_table_sim_gamma[i,] <- get_quantiles_sim(d_type = "temporal",
                                              distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                              params = params, 
                                              q = quants)$threshold_sim
}


# Look at the difference between the tables
diff_table <- round((((si_multi_sim_gamma - si_table_sim_gamma)/si_table_sim_gamma)*100),2)

## I assume that the differences are because of the shuffling of the creature in the two species.
## Differences are very small  

#write.csv(si_table_sim_gamma, "tests_sh/si_table_sim_gamma.csv")
#write.csv(si_multi_sim_gamma, "tests_sh/si_multi_sim_gamma.csv")
#write.csv(diff_table, "tests_sh/diff_table_sims_multi.csv")


## compare two runs of the single species if we set seed

tab1 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(quants)))
colnames(tab1) <- quants
rownames(tab1) <- pi_range
#Just getting the quantiles

set.seed(4)

for(i in 1:length(pi_range)){
  tab1[i,] <- get_quantiles_sim(d_type = "temporal",
                                              distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                              params = params, 
                                              q = quants)$threshold_sim
}

# rpt
tab2 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(quants)))
colnames(tab2) <- quants
rownames(tab2) <- pi_range
#Just getting the quantiles

set.seed(4)

for(i in 1:length(pi_range)){
  tab2[i,] <- get_quantiles_sim(d_type = "temporal",
                                distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                params = params, 
                                q = quants)$threshold_sim
}

# Look at the difference between the tables
dt <- round((((tab1 - tab2)/tab2)*100),2)

### No difference between these tables - this is what we would expect
### Repeat the same process with the multi_species

m1 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(quants)))
colnames(m1) <- quants
rownames(m1) <- pi_range
#Just getting the quantiles

set.seed(4)

for(i in 1:length(pi_range)){
  m1[i,] <- get_quantiles_multi(s1_obs = s1_obs, s2_obs = s2_obs, 
                                                s1_rr = pi_range[i], s2_rr = pi_range[i],
                                                n = n, params = params, 
                                                q = quants)$threshold_sim
}

## repeat
m2 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(quants)))
colnames(m2) <- quants
rownames(m2) <- pi_range
#Just getting the quantiles

set.seed(4)

for(i in 1:length(pi_range)){
  m2[i,] <- get_quantiles_multi(s1_obs = s1_obs, s2_obs = s2_obs, 
                                s1_rr = pi_range[i], s2_rr = pi_range[i],
                                n = n, params = params, 
                                q = quants)$threshold_sim
}


diff_m <- round((((m1 - m2)/m2)*100),2)

round((((tab1 - m1)/m1)*100),2)
round((((tab2 - m2)/m2)*100),2)
