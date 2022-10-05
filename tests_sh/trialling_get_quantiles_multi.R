### Multi-species function first look
#rm(list = ls())

## number of observed cases for each species
s1_obs <- 303 # no of cases observed for dogs/species 1
s2_obs <- 221 # no of cases observed for jackals/species 2

## Specify the reporting rates of the different species. 
s1_rr <- 1 #0.60 # rr for dogs/species 1
s2_rr <- 1 #  0.60 # rr for jackals/species 2

n = 1000 # set number of simulations

q <- 0.95 # set the level of the quantile we want to use later

# generate the parameters for use within the simulation
si_mean <- 27.8175438596491
si_sd <- 26.8565433014125
rayleigh_mean <- 0.88

## have the option to use different parameters for the different type of tranmsission
## Below we are using all the same

params_s1s1 <- c(si_mean, si_sd)
params_s1s2 <- c(si_mean, si_sd)
#params_s2s1 <- c(si_mean, si_sd)
params_s2s2 <- c(si_mean*2, si_sd)


params_s1s1 <- c(rayleigh_mean)
#params_s1s2 <- c(rayleigh_mean)
params_s2s1 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean*20)


## Should I set the first animals as zero? Meed to look at that

source("R/get_quantiles_multi.R")

## Pierre we had a convergence check in the single species model. 
## Put something like that in here?

## Let's try the function
tictoc::tic()
out_si_gamma <- get_quantiles_multi(d_type = "temporal", distrib = "gamma", 
                                    s1_obs = s1_obs, s2_obs = s2_obs,
                                    s1_rr = 0.6, s2_rr = 0.4,
                                    params_s1s1 = c(si_mean, si_sd), params_s1s2 = c(si_mean, si_sd),
                                    #params_s2s1 = c(si_mean, si_sd), 
                                    params_s2s2 = c(si_mean, si_sd),
                                    n = 1000000, q = 0.95)

tictoc::toc()


out_si_gamma$prop_s1s1
out_si_gamma$prop_s1s2
#out_si_gamma$prop_s2s1
out_si_gamma$prop_s2s2

out_si_gamma$prop_s1s1_below_quant
out_si_gamma$prop_s1s2_below_quant
out_si_gamma$prop_s2s1_below_quant
out_si_gamma$prop_s2s2_below_quant

# As want to look at the values for lots of different combinations of reporting rates
# for the different transmission types will write a function to run this

### first get the results using all the transmission types

rr_s1_vect <- rr_s2_vect <-  c(1.0, 0.8, 0.6, 0.4, 0.2, 0.1)

#cutoff tables
all_trans_table <- s1_trans_table <- s1s2_trans_table <- s2s1_trans_table <- 
  s2_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(all_trans_table) <- colnames(s1_trans_table) <- colnames(s1s2_trans_table) <-
  colnames(s2s1_trans_table) <- colnames(s2_trans_table) <-  rr_s1_vect
rownames(all_trans_table) <- rownames(s1_trans_table) <- rownames(s1s2_trans_table) <-
  rownames(s2s1_trans_table) <- rownames(s2_trans_table) <-  rr_s2_vect

# proportions tables
s1_props_table <- s1s2_props_table <- s2s1_props_table <- 
  s2_props_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s1_props_table) <- colnames(s1s2_props_table) <-
  colnames(s2s1_props_table) <- colnames(s2_props_table) <-  rr_s1_vect
rownames(s1_props_table) <- rownames(s1s2_props_table) <-
  rownames(s2s1_props_table) <- rownames(s2_props_table) <-  rr_s2_vect

# proportions tables below quantiles
s1_props_table_bq <- s1s2_props_table_bq <- s2s1_props_table_bq <- 
  s2_props_table_bq <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s1_props_table_bq) <- colnames(s1s2_props_table_bq) <-
  colnames(s2s1_props_table_bq) <- colnames(s2_props_table_bq) <-  rr_s1_vect
rownames(s1_props_table_bq) <- rownames(s1s2_props_table_bq) <-
  rownames(s2s1_props_table_bq) <- rownames(s2_props_table_bq) <-  rr_s2_vect


q <- 0.95

tictoc::tic()
for (i in 1:length(rr_s1_vect)) {
  for (j in 1:length(rr_s2_vect)) {
    out <-  get_quantiles_multi(d_type = "temporal", distrib = "gamma", 
                                s1_obs = s1_obs, s2_obs = s2_obs,
                                s1_rr = rr_s1_vect[i],
                                s2_rr = rr_s2_vect[j],
                                params_s1s1 = params_s1s1, params_s1s2 = params_s1s2,
                                params_s2s1 = params_s2s1, params_s2s2 = params_s2s2,
                                n = 10000000, q = q)
    all_trans_table[j,i] <- out$threshold_sim
    s1_trans_table[j,i] <- out$threshold_s1s1
    s2_trans_table[j,i] <- out$threshold_s2s2
    s1s2_trans_table[j,i] <- out$threshold_s1s2
    s2s1_trans_table[j,i] <- out$threshold_s2s1
    
    s1_props_table[j,i] <- out$prop_s1s1
    s2_props_table[j,i] <- out$prop_s2s2
    s1s2_props_table[j,i] <- out$prop_s1s2
    s2s1_props_table[j,i] <- out$prop_s2s1
    
    s1_props_table_bq[j,i] <- out$prop_s1s1_below_quant
    s2_props_table_bq[j,i] <- out$prop_s2s2_below_quant
    s1s2_props_table_bq[j,i] <- out$prop_s1s2_below_quant
    s2s1_props_table_bq[j,i] <- out$prop_s2s1_below_quant
    
  }
}
tictoc::toc()


## Gamma tables at q = 0.95

# write.csv(all_trans_table, "tests_sh/all_trans_table_gamma0.95.csv")
# write.csv(s1_trans_table, "tests_sh/s1_trans_table_gamma0.95.csv")
# write.csv(s2_trans_table, "tests_sh/s2_trans_table_gamma0.95.csv")
# write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_gamma0.95.csv")
# write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_gamma0.95.csv")
# 
# # and extract and save proportions
# write.csv(s1_props_table, "tests_sh/s1_props_table_gamma0.95.csv")
# write.csv(s2_props_table, "tests_sh/s2_props_table_gamma0.95.csv")
# write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_gamma0.95.csv")
# write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_gamma0.95.csv")

# and extract and save proportions below the quantile cut-off
# write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_gamma0.95.csv")
# write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_gamma0.95.csv")
# write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_gamma0.95.csv")
# write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_gamma0.95.csv")



# Gamma at q = 0.99

#write.csv(all_trans_table, "tests_sh/all_trans_table_gamma0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_gamma0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_gamma0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_gamma0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_gamma0.99.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_gamma0.99.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_gamma0.99.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_gamma0.99.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_gamma0.99.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_gamma0.99.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_gamma0.99.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_gamma0.99.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_gamma0.99.csv")


# Gamma at 0.75
#write.csv(all_trans_table, "tests_sh/all_trans_table_gamma0.75.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_gamma0.75.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_gamma0.75.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_gamma0.75.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_gamma0.75.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_gamma0.75.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_gamma0.75.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_gamma0.75.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_gamma0.75.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_gamma0.75.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_gamma0.75.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_gamma0.75.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_gamma0.75.csv")



#####################################
#### Now do the same set of values using the lognormal for the serial interval distribution

q <- 0.99

tictoc::tic()
for (i in 1:length(rr_s1_vect)) {
  for (j in 1:length(rr_s2_vect)) {
    out <-  get_quantiles_multi(d_type = "temporal", distrib = "lognormal", 
                                s1_obs = s1_obs, s2_obs = s2_obs,
                                s1_rr = rr_s1_vect[i],
                                s2_rr = rr_s2_vect[j],
                                params_s1s1 = params_s1s1, params_s1s2 = params_s1s2,
                                params_s2s1 = params_s2s1, params_s2s2 = params_s2s2,
                                n = 10000000, q = q)
    all_trans_table[j,i] <- out$threshold_sim
    s1_trans_table[j,i] <- out$threshold_s1s1
    s2_trans_table[j,i] <- out$threshold_s2s2
    s1s2_trans_table[j,i] <- out$threshold_s1s2
    s2s1_trans_table[j,i] <- out$threshold_s2s1
    
    s1_props_table[j,i] <- out$prop_s1s1
    s2_props_table[j,i] <- out$prop_s2s2
    s1s2_props_table[j,i] <- out$prop_s1s2
    s2s1_props_table[j,i] <- out$prop_s2s1
    
    s1_props_table_bq[j,i] <- out$prop_s1s1_below_quant
    s2_props_table_bq[j,i] <- out$prop_s2s2_below_quant
    s1s2_props_table_bq[j,i] <- out$prop_s1s2_below_quant
    s2s1_props_table_bq[j,i] <- out$prop_s2s1_below_quant
  }
}
tictoc::toc()

## Lognormal tables at q = 0.95

# write.csv(all_trans_table, "tests_sh/all_trans_table_ln0.95.csv")
# write.csv(s1_trans_table, "tests_sh/s1_trans_table_ln0.95.csv")
# write.csv(s2_trans_table, "tests_sh/s2_trans_table_ln0.95.csv")
# write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_ln0.95.csv")
# write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_ln0.95.csv")
# 
# # and extract and save proportions
# write.csv(s1_props_table, "tests_sh/s1_props_table_ln0.95.csv")
# write.csv(s2_props_table, "tests_sh/s2_props_table_ln0.95.csv")
# write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_ln0.95.csv")
# write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_ln0.95.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_ln0.95.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_ln0.95.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_ln0.95.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_ln0.95.csv")



# Lognormal tables at q = 0.99

#write.csv(all_trans_table, "tests_sh/all_trans_table_ln0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_ln0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_ln0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_ln0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_ln0.99.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_ln0.99.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_ln0.99.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_ln0.99.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_ln0.99.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_ln0.99.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_ln0.99.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_ln0.99.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_ln0.99.csv")


# Lognormal at 0.75
#write.csv(all_trans_table, "tests_sh/all_trans_table_ln0.75.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_ln0.75.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_ln0.75.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_ln0.75.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_ln0.75.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_ln0.75.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_ln0.75.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_ln0.75.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_ln0.75.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_ln0.75.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_ln0.75.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_ln0.75.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_ln0.75.csv")

#### Now do the same set of values for the distance kernel distribution

params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s1 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean)

q <- 0.99

tictoc::tic()
for (i in 1:length(rr_s1_vect)) {
  for (j in 1:length(rr_s2_vect)) {
    out <-  get_quantiles_multi(d_type = "spatial", 
                                s1_obs = s1_obs, s2_obs = s2_obs,
                                s1_rr = rr_s1_vect[i],
                                s2_rr = rr_s2_vect[j],
                                params_s1s1 = params_s1s1, params_s1s2 = params_s1s2,
                                params_s2s1 = params_s2s1, params_s2s2 = params_s2s2,
                                n = 10000000, q = q)
    all_trans_table[j,i] <- out$threshold_sim
    s1_trans_table[j,i] <- out$threshold_s1s1
    s2_trans_table[j,i] <- out$threshold_s2s2
    s1s2_trans_table[j,i] <- out$threshold_s1s2
    s2s1_trans_table[j,i] <- out$threshold_s2s1
    
    s1_props_table[j,i] <- out$prop_s1s1
    s2_props_table[j,i] <- out$prop_s2s2
    s1s2_props_table[j,i] <- out$prop_s1s2
    s2s1_props_table[j,i] <- out$prop_s2s1
    
    s1_props_table_bq[j,i] <- out$prop_s1s1_below_quant
    s2_props_table_bq[j,i] <- out$prop_s2s2_below_quant
    s1s2_props_table_bq[j,i] <- out$prop_s1s2_below_quant
    s2s1_props_table_bq[j,i] <- out$prop_s2s1_below_quant
  }
}
tictoc::toc()

## Spatial tables at q = 0.95

# write.csv(all_trans_table, "tests_sh/all_trans_table_spatial0.95.csv")
# write.csv(s1_trans_table, "tests_sh/s1_trans_table_spatial0.95.csv")
# write.csv(s2_trans_table, "tests_sh/s2_trans_table_spatial0.95.csv")
# write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_spatial0.95.csv")
# write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_spatial0.95.csv")
 
 # and extract and save proportions
# write.csv(s1_props_table, "tests_sh/s1_props_table_spatial0.95.csv")
# write.csv(s2_props_table, "tests_sh/s2_props_table_spatial0.95.csv")
# write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_spatial0.95.csv")
# write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_spatial0.95.csv")

# and extract and save proportions below the quantile cut-off
# write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_spatial0.95.csv")
# write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_spatial0.95.csv")
# write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_spatial0.95.csv")
# write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_spatial0.95.csv")


# Spatial tables at q = 0.99

#write.csv(all_trans_table, "tests_sh/all_trans_table_spatial0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_spatial0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_spatial0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_spatial0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_spatial0.99.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_spatial0.99.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_spatial0.99.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_spatial0.99.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_spatial0.99.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_spatial0.99.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_spatial0.99.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_spatial0.99.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_spatial0.99.csv")



# spatial at 0.75
#write.csv(all_trans_table, "tests_sh/all_trans_table_spatial0.75.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_spatial0.75.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_spatial0.75.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_spatial0.75.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_spatial0.75.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_spatial0.75.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_spatial0.75.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_spatial0.75.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_spatial0.75.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_spatial0.75.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_spatial0.75.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_spatial0.75.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_spatial0.75.csv")



###------------------------------------------------------------------------------
# Old work below here

## Now try the function for the SIs
tictoc::tic()
out1 <-  get_quantiles_multi(d_type = "temporal", distrib = "lognormal", s1_obs = s1_obs, s2_obs = s2_obs,
                             s1_rr = 0.4, s2_rr = 0.4,
                             n = 100000, params = params, q = q)
tictoc::toc()


out1$threshold_sim
out1$threshold_s1s1
out1$threshold_s2s2
out1$threshold_s1s2
out1$threshold_s2s1

plot(out1$density_sim, xlab = "Serial interval", main = "Density plot of serial interval distribution")
lines(out1$density_s1s1, col = "red")
lines(out1$density_s2s2, col = "blue")
lines(out1$density_s1s2, col = "hot pink")
lines(out1$density_s2s1, col = "purple")


####################################################
## and for the distances

tictoc::tic()
out2 <-  get_quantiles_multi(d_type = "spatial", s1_obs = s1_obs, s2_obs = s2_obs,
                             s1_rr = 0.2, s2_rr = 0.2,
                             n = 100000, params = params, q = q)
tictoc::toc()


out2$threshold_sim
out2$threshold_s1s1
out2$threshold_s2s2
out2$threshold_s1s2
out2$threshold_s2s1

plot(out2$density_sim, xlab = "Distance (km)", main = "Density plot of distance kernel distribution")
lines(out2$density_s1s1, col = "red")
lines(out2$density_s2s2, col = "blue")
lines(out2$density_s1s2, col = "hot pink")
lines(out2$density_s2s1, col = "purple")




########### 

# As want to look at the values for lots of different combinations of reporting rates
# for the different transmission types will write a function to run this

### first get the results using all the transmission types
 
rr_s1_vect <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0.1)
rr_s2_vect <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0.1)

all_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(all_trans_table) <- rr_s1_vect
rownames(all_trans_table) <- rr_s2_vect

## now with s1s1 trans
s1_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s1_trans_table) <- rr_s1_vect
rownames(s1_trans_table) <- rr_s2_vect

## now with s1s2 trans
s1s2_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s1s2_trans_table) <- rr_s1_vect
rownames(s1s2_trans_table) <- rr_s2_vect

## now with s2s1 trans
s2s1_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s2s1_trans_table) <- rr_s1_vect
rownames(s2s1_trans_table) <- rr_s2_vect

## now with s2 trans
s2_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s2_trans_table) <- rr_s1_vect
rownames(s2_trans_table) <- rr_s2_vect

q <- 0.99


tictoc::tic()
for (i in 1:length(rr_s1_vect)) {
  for (j in 1:length(rr_s2_vect)) {
    out <-  get_quantiles_multi(d_type = "spatial",
                                s1_obs = s1_obs, s2_obs = s2_obs, 
                                s1_rr = rr_s1_vect[i], 
                                s2_rr = rr_s2_vect[j],
                                n = 1000000, 
                                params = params, 
                                q = q)
    all_trans_table[j,i] <- out$threshold_sim
    s1_trans_table[j,i] <- out$threshold_s1s1
    s2_trans_table[j,i] <- out$threshold_s2s2
    s1s2_trans_table[j,i] <- out$threshold_s1s2
    s2s1_trans_table[j,i] <- out$threshold_s2s1
  }
}
tictoc::toc()


## Gamma tables

#write.csv(all_trans_table, "tests_sh/all_trans_table_gamma0.95.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_gamma0.95.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_gamma0.95.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_gamma0.95.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_gamma0.95.csv")

# Run at q = 0.99 and save too 

#write.csv(all_trans_table, "tests_sh/all_trans_table_gamma0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_gamma0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_gamma0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_gamma0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_gamma0.99.csv")


# Lognormal tables
#write.csv(all_trans_table, "tests_sh/all_trans_table_ln0.95.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_ln0.95.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_ln0.95.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_ln0.95.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_ln0.95.csv")

# With q at 0.99
#write.csv(all_trans_table, "tests_sh/all_trans_table_ln0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_ln0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_ln0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_ln0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_ln0.99.csv")


# Distance tables
#write.csv(all_trans_table, "tests_sh/all_trans_table_dist0.95.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_dist0.95.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_dist0.95.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_dist0.95.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_dist0.95.csv")

# With q at 0.99
#write.csv(all_trans_table, "tests_sh/all_trans_table_dist0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_dist0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_dist0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_dist0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_dist0.99.csv")

########################################################################################
## There is more variation in the lower numbers so I am going to rerun the
## lower reporting rates with higher n to see if this irons out this variation

rr_s1_vect <- c(0.4, 0.2, 0.1)
rr_s2_vect <- c(0.4, 0.2, 0.1)

all_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(all_trans_table) <- rr_s1_vect
rownames(all_trans_table) <- rr_s2_vect

## now with s1s1 trans
s1_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s1_trans_table) <- rr_s1_vect
rownames(s1_trans_table) <- rr_s2_vect

## now with s1s2 trans
s1s2_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s1s2_trans_table) <- rr_s1_vect
rownames(s1s2_trans_table) <- rr_s2_vect

## now with s2s1 trans
s2s1_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s2s1_trans_table) <- rr_s1_vect
rownames(s2s1_trans_table) <- rr_s2_vect

## now with s2 trans
s2_trans_table <- as.data.frame(matrix(ncol = length(rr_s1_vect), nrow = length(rr_s2_vect)))
colnames(s2_trans_table) <- rr_s1_vect
rownames(s2_trans_table) <- rr_s2_vect

q <- 0.99

tictoc::tic()
for (i in 1:length(rr_s1_vect)) {
  for (j in 1:length(rr_s2_vect)) {
    out <-  get_quantiles_multi(d_type = "temporal", distrib = "gamma",
                                s1_obs = s1_obs, s2_obs = s2_obs, 
                                s1_rr = rr_s1_vect[i], 
                                s2_rr = rr_s2_vect[j],
                                n = 2000000, 
                                params = params, 
                                q = q)
    all_trans_table[j,i] <- out$threshold_sim
    s1_trans_table[j,i] <- out$threshold_s1s1
    s2_trans_table[j,i] <- out$threshold_s2s2
    s1s2_trans_table[j,i] <- out$threshold_s1s2
    s2s1_trans_table[j,i] <- out$threshold_s2s1
  }
}
tictoc::toc()


## Gamma tables

# write.csv(all_trans_table, "tests_sh/all_trans_table_gamma0.95_lowrr_highn.csv")
# write.csv(s1_trans_table, "tests_sh/s1_trans_table_gamma0.95_lowrr_highn.csv")
# write.csv(s2_trans_table, "tests_sh/s2_trans_table_gamma0.95_lowrr_highn.csv")
# write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_gamma0.95_lowrr_highn.csv")
# write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_gamma0.95_lowrr_highn.csv")

# And 0.99 quantile cut-off
write.csv(all_trans_table, "tests_sh/all_trans_table_gamma0.99_lowrr_highn.csv")
write.csv(s1_trans_table, "tests_sh/s1_trans_table_gamma0.99_lowrr_highn.csv")
write.csv(s2_trans_table, "tests_sh/s2_trans_table_gamma0.99_lowrr_highn.csv")
write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_gamma0.99_lowrr_highn.csv")
write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_gamma0.99_lowrr_highn.csv")


###############################################################################################
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

#this will just give us the value for the overall threshold - not all the values for the different transmissions 
for(i in 1:length(pi_range)){
  si_multi_sim_gamma[i,] <- get_quantiles_multi(s1_obs = s1_obs, s2_obs = s2_obs, 
                                                s1_rr = pi_range[i], s2_rr = pi_range[i],
                                                n = n, params = params, 
                                                q = quants)$threshold_sim
}

write.csv(si_multi_sim_gamma, "tests_sh/multi_sp_d100_j50_1mil.csv")







######################  Got to here


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
