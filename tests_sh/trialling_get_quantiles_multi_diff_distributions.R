### 26/10/2021
### Using the multi-species function in the R folder
### Having different distributions for the different species
### Using the numbers in our data for the proportions. 

rm(list = ls())

## number of observed cases for each species
s1_obs <- 303 # no of cases observed for dogs/species 1
s2_obs <- 221 # no of cases observed for jackals/species 2

## Specify the reporting rates of the different species. 
s1_rr <- 0.60 # rr for dogs/species 1
s2_rr <- 0.60 # rr for jackals/species 2

n = 10000000 # set number of simulations - using 10 million as algorithm very quick

q <- 0.95 # set the level of the quantile we want to use later

# generate the parameters for use within the simulation
si_mean <- 27.8175438596491
si_sd <- 26.8565433014125
rayleigh_mean <- 0.88

## have the option to use different parameters for the different type of tranmsission
## Below we are using all the same
params_s1s1 <- c(si_mean*2, si_sd)
params_s1s2 <- params_s2s1 <- c(si_mean, si_sd)
#params_s2s1 <- c(si_mean, si_sd)
params_s2s2 <- c(si_mean, si_sd)

source("R/get_quantiles_multi.R")


## Pierre we had a convergence check in the single species model. 
## Put something like that in here?

## Let's try the function
tictoc::tic()
out_si_gamma <- get_quantiles_multi(d_type = "temporal", distrib = "gamma", 
                                    s1_obs = s1_obs, s2_obs = s2_obs,
                                    s1_rr = 0.4, s2_rr = 0.4,
                                    params_s1s1 = params_s1s1, params_s1s2 = params_s1s2,
                                    params_s2s1 = params_s2s1, params_s2s2 = params_s2s2,
                                    n = 10000000, q = 0.95)

tictoc::toc()


out_si_gamma$prop_s1s1
out_si_gamma$prop_s1s2
out_si_gamma$prop_s2s1
out_si_gamma$prop_s2s2

out_si_gamma$prop_s1s1_below_quant
out_si_gamma$prop_s1s2_below_quant
out_si_gamma$prop_s2s1_below_quant
out_si_gamma$prop_s2s2_below_quant


out_si_gamma$threshold_sim
out_si_gamma$threshold_s1s1
out_si_gamma$threshold_s1s2
out_si_gamma$threshold_s2s1
out_si_gamma$threshold_s2s2

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


q <- 0.99

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


# Gamma tables at q = 0.95

 # write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_gamma0.95.csv")
 # write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_gamma0.95.csv")
 # write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_gamma0.95.csv")
 # write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_gamma0.95.csv")
 # write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_gamma0.95.csv")
 # 
 # # and extract and save proportions
 # write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_gamma0.95.csv")
 # write.csv(s2_props_table, "tests_sh/s2_props_table_2mean_gamma0.95.csv")
 # write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_gamma0.95.csv")
 # write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_gamma0.95.csv")
 # 
 # # and extract and save proportions below the quantile cut-off
 # write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_gamma0.95.csv")
 # write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_gamma0.95.csv")
 # write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_gamma0.95.csv")
 # write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_gamma0.95.csv")

# Gamma at q = 0.99

#write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_gamma0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_gamma0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_gamma0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_gamma0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_gamma0.99.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_gamma0.99.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_2mean_gamma0.99.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_gamma0.99.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_gamma0.99.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_gamma0.99.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_gamma0.99.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_gamma0.99.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_gamma0.99.csv")


# Gamma at 0.75
#write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_gamma0.75.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_gamma0.75.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_gamma0.75.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_gamma0.75.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_gamma0.75.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_gamma0.75.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_2mean_gamma0.75.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_gamma0.75.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_gamma0.75.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_gamma0.75.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_gamma0.75.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_gamma0.75.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_gamma0.75.csv")


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

# write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_ln0.95.csv")
# write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_ln0.95.csv")
# write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_ln0.95.csv")
# write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_ln0.95.csv")
# write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_ln0.95.csv")
 
 # and extract and save proportions
# write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_ln0.95.csv")
# write.csv(s2_props_table, "tests_sh/s2_props_table_2meanln0.95.csv")
# write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_ln0.95.csv")
# write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_ln0.95.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_ln0.95.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_ln0.95.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_ln0.95.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_ln0.95.csv")


# Lognormal tables at q = 0.99

#write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_ln0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_ln0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_ln0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_ln0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_ln0.99.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_ln0.99.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_2mean_ln0.99.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_ln0.99.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_ln0.99.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_ln0.99.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_ln0.99.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_ln0.99.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_ln0.99.csv")


# Lognormal at 0.75
#write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_ln0.75.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_ln0.75.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_ln0.75.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_ln0.75.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_ln0.75.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_ln0.75.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_2mean_ln0.75.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_ln0.75.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_ln0.75.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_ln0.75.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_ln0.75.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_ln0.75.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_ln0.75.csv")



#### Now do the same set of values for the distance kernel distribution


params_s1s1 <- c(rayleigh_mean*2)
params_s1s2 <- c(rayleigh_mean*2)
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

# write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_spatial0.95.csv")
# write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_spatial0.95.csv")
# write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_spatial0.95.csv")
# write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_spatial0.95.csv")
# write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_spatial0.95.csv")

# and extract and save proportions
# write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_spatial0.95.csv")
# write.csv(s2_props_table, "tests_sh/s2_props_table_2mean_spatial0.95.csv")
# write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_spatial0.95.csv")
# write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_spatial0.95.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_spatial0.95.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_spatial0.95.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_spatial0.95.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_spatial0.95.csv")



# Spatial tables at q = 0.99

#write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_spatial0.99.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_spatial0.99.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_spatial0.99.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_spatial0.99.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_spatial0.99.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_spatial0.99.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_2mean_spatial0.99.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_spatial0.99.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_spatial0.99.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_spatial0.99.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_spatial0.99.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_spatial0.99.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_spatial0.99.csv")



# spatial at 0.75
#write.csv(all_trans_table, "tests_sh/all_trans_table_2mean_spatial0.75.csv")
#write.csv(s1_trans_table, "tests_sh/s1_trans_table_2mean_spatial0.75.csv")
#write.csv(s2_trans_table, "tests_sh/s2_trans_table_2mean_spatial0.75.csv")
#write.csv(s1s2_trans_table, "tests_sh/s1s2_trans_table_2mean_spatial0.75.csv")
#write.csv(s2s1_trans_table, "tests_sh/s2s1_trans_table_2mean_spatial0.75.csv")

# and extract and save proportions
#write.csv(s1_props_table, "tests_sh/s1_props_table_2mean_spatial0.75.csv")
#write.csv(s2_props_table, "tests_sh/s2_props_table_2mean_spatial0.75.csv")
#write.csv(s1s2_props_table, "tests_sh/s1s2_props_table_2mean_spatial0.75.csv")
#write.csv(s2s1_props_table, "tests_sh/s2s1_props_table_2mean_spatial0.75.csv")

# and extract and save proportions below the quantile cut-off
#write.csv(s1_props_table_bq, "tests_sh/s1_props_table_bq_2mean_spatial0.75.csv")
#write.csv(s2_props_table_bq, "tests_sh/s2_props_table_bq_2mean_spatial0.75.csv")
#write.csv(s1s2_props_table_bq, "tests_sh/s1s2_props_table_bq_2mean_spatial0.75.csv")
#write.csv(s2s1_props_table_bq, "tests_sh/s2s1_props_table_bq_2mean_spatial0.75.csv")



