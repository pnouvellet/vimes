## 01/12/2021
## Looking at assortativity
## We are going to use all the cases in our data for the numbers. 
## Domestic will be species 1 - this is dogs and cats. 
## Wildlife will be all wildlife species and will be species 2.
## We are going to use a larger distance for S2S2 transmissions to represent the
## larger home ranges of jackals cf dogs
## Looking at how the diffferent reporting and assortativity parameters affect the proportions and cut-offs

rm(list=ls())
library(devtools)
library(vimes)
library(fields)
library(tidyverse)

# Bring in the data we are going to use
source("tests_sh/prep_data_for_vimes.R")

set.seed(1234)

## number of observed cases for each species
s1_obs <- 313 # no of cases observed for domestic/species 1
s2_obs <- 236 # no of cases observed for wildlife/species 2

## Specify the reporting rates of the different species. 
s1_rr <- 1 # rr for dogs/species 1
s2_rr <- 1 # rr for jackals/species 2

n = 10000000 # set number of simulations - using 10 million initially #as algorithm very quick

q <- 0.95 # set the level of the quantile we want to use later

# generate the parameters for use within the simulation
si_mean <- 27.8175438596491
si_sd <- 36.8565433014125
rayleigh_mean <- 0.87
rayleigh_mean <- 870

## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean)

#shape_1 <- 1 # if keep the shape_! = 1 within the function we won't need to specify externally
shape_2 <- 1

source("tests_sh/get_quantiles_multi_assort.R")

# We want to run a number of different values for the shape_2 parameter.
# number of values that we want to check
n_grid <- 10

shape_vect <- c(seq(1, 10.0, length.out = n_grid))

# change at higher values.
shape_vect

sp_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(sp_res_df) <- shape_vect
colnames(sp_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_sp <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                          params_s1s2 = params_s1s2,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  sp_res_df[i,"all_cut"] <- out_sp$threshold_sim[[1]]
  sp_res_df[i, "s1_cut"] <- out_sp$threshold_s1s1[[1]]
  sp_res_df[i, "mix_cut"] <- out_sp$threshold_s1s2[[1]]
  sp_res_df[i, "s2_cut"] <- out_sp$threshold_s2s2[[1]]
  sp_res_df[i, "s1_prop_all"] <- out_sp$prop_s1s1
  sp_res_df[i, "mix_prop_all"] <- out_sp$prop_s1s2
  sp_res_df[i, "s2_prop_all"] <- out_sp$prop_s2s2
  sp_res_df[i, "s1_prop_cut"] <- out_sp$prop_s1s1_below_quant
  sp_res_df[i, "mix_prop_cut"] <- out_sp$prop_s1s2_below_quant
  sp_res_df[i, "s2_prop_cut"] <- out_sp$prop_s2s2_below_quant
  
}


#write.csv(sp_res_df, "tests_sh/effect_of_params_on cut_offs/exp_assort_spatial_rp1_same_params.csv")


###### Repeat with an increase serial interval for S2S2 but keep everything else the same


params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean*5)

# change at higher values.
shape_vect

sp_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(sp_res_df) <- shape_vect
colnames(sp_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_sp <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                          params_s1s2 = params_s1s2,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  sp_res_df[i,"all_cut"] <- out_sp$threshold_sim[[1]]
  sp_res_df[i, "s1_cut"] <- out_sp$threshold_s1s1[[1]]
  sp_res_df[i, "mix_cut"] <- out_sp$threshold_s1s2[[1]]
  sp_res_df[i, "s2_cut"] <- out_sp$threshold_s2s2[[1]]
  sp_res_df[i, "s1_prop_all"] <- out_sp$prop_s1s1
  sp_res_df[i, "mix_prop_all"] <- out_sp$prop_s1s2
  sp_res_df[i, "s2_prop_all"] <- out_sp$prop_s2s2
  sp_res_df[i, "s1_prop_cut"] <- out_sp$prop_s1s1_below_quant
  sp_res_df[i, "mix_prop_cut"] <- out_sp$prop_s1s2_below_quant
  sp_res_df[i, "s2_prop_cut"] <- out_sp$prop_s2s2_below_quant
  
}


#write.csv(sp_res_df, "tests_sh/effect_of_params_on cut_offs/exp_assort_spatial_rp1_5mean.csv")

### Next scenario
### Going to look at different reporting rates.

# Specify the reporting rates of the different species. 
s1_rr <- 0.5 # rr for dogs/species 1
s2_rr <- 0.5 # rr for jackals/species 2


## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean)

# We want to run a number of different values for the shape_2 parameter.
# number of values that we want to check
n_grid <- 10

shape_vect <- c(seq(1, 10.0, length.out = n_grid))

# change at higher values.
shape_vect

sp_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(sp_res_df) <- shape_vect
colnames(sp_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_sp <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                          params_s1s2 = params_s1s2,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  sp_res_df[i,"all_cut"] <- out_sp$threshold_sim[[1]]
  sp_res_df[i, "s1_cut"] <- out_sp$threshold_s1s1[[1]]
  sp_res_df[i, "mix_cut"] <- out_sp$threshold_s1s2[[1]]
  sp_res_df[i, "s2_cut"] <- out_sp$threshold_s2s2[[1]]
  sp_res_df[i, "s1_prop_all"] <- out_sp$prop_s1s1
  sp_res_df[i, "mix_prop_all"] <- out_sp$prop_s1s2
  sp_res_df[i, "s2_prop_all"] <- out_sp$prop_s2s2
  sp_res_df[i, "s1_prop_cut"] <- out_sp$prop_s1s1_below_quant
  sp_res_df[i, "mix_prop_cut"] <- out_sp$prop_s1s2_below_quant
  sp_res_df[i, "s2_prop_cut"] <- out_sp$prop_s2s2_below_quant
  
}


#write.csv(sp_res_df, "tests_sh/effect_of_params_on cut_offs/exp_assort_spatial_rp0.5_0.5_same_params.csv")

#### next scenario - reporting rates of 0.5 for s1 and 0.25 for s2

# Specify the reporting rates of the different species. 
s1_rr <- 0.5 # rr for dogs/species 1
s2_rr <- 0.25 # rr for jackals/species 2


## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean)


sp_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(sp_res_df) <- shape_vect
colnames(sp_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_sp <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                          params_s1s2 = params_s1s2,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  sp_res_df[i,"all_cut"] <- out_sp$threshold_sim[[1]]
  sp_res_df[i, "s1_cut"] <- out_sp$threshold_s1s1[[1]]
  sp_res_df[i, "mix_cut"] <- out_sp$threshold_s1s2[[1]]
  sp_res_df[i, "s2_cut"] <- out_sp$threshold_s2s2[[1]]
  sp_res_df[i, "s1_prop_all"] <- out_sp$prop_s1s1
  sp_res_df[i, "mix_prop_all"] <- out_sp$prop_s1s2
  sp_res_df[i, "s2_prop_all"] <- out_sp$prop_s2s2
  sp_res_df[i, "s1_prop_cut"] <- out_sp$prop_s1s1_below_quant
  sp_res_df[i, "mix_prop_cut"] <- out_sp$prop_s1s2_below_quant
  sp_res_df[i, "s2_prop_cut"] <- out_sp$prop_s2s2_below_quant
  
}


#write.csv(sp_res_df, "tests_sh/effect_of_params_on cut_offs/exp_assort_spatial_rp0.5_0.25_same_params.csv")

## Now 0.75 for dogs and 0.5 for jackals. Still same parameters for spatial kernel.

# Specify the reporting rates of the different species. 
s1_rr <- 0.75 # rr for dogs/species 1
s2_rr <- 0.50 # rr for jackals/species 2


## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean)

# We want to run a number of different values for the shape_2 parameter.
# number of values that we want to check
n_grid <- 10

shape_vect <- c(seq(1, 10.0, length.out = n_grid))

# change at higher values.
shape_vect

sp_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(sp_res_df) <- shape_vect
colnames(sp_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_sp <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                          params_s1s2 = params_s1s2,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  sp_res_df[i,"all_cut"] <- out_sp$threshold_sim[[1]]
  sp_res_df[i, "s1_cut"] <- out_sp$threshold_s1s1[[1]]
  sp_res_df[i, "mix_cut"] <- out_sp$threshold_s1s2[[1]]
  sp_res_df[i, "s2_cut"] <- out_sp$threshold_s2s2[[1]]
  sp_res_df[i, "s1_prop_all"] <- out_sp$prop_s1s1
  sp_res_df[i, "mix_prop_all"] <- out_sp$prop_s1s2
  sp_res_df[i, "s2_prop_all"] <- out_sp$prop_s2s2
  sp_res_df[i, "s1_prop_cut"] <- out_sp$prop_s1s1_below_quant
  sp_res_df[i, "mix_prop_cut"] <- out_sp$prop_s1s2_below_quant
  sp_res_df[i, "s2_prop_cut"] <- out_sp$prop_s2s2_below_quant
  
}

#write.csv(sp_res_df, "tests_sh/effect_of_params_on cut_offs/exp_assort_spatial_rp0.75_0.5_same_params.csv")

##### Now look at 0.5 and 0.5 with the S2S2 transmissions 5* the mean of the baseline

# Specify the reporting rates of the different species. 
s1_rr <- 0.50 # rr for dogs/species 1
s2_rr <- 0.50 # rr for jackals/species 2


## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean*5)

# We want to run a number of different values for the shape_2 parameter.
# number of values that we want to check


sp_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(sp_res_df) <- shape_vect
colnames(sp_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_sp <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                          params_s1s2 = params_s1s2,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  sp_res_df[i,"all_cut"] <- out_sp$threshold_sim[[1]]
  sp_res_df[i, "s1_cut"] <- out_sp$threshold_s1s1[[1]]
  sp_res_df[i, "mix_cut"] <- out_sp$threshold_s1s2[[1]]
  sp_res_df[i, "s2_cut"] <- out_sp$threshold_s2s2[[1]]
  sp_res_df[i, "s1_prop_all"] <- out_sp$prop_s1s1
  sp_res_df[i, "mix_prop_all"] <- out_sp$prop_s1s2
  sp_res_df[i, "s2_prop_all"] <- out_sp$prop_s2s2
  sp_res_df[i, "s1_prop_cut"] <- out_sp$prop_s1s1_below_quant
  sp_res_df[i, "mix_prop_cut"] <- out_sp$prop_s1s2_below_quant
  sp_res_df[i, "s2_prop_cut"] <- out_sp$prop_s2s2_below_quant
  
}


#write.csv(sp_res_df, "tests_sh/effect_of_params_on cut_offs/exp_assort_spatial_rp0.5_0.5_5meanS2.csv")

#Now with 0.5 and 0.25 and 5*mean for S2S2
# Specify the reporting rates of the different species. 
s1_rr <- 0.5 # rr for dogs/species 1
s2_rr <- 0.25 # rr for jackals/species 2


## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean*5)


sp_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(sp_res_df) <- shape_vect
colnames(sp_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_sp <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                          params_s1s2 = params_s1s2,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  sp_res_df[i,"all_cut"] <- out_sp$threshold_sim[[1]]
  sp_res_df[i, "s1_cut"] <- out_sp$threshold_s1s1[[1]]
  sp_res_df[i, "mix_cut"] <- out_sp$threshold_s1s2[[1]]
  sp_res_df[i, "s2_cut"] <- out_sp$threshold_s2s2[[1]]
  sp_res_df[i, "s1_prop_all"] <- out_sp$prop_s1s1
  sp_res_df[i, "mix_prop_all"] <- out_sp$prop_s1s2
  sp_res_df[i, "s2_prop_all"] <- out_sp$prop_s2s2
  sp_res_df[i, "s1_prop_cut"] <- out_sp$prop_s1s1_below_quant
  sp_res_df[i, "mix_prop_cut"] <- out_sp$prop_s1s2_below_quant
  sp_res_df[i, "s2_prop_cut"] <- out_sp$prop_s2s2_below_quant
  
}


#write.csv(sp_res_df, "tests_sh/effect_of_params_on cut_offs/exp_assort_spatial_rp0.5_0.25_5meanS2.csv")


#####Same process with the mean of S2S2 as twice the values of the other parameters and reporting as 0.75 and 0.5

# Specify the reporting rates of the different species. 
s1_rr <- 0.75 # rr for dogs/species 1
s2_rr <- 0.50 # rr for jackals/species 2


## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(rayleigh_mean)
params_s1s2 <- c(rayleigh_mean)
params_s2s2 <- c(rayleigh_mean*5)


sp_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(sp_res_df) <- shape_vect
colnames(sp_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_sp <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                          params_s1s2 = params_s1s2,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  sp_res_df[i,"all_cut"] <- out_sp$threshold_sim[[1]]
  sp_res_df[i, "s1_cut"] <- out_sp$threshold_s1s1[[1]]
  sp_res_df[i, "mix_cut"] <- out_sp$threshold_s1s2[[1]]
  sp_res_df[i, "s2_cut"] <- out_sp$threshold_s2s2[[1]]
  sp_res_df[i, "s1_prop_all"] <- out_sp$prop_s1s1
  sp_res_df[i, "mix_prop_all"] <- out_sp$prop_s1s2
  sp_res_df[i, "s2_prop_all"] <- out_sp$prop_s2s2
  sp_res_df[i, "s1_prop_cut"] <- out_sp$prop_s1s1_below_quant
  sp_res_df[i, "mix_prop_cut"] <- out_sp$prop_s1s2_below_quant
  sp_res_df[i, "s2_prop_cut"] <- out_sp$prop_s2s2_below_quant
  
}


#write.csv(sp_res_df, "tests_sh/effect_of_params_on cut_offs/exp_assort_spatial_rp0.75_0.5_5meanS2.csv")

