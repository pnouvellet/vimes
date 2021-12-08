# Comparing the cut-off values that are generated when we use a weighted mean in vimes
# versus when we use the different reporting probabilities in the two species simulation. 

rm(list=ls())

library(devtools)
library(vimes)

# Bring in the data we are going to use
source("tests_sh/prep_data_for_vimes.R")

## select the complete cases for time. 
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])
plot(new)


D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix

library(fields)
head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. 

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
D_all <- vimes_data(dates = D_dates, geo = D_geo)

# Defining cutoff distances above which cases are considered not linked by transmission 

## Distributions of expected distances between cases for rabies
## Using the distribution parameters that I found from Serengeti data set
## For the serial interval this was a log normal
si_mean <- 27.8175438596491
si_sd <- 26.8565433014125
rayleigh_mean <- 0.88

rayleigh_scale <- rayleigh_mean / sqrt(acos(-1)/2)

max(D_dates)
date_vect <- seq(0,max(D_dates), 1)

## gamma parameters
gam_parms <- epitrix::gamma_mucv2shapescale(mu = si_mean,
                                                 cv = si_sd/si_mean)

gam_parms

##lognormal distribution
LN_mean <- log(si_mean/(sqrt(1 + si_sd^2/si_mean^2)))
LN_SD <- sqrt(log(1 + si_sd^2/si_mean^2))


f_temporal_gam <- fpaircase(type = "temporal", gamma_shape = gam_parms[[1]], gamma_scale = gam_parms[[2]], 
                             alpha = 0.00001)

f_temporal_ln <- fpaircase(type = "empiric", p = dlnorm(x = date_vect, meanlog = LN_mean, 
                                                     sdlog = LN_SD, log = FALSE), alpha = 0.00001)

f_spatial <- fpaircase(type = "spatial", sd_spatial = rayleigh_scale, alpha = 0.00001)


## work out the weighted mean reporting probabilities
dog_rr <- c(1, 0.8, 0.6, 0.4, 0.2)
jackal_rr <- c(1, 0.8, 0.6, 0.4, 0.21)

ndogs <- 313
njacks <- 236
ntotal <- ndogs + njacks

probs_df <- as.data.frame(matrix(ncol = length(dog_rr), nrow = length(jackal_rr)))

for(i in 1:length(dog_rr)){
  for (j in 1:length(jackal_rr)) {
    probs_df[j,i] <- ntotal/(ndogs/dog_rr[i] + njacks/jackal_rr[j])
  }
}

#write.csv(probs_df, "tests_sh/props_to_test.csv")

colnames(probs_df) <- dog_rr
rownames(probs_df) <- jackal_rr

### df for the results
out_df <- as.data.frame(matrix(ncol = length(dog_rr), nrow= length(jackal_rr)))

for (i in 1:length(dog_rr)) {
  for (j in 1:length(jackal_rr))
    out_df[j,i] <- get_quantiles(f_temporal_gam,  pi = probs_df[j,i], 
                                 0.95,  precision = 0.00001, maxit = 10000, n_steps = 10000)
  
}

#tictoc::tic()
#out_df[5,5] <- get_quantiles(f_temporal_gam,  pi = probs_df[5,5], 
#                             0.95,  precision = 0.00001, maxit = 10000, n_steps = 10000)
#tictoc::toc()

view(out_df)

#write.csv(out_df, "tests_sh/weighted_mean_results/vimes_res_si_gamma.csv")


### repeat for the lognormal distribution
out_df_ln <- as.data.frame(matrix(ncol = length(dog_rr), nrow= length(jackal_rr)))

for (i in 1:length(dog_rr)) {
  for (j in 1:length(jackal_rr))
    out_df_ln[j,i] <- get_quantiles(f_temporal_ln,  pi = probs_df[j,i], 
                                 0.95,  precision = 0.00001, maxit = 10000, n_steps = 10000)
  
}

#write.csv(out_df_ln, "tests_sh/weighted_mean_results/vimes_res_si_ln.csv")

# And repeat for spatial

out_df_sp <- as.data.frame(matrix(ncol = length(dog_rr), nrow= length(jackal_rr)))

for (i in 1:length(dog_rr)) {
  for (j in 1:length(jackal_rr))
    out_df_sp[j,i] <- get_quantiles(f_spatial,  pi = probs_df[j,i], 
                                    0.95,  precision = 0.00001, maxit = 10000, n_steps = 10000)
  
}

#write.csv(out_df_sp,  "tests_sh/weighted_mean_results/vimes_res_spatial.csv")


##### And get the results from the simulation
sim_res_df <- as.data.frame(matrix(ncol = length(dog_rr), nrow = length(jackal_rr)))
colnames(sim_res_df) <- dog_rr
rownames(sim_res_df) <- jackal_rr

for (i in 1:length(dog_rr)) {
  for (j in 1:length(jackal_rr)) {
    
    sim_res_df[j,i] <- get_quantiles_multi(d_type = "temporal", distrib = "gamma", 
                                           s1_obs = s1_obs, s2_obs = s2_obs,
                                           s1_rr = dog_rr[i], s2_rr = jackal_rr[j],
                                           params_s1s1 = params_s1s1, params_s1s2 = params_s1s2, 
                                           params_s2s2 = params_s2s2, 
                                           q = 0.95, n = n)$threshold_sim[[1]]
    
    
  }
  
}

#write.csv(sim_res_df, "tests_sh/weighted_mean_results/sim_res_si_gamma.csv")


sim_res_df_ln <- as.data.frame(matrix(ncol = length(dog_rr), nrow = length(jackal_rr)))
colnames(sim_res_df_ln) <- dog_rr
rownames(sim_res_df_ln) <- jackal_rr

for (i in 1:length(dog_rr)) {
  for (j in 1:length(jackal_rr)) {
    
    sim_res_df_ln[j,i] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal", 
                                              s1_obs = s1_obs, s2_obs = s2_obs,
                                              s1_rr = dog_rr[i], s2_rr = jackal_rr[j],
                                              params_s1s1 = params_s1s1, params_s1s2 = params_s1s2, 
                                              params_s2s2 = params_s2s2, 
                                              q = 0.95, n = n)$threshold_sim[[1]]
    
    
  }
  
}

# write.csv(sim_res_df_ln, "tests_sh/weighted_mean_results/sim_res_si_ln.csv")


sim_res_df_sp <- as.data.frame(matrix(ncol = length(dog_rr), nrow = length(jackal_rr)))
colnames(sim_res_df_sp) <- dog_rr
rownames(sim_res_df_sp) <- jackal_rr

## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1_sp <- c(rayleigh_mean)
params_s1s2_sp <- c(rayleigh_mean)
params_s2s2_sp <- c(rayleigh_mean)


for (i in 1:length(dog_rr)) {
  for (j in 1:length(jackal_rr)) {
    
    sim_res_df_sp[j,i] <- get_quantiles_multi(d_type = "spatial",  
                                              s1_obs = s1_obs, s2_obs = s2_obs,
                                              s1_rr = dog_rr[i], s2_rr = jackal_rr[j],
                                              params_s1s1 = params_s1s1_sp, params_s1s2 = params_s1s2_sp, 
                                              params_s2s2 = params_s2s2_sp, 
                                              q = 0.95, n = n)$threshold_sim[[1]]
    
    
  }
  
}

#write.csv(sim_res_df_sp, "tests_sh/weighted_mean_results/sim_res_spatial.csv")
