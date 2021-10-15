### Multi-species function first look

## number of observed cases for each species
s1_obs <- 303 # no of cases observed for dogs/species 1
s2_obs <- 221 # no of cases observed for jackals/species 2

## Specify the reporting rates of the different species. 
s1_rr <- 0.40 # rr for dogs/species 1
s2_rr <- 0.60 # rr for jackals/species 2

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
params_s2s1 <- c(si_mean, si_sd)
params_s2s2 <- c(si_mean, si_sd)


#params_s1s1 <- c(rayleigh_mean)
#params_s1s2 <- c(rayleigh_mean)
#params_s2s1 <- c(rayleigh_mean)
#params_s2s2 <- c(rayleigh_mean)


## Should I set the first animals as zero? Meed to look at that

get_quantiles_multi <- function(d_type, distrib, s1_obs, s2_obs, s1_rr, s2_rr, n, 
                                params_s1s1, params_s1s2, params_s2s1, params_s2s2, q) {
  
  ## Calculate the actual number of cases for each species
  s1_n <- s1_obs/s1_rr
  s2_n <- s2_obs/s2_rr
  
  # now use this to calculate the proportions of each species 
  s1_prop <- s1_n/(s1_n+s2_n)
  #s2_prop <- s2_n/(s1_n+s2_n)
  s2_prop <- 1 - s1_prop
  
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
  
  reporting <- c(s1_rr, s2_rr)
  
  all_ind <- data.frame(species = sample(x = c('s1','s2'), size = n, 
                                         replace = TRUE, prob = c(s1_prop, s2_prop)))
  all_ind$detected <- runif(n= n, 0, 1) < ((all_ind$species == 's1') * reporting[1] +
                                             (all_ind$species == 's2') * reporting[2])
  all_ind$trans <- c(paste(all_ind$species[1:n-1], all_ind$species[2:n], sep = ""),NA)
  
  
  if(d_type == "temporal"){
    
    if(distrib == "gamma"){
      
      gam_parms_s1s1 <- epitrix::gamma_mucv2shapescale(mu = params_s1s1[1],
                                                       cv = params_s1s1[2]/params_s1s1[1])
      gam_parms_s1s2 <- epitrix::gamma_mucv2shapescale(mu = params_s1s2[1],
                                                       cv = params_s1s2[2]/params_s1s1[1])
      gam_parms_s2s1 <- epitrix::gamma_mucv2shapescale(mu = params_s2s1[1],
                                                       cv = params_s2s1[2]/params_s2s1[1])
      gam_parms_s2s2 <- epitrix::gamma_mucv2shapescale(mu = params_s2s2[1],
                                                       cv = params_s2s2[2]/params_s2s2[1])
      
      # all_ind <- data.frame(species = sample(x = c('s1','s2'), size = n, 
      #                                        replace = TRUE, prob = c(s1_prop, s2_prop)))
      # all_ind$obs <- runif(n= n, 0, 1) < ((all_ind$species == 's1') * reporting[1] +
      #                                       (all_ind$species == 's2') * reporting[2])
      # all_ind$trans <- c(paste(all_ind$species[1:n-1], all_ind$species[2:n], sep = ""),NA)
      
      # add a column with the parameters for the type of transmission
      shape_vect <- c(gam_parms_s1s1[1], gam_parms_s1s2[1], gam_parms_s2s1[1], gam_parms_s2s2[1]) # s1s1,s1s2,s2s1,s2s2
      scale_vect <- c(gam_parms_s1s1[2], gam_parms_s1s2[2], gam_parms_s2s1[2], gam_parms_s2s2[2])
      
      all_ind$shape <- (all_ind$trans == 's1s1') * shape_vect[[1]] + 
        (all_ind$trans == 's1s2') * shape_vect[[2]] + 
        (all_ind$trans == 's2s1') * shape_vect[[3]] + 
        (all_ind$trans == 's2s2') * shape_vect[[4]]
      
      all_ind$scale <- (all_ind$trans == 's1s1') * scale_vect[[1]] + 
        (all_ind$trans == 's1s2') * scale_vect[[2]] + 
        (all_ind$trans == 's2s1') * scale_vect[[3]] + 
        (all_ind$trans == 's2s2') * scale_vect[[4]]
      
      all_ind$distance <- c(rgamma(n, shape = all_ind$shape, scale = all_ind$scale))
      
      #all_ind <- data.frame(Case_no = seq(1, n, 1),
      #                      distance = c(rgamma(n, shape = gam_parms$shape, scale = gam_parms$scale))) 
      
    }
    
    if(distrib == "lognormal"){
      
      meanlog_s1s1 <- log(params_s1s1[1]/(sqrt(1 + params_s1s1[2]^2/params_s1s1[1]^2)))
      sdlog_s1s1 <- sqrt(log(1 + params_s1s1[2]^2/params_s1s1[1]^2))
      
      meanlog_s1s2 <- log(params_s1s2[1]/(sqrt(1 + params_s1s2[2]^2/params_s1s2[1]^2)))
      sdlog_s1s2 <- sqrt(log(1 + params_s1s2[2]^2/params_s1s2[1]^2))
      
      meanlog_s2s1 <- log(params_s2s1[1]/(sqrt(1 + params_s2s1[2]^2/params_s2s1[1]^2)))
      sdlog_s2s1 <- sqrt(log(1 + params_s2s1[2]^2/params_s2s1[1]^2))
      
      meanlog_s2s2 <- log(params_s2s2[1]/(sqrt(1 + params_s2s2[2]^2/params_s2s2[1]^2)))
      sdlog_s2s2 <- sqrt(log(1 + params_s2s2[2]^2/params_s2s2[1]^2))
      
      meanlog_vect <- c(meanlog_s1s1, meanlog_s1s2, meanlog_s2s1, meanlog_s2s2) # s1s1,s1s2,s2s1,s2s2
      sdlog_vect <- c(sdlog_s1s1, sdlog_s1s2, sdlog_s2s1, sdlog_s2s2)
      
      all_ind$meanlog <- (all_ind$trans == 's1s1') * meanlog_vect[[1]] + 
        (all_ind$trans == 's1s2') * meanlog_vect[[2]] + 
        (all_ind$trans == 's2s1') * meanlog_vect[[3]] + 
        (all_ind$trans == 's2s2') * meanlog_vect[[4]]
      
      all_ind$sdlog <- (all_ind$trans == 's1s1') * sdlog_vect[[1]] + 
        (all_ind$trans == 's1s2') * sdlog_vect[[2]] + 
        (all_ind$trans == 's2s1') * sdlog_vect[[3]] + 
        (all_ind$trans == 's2s2') * sdlog_vect[[4]]
      
      all_ind$distance <- c(rlnorm(n, meanlog = all_ind$meanlog, sdlog = all_ind$sdlog))
      
    }
  
    # work out the cumulative SI
    all_ind$cum = cumsum(all_ind$distance)
    
  ## Separate out the different types of transmission
    obs <- all_ind[which(all_ind$detected == TRUE),] # select just the detected cases
    obs$diff <- c(diff(obs$cum), NA) # we don't have the species for the final transmission 
    # as don't know what this would be so use NA - assume animal 1 - animal 2 and the SI for 2 relates to this transmission 
    
    obs_n <- nrow(obs)
    obs$obs_trans <- NA
    obs$obs_trans <- c(paste(obs$species[1:obs_n-1], obs$species[2:obs_n], sep = ""),NA)
  
}
    
  if(d_type == "spatial") {
    
    ray_sig_s1s1 <- params_s1s1 / sqrt(acos(-1)/2)
    ray_sig_s1s2 <- params_s1s2 / sqrt(acos(-1)/2)
    ray_sig_s2s1 <- params_s2s1 / sqrt(acos(-1)/2)
    ray_sig_s2s2 <- params_s2s2 / sqrt(acos(-1)/2)
    
    ray_sig_vect <- c(ray_sig_s1s1, ray_sig_s1s2, ray_sig_s2s1, ray_sig_s2s2) # s1s1,s1s2,s2s1,s2s2
    
    all_ind$ray_sig <- (all_ind$trans == 's1s1') * ray_sig_vect[[1]] + 
      (all_ind$trans == 's1s2') * ray_sig_vect[[2]] + 
      (all_ind$trans == 's2s1') * ray_sig_vect[[3]] + 
      (all_ind$trans == 's2s2') * ray_sig_vect[[4]]
    
    all_ind$x_rel <- c(rnorm(n, mean = 0, sd = all_ind$ray_sig))
    all_ind$y_rel <- c(rnorm(n, mean = 0, sd = all_ind$ray_sig))
    
    # all distances
    all_ind$True_dist <- sqrt(all_ind$x_rel^2+all_ind$y_rel^2)  # using trigonometry of a^2 + b^2 = c^2
    
    # absolute positions - need to work out the actual position of each animal rather than position relative to others which is what we have until now
    all_ind$x_abs = cumsum(all_ind$x_rel)
    all_ind$y_abs = cumsum(all_ind$y_rel)
    
    #Extract the case numbers of those observed
    ##************ Check this bit with Pierre
    
    obs <- all_ind[which(all_ind$detected == TRUE),] # select just the detected cases
    obs$new_relative_position_x <- c(diff(obs$x_abs),NA)
    obs$new_relative_position_y <- c(diff(obs$y_abs), NA)
     
    obs$diff <- c(sqrt(obs$new_relative_position_x^2+obs$new_relative_position_y^2))
    # as don't know what this would be so use NA - assume animal 1 - animal 2 and the dist for 2 relates to this transmission 
    
    obs_n <- nrow(obs)
    obs$obs_trans <- NA
    obs$obs_trans <- c(paste(obs$species[1:obs_n-1], obs$species[2:obs_n], sep = ""),NA)
    
  }
 
  f_s1s1<- which(obs$obs_trans == "s1s1")
  f_s2s2<- which(obs$obs_trans == "s2s2")
  f_s1s2<- which(obs$obs_trans == "s1s2")
  f_s2s1<- which(obs$obs_trans == "s2s1")
  
  #work out the proportion of each type of transmission
  
  prop_s1s1 <- length(f_s1s1)/(obs_n-1)
  prop_s1s2 <- length(f_s1s2)/(obs_n-1)
  prop_s2s1 <- length(f_s2s1)/(obs_n-1)
  prop_s2s2 <- length(f_s2s2)/(obs_n-1)
  
# prop_total <- prop_s1s1+prop_s1s2+prop_s2s1+prop_s2s2
#  if(prop_total != 1) {
#    msg3 <- "Proportions of transmissions do not sum to 1"
#    stop(msg3)
#  }
  
  
  threshold_sim <- quantile(obs$diff, q, na.rm = T)
  threshold_s1s1 <- quantile(obs[f_s1s1, "diff"], q, na.rm = T)
  threshold_s2s2 <- quantile(obs[f_s2s2, "diff"], q, na.rm = T)
  threshold_s1s2 <- quantile(obs[f_s1s2, "diff"], q, na.rm = T)
  threshold_s2s1 <- quantile(obs[f_s2s1, "diff"], q, na.rm = T)
  
  density_sim <- density(obs$diff, na.rm = T, from = 0, n = round(max(obs$diff, na.rm = T)))
  density_s1s1 <- density(obs[f_s1s1, "diff"],
                          na.rm = T, from = 0, n = round(max(obs$diff, na.rm = T)))
  density_s2s2 <- density(obs[f_s2s2, "diff"],
                          na.rm = T, from = 0, n = round(max(obs$diff, na.rm = T)))
  density_s1s2 <- density(obs[f_s1s2, "diff"], 
                          na.rm = T, from = 0, n = round(max(obs$diff, na.rm = T)))
  density_s2s1 <- density(obs[f_s2s1, "diff"], 
                          na.rm = T, from = 0, n = round(max(obs$diff, na.rm = T)))
  
  
  return(res = list(f_s1s1 = f_s1s1, f_s1s2 = f_s1s2, f_s2s1 = f_s2s1, f_s2s2 = f_s2s2,
                    prop_s1s1 = prop_s1s1, prop_s1s2 = prop_s1s2,
                    prop_s2s1 = prop_s2s1, prop_s2s2 = prop_s2s2,
                    threshold_sim = threshold_sim, density_sim = density_sim, 
                    threshold_s1s1 = threshold_s1s1, density_s1s1 = density_s1s1,
                    threshold_s2s2 = threshold_s2s2, density_s2s2 = density_s2s2, 
                    threshold_s1s2 = threshold_s1s2, density_s1s2 = density_s1s2,
                    threshold_s2s1 = threshold_s2s1, density_s2s1 = density_s2s1
  ))
  
}

## Pierre we had a convergence check in the single species model. Put something like that in here?

## LEt's try the function
tictoc::tic()
out_si_gamma <- get_quantiles_multi(d_type = "temporal", distrib = "gamma", 
                                    s1_obs = s1_obs, s2_obs = s2_obs,
                                    s1_rr = 0.4, s2_rr = 0.4,
                                    params_s1s1 = params_s1s1, params_s1s2 = params_s1s2,
                                    params_s2s1 = params_s2s1, params_s2s2 = params_s2s2,
                                    n = 1000000, q = q)

tictoc::toc()


out_si_gamma$prop_s1s1
out_si_gamma$prop_s1s2
out_si_gamma$prop_s2s1
out_si_gamma$prop_s2s2



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
