

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
      
      all_ind <- all_ind[1:nrow(all_ind)-1,]
      
      all_ind$distance <- c(rgamma(n-1, shape = all_ind$shape, scale = all_ind$scale))
      all_ind[1,"distance"] <- 0
      
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
      
      all_ind <- all_ind[1:nrow(all_ind)-1,] # remove the last row with NA
      
      all_ind$distance <- c(rlnorm(n-1, meanlog = all_ind$meanlog, sdlog = all_ind$sdlog))
      all_ind[1,"distance"] <- 0
      
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
    
    all_ind <- all_ind[1:nrow(all_ind)-1,] # remove the last line as NA for transmission 
    
    all_ind$x_rel <- c(rnorm(n-1, mean = 0, sd = all_ind$ray_sig))
    all_ind$y_rel <- c(rnorm(n-1, mean = 0, sd = all_ind$ray_sig))
    
    all_ind[1,"x_rel"] <- 0
    all_ind[1,"y_rel"] <- 0
    
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
  
  f_s1s1_below_quant <- which(obs$obs_trans == "s1s1" & obs$diff <= threshold_sim)
  f_s2s2_below_quant <- which(obs$obs_trans == "s2s2" & obs$diff <= threshold_sim)
  f_s1s2_below_quant <- which(obs$obs_trans == "s1s2" & obs$diff <= threshold_sim)
  f_s2s1_below_quant <- which(obs$obs_trans == "s2s1" & obs$diff <= threshold_sim)
  
  #work out the proportion of each type of transmission
  
  n_below_quant <- length(which(obs$diff <= threshold_sim))
  
  prop_s1s1_below_quant <- length(f_s1s1_below_quant)/(n_below_quant)
  prop_s1s2_below_quant <- length(f_s1s2_below_quant)/(n_below_quant)
  prop_s2s1_below_quant <- length(f_s2s1_below_quant)/(n_below_quant)
  prop_s2s2_below_quant <- length(f_s2s2_below_quant)/(n_below_quant)
  
  return(res = list(f_s1s1 = f_s1s1, f_s1s2 = f_s1s2, f_s2s1 = f_s2s1, f_s2s2 = f_s2s2,
                    prop_s1s1 = prop_s1s1, prop_s1s2 = prop_s1s2,
                    prop_s2s1 = prop_s2s1, prop_s2s2 = prop_s2s2,
                    threshold_sim = threshold_sim, density_sim = density_sim, 
                    threshold_s1s1 = threshold_s1s1, density_s1s1 = density_s1s1,
                    threshold_s2s2 = threshold_s2s2, density_s2s2 = density_s2s2, 
                    threshold_s1s2 = threshold_s1s2, density_s1s2 = density_s1s2,
                    threshold_s2s1 = threshold_s2s1, density_s2s1 = density_s2s1,
                    f_s1s1_below_quant = f_s1s1_below_quant, f_s1s2_below_quant = f_s1s2_below_quant,
                    f_s2s1_below_quant = f_s2s1_below_quant, f_s2s2_below_quant = f_s2s2_below_quant, 
                    prop_s1s1_below_quant = prop_s1s1_below_quant, prop_s1s2_below_quant = prop_s1s2_below_quant,
                    prop_s2s1_below_quant = prop_s2s1_below_quant, prop_s2s2_below_quant = prop_s2s2_below_quant))
  
}

