
## The function get_quantiles_sim is [[a wrapper around 2 non exported functions
## which approximate quantiles for a given probability density / mass
## function. The main function is get_quantiles_pdf, on which the 'pmf'
## equivalent relies. It creates a 1D regular grid iteratively, which it uses to
## compute the cummulative density/mass, until it reaches the highest quantile
## requested. This grid has a mesh resolution of 'precision' and a size
## 'n_steps'.]]

# d_type = "temporal"
# d_type = "spatial"
# n = 10000
# rrpi = 1
# si_log_mean = LN_mean
# si_log_sd = LN_SD
# q = quants
# dist_ray = rayleigh_scale


## Main function to compute the quantiles

get_density_sim <- function(d_type, n, rrpi, si_log_mean, si_log_sd, q, dist_ray) {
  
  if(d_type == "temporal"){
    
    all_ind <- data.frame(Case_no = seq(1, n, 1),
                          distance = c(0,rlnorm(n-1, meanlog = si_log_mean, sdlog = si_log_sd)), # adding location of (0,0) for first dog
                          detected = runif(n, 0, 1) <= rrpi)
    
    # work out the cumulative SI
    all_ind$cum = cumsum(all_ind$distance)
    
    #Extract the case numbers of those observed
    Observed_ind <- which(all_ind$detected)
    
    observed_dist <- diff(all_ind$cum[Observed_ind])
    
    Output <- data.frame(Missing_gens = diff(Observed_ind), # more like k
                         observed_dist = observed_dist) # observed distances between observed animals
    
  #  density_sim <- density(Output$observed_dist, from = 0, n = round(max(Output$observed_dist)))
    density_sim <- density(Output$observed_dist, from = 0)
  } 
  
  if(d_type == "spatial") {
    all_ind <- data.frame(Case_no = seq(1, n, 1),
                          x_rel = c(0,rnorm(n-1,mean = 0, sd = dist_ray)), # adding location of (0,0) for first dog
                          y_rel = c(0,rnorm(n-1,mean = 0, sd = dist_ray)),
                          detected = runif(n, 0, 1) <= rrpi)
    
    # all distances
    all_ind$True_dist <- sqrt(all_ind$x_rel^2+all_ind$y_rel^2)  # using trigonometry of a^2 + b^2 = c^2
    
    # absolute positions - need to work out the actual position of each animal rather than position relative to others which is what we have until now
    all_ind$x_abs = cumsum(all_ind$x_rel)
    all_ind$y_abs = cumsum(all_ind$y_rel)
    
    
    #Extract the case numbers of those observed
    Observed_ind <- which(all_ind$detected)
    
    ## Set up a vector of the positions observed
    new_relative_position <- cbind(diff(all_ind$x_abs[Observed_ind]),
                                   diff(all_ind$y_abs[Observed_ind]))
    
    Output <- data.frame(Missing_gens = diff(Observed_ind), # more like k
                         observed_dist = sqrt(rowSums(new_relative_position^2))) # observed distances between observed animals
    
    ## Extract the density that our method produces.
    density_sim <- density(Output$observed_dist, from = 0)
    
  }
  
  
  # Check for convergence
  # check how 1st half differ from the 2nd half
  Q_convergence <- c(quantile(Output$observed_dist[1:round(nrow(Output)/2)],.99),
                     quantile(Output$observed_dist[round(nrow(Output)/2):nrow(Output)],.99))
  
  rel_diff <- abs(diff(Q_convergence)/Q_convergence[2])
  warning_message <- paste0('if running ',round(n/2),' iterations instead of ',n,' the 99%CI threshold',
                            ' would be more than ',round(rel_diff,digits = 3)*100,'% different;',
                            ' not sure of convergence. Consider increasing n')
  if(abs(diff(Q_convergence)/Q_convergence[2]) > .001) warning(warning_message)
  
  return(density_sim)
  
}

