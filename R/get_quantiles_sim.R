#' Generate quantiles cut-off values and density using simulation
#'
#' This function takes information regarding the mean and sd of the distribution
#' of the serial interval or transmission distance and generates expected 
#' densities based on specified reporting rates
#' 
#' for serial interval the default is the gamma distribution, but the lognormal 
#'can be specified
#'
#' @author  \email{}
#'
#' @param ... a vector of parameters serving as input. 
#' For gamma distribution: param[1] = shape and param[2] = scale from rgamma
#' For lognormal distribution: param[1] = mean of data and param[2] = sd of data
#' The estimates for the meanlog and sdlog for the lognormal distribution and  
#' the shape and scale for the gamma distribution are calculated within
#'  the function
#' 
#' For spatial - param[1] is the mean of the sample data. The parameter of the 
#' Rayleigh distribution is calculated within the function
#'
#' @export
#'
#' @return a list of objects with attributes:
#'     'threshold_sim' (cut-off values for the quantiles) and 
#'     'density_sim' (density of the simulated distribution)
#'
#' @examples
#'


# d_type = "temporal"
# d_type = "spatial"
# n = 10000
# rrpi = 1
# si_log_mean = LN_mean
# si_log_sd = LN_SD
# q = quants
# dist_ray = rayleigh_scale


## Main function to compute the quantiles

get_quantiles_sim <- function(d_type, n, rrpi, q, distrib = "gamma", params) {
  
  if(d_type == "temporal"){
    
    if(distrib == "gamma"){
      
      #theta <- params[1]^2/params[2]
      #k <- params[1]/theta
      
      gam_parms <- epitrix::gamma_mucv2shapescale(mu = params[1], cv = params[2]/params[1])
      
      all_ind <- data.frame(Case_no = seq(1, n, 1),
                            distance = c(0,rgamma(n-1, shape = gam_parms$shape, scale = gam_parms$scale)), # adding location of (0,0) for first dog
                            detected = runif(n, 0, 1) <= rrpi)
      
    }
    
    if(distrib == "lognormal"){
      
      #sdlog <- sqrt(log(params[2]^2*exp(-2*log(params[1]))+1))
      #meanlog <- log(params[1]) - ((sdlog^2)/2)
      
      meanlog <- log(params[1]/(sqrt(1 + params[2]^2/params[1]^2)))
      sdlog <- sqrt(log(1 + params[2]^2/params[1]^2))
      
      
  all_ind <- data.frame(Case_no = seq(1, n, 1),
                        distance = c(0,rlnorm(n-1, meanlog = meanlog, sdlog = sdlog)), # adding location of (0,0) for first dog
                        detected = runif(n, 0, 1) <= rrpi)
    }
  
  # work out the cumulative SI
  all_ind$cum = cumsum(all_ind$distance)
  
  #Extract the case numbers of those observed
  Observed_ind <- which(all_ind$detected)
  
  observed_dist <- diff(all_ind$cum[Observed_ind])
  
  Output <- data.frame(Missing_gens = diff(Observed_ind), # more like k
                       observed_dist = observed_dist) # observed distances between observed animals
  
    
  threshold_sim <- quantile(Output$observed_dist, q)
  #density_sim <- density(Output$observed_dist, from = 0)
  density_sim <- density(Output$observed_dist, from = 0, n = round(max(Output$observed_dist)))
  
  
  } 
  
  if(d_type == "spatial") {
    
    ray_sig <- params[1] / sqrt(acos(-1)/2)
    
    all_ind <- data.frame(Case_no = seq(1, n, 1),
                          x_rel = c(0,rnorm(n-1,mean = 0, sd = ray_sig)), # adding location of (0,0) for first dog
                          y_rel = c(0,rnorm(n-1,mean = 0, sd = ray_sig)),
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
    
    ## Extract the quantile and density values that are produced 
    threshold_sim <- quantile(Output$observed_dist, q)
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
  
  return(res = list(threshold_sim = threshold_sim, density_sim = density_sim))
  
}

