## functions used in getting f_temporal when we have the lognoraml distribution

rm(list=ls())
library(devtools)
library(vimes)
library(fields)
library(dplyr)

source("tests_sh/prep_data_for_vimes.R")

# Bring in the data we are going to use
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])

D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix

head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. 

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
D_all <- vimes_data(dates = D_dates, geo = D_geo)

max(D_dates)
date_vect <- seq(0,max(D_dates), 1)


# the p we use in our work is defined below.

si_mean <- 27.8175438596491
si_sd <- 26.8565433014125

LN_mean <- log(si_mean/(sqrt(1 + si_sd^2/si_mean^2)))
LN_SD <- sqrt(log(1 + si_sd^2/si_mean^2))


#f_temporal <- fpaircase(type = "empiric", p = dlnorm(x = date_vect, meanlog = LN_mean, 
#                                                     sdlog = LN_SD, log = FALSE))

p = dlnorm(x = date_vect, meanlog = LN_mean, sdlog = LN_SD, log = FALSE)

fpaircase
dpaircase

# follow down through the above functions to dempiric. 

#dempiric function below - go through stage by stage
pi = 1 # reporting rate
alpha = 0.001 # precision on the kappa term

# function(p, pi, alpha = 0.001) {
  pi <- vimes:::check_one_proba(pi)
  alpha <- vimes:::check_one_proba(alpha)
  p <- vimes:::check_pmf(p)
  
  max_kappa <- vimes:::get_max_kappa(pi, alpha)
  weights <- vimes:::get_weights(pi, max_kappa)
  distributions <- vimes:::convolve_empirical(p, max_kappa, TRUE)
  
  out <- distributions %*% weights
  #return(as.vector(out))
#}

#plot(out)

# vimes:::convolve_empirical # this is used for log normal.
# function(x, kappa, keep_all = FALSE) {
 
   ####___ CHECK WHAT X IS .
  
  kappa <- check_kappa(kappa, only_one = TRUE)
  
  if (kappa == 1) {
    if (keep_all) {
      x <- matrix(x)
      colnames(x) <- "1"
    }
    return(x)
  }
  
  
  ## Computations should be checked by Anne
  
  if (keep_all) {
    out <- list()
    out[[1]] <- x
    
    for (k in 2:kappa) {
      out[[k]] <- stats::convolve(out[[k-1]],
                                  rev(x),
                                  type="open")
    }
    
    
    ## Here we shape the output as a data.frame where each column is a different
    ## value of kappa.
    
    L <- length(out[[kappa]])
    out <- lapply(out, fill_with, 0, L)
    out <- as.matrix(data.frame(out))
    colnames(out) <- seq_len(ncol(out))
  } else {
    out <- x
    
    for (k in 2:kappa) {
      out <- stats::convolve(out,
                             rev(x),
                             type="open")
    }
  }
  return(out)
}