
# Based on an online vimes vignette. Workflow to allow comparison to simulation method. 

rm(list=ls())
#install.packages("devtools")
library(devtools)
#install_github("reconhub/vimes")
library(vimes)
library(fields)
library(dplyr)

source("tests_sh/prep_data_for_vimes.R")


# Bring in the data we are going to use
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])

# Computing pairwise distances between cases in time, space,

#Distances between dates are computed as numbers of days:
head(vd$Time_diff, 5)
D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix

## Distances between locations are computed using the great circle distance:
## The great-circle distance or orthodromic distance is the shortest distance
## between two points on the surface of a sphere, measured along the surface 
## of the sphere (as opposed to a straight line through the sphere's interior).

head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. 

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
D_all <- vimes_data(dates = D_dates, geo = D_geo)
par(mar = c(2,2,2,2))

plot(D_all, nclass = 60)


# Defining cutoff distances above which cases are considered not linked by transmission 

# Set the values for the parameters

# Distributions of expected distances between cases for rabies. Using the distribution parameters from Serengeti data set. For the serial interval this was a log normal

#LN_mean <- 2.82
#LN_SD <- 0.954

si_mean <- 27.8175438596491
si_sd <- 26.8565433014125

si_cv <- si_sd/si_mean

gam_parms <- epitrix::gamma_mucv2shapescale(mu = si_mean, cv = si_cv)

## spatial kernel parameters
## from wikipedia: In probability theory and statistics, 
##the Rayleigh distribution is a continuous probability distribution for positive-valued 
##random variables. It is a chi distribution in two degrees of freedom.
##A Rayleigh distribution is often observed when the overall magnitude of a vector 
##is related to its directional components.

rayleigh_mean <- 0.88
## find Rayleigh parameters to match this mean
rayleigh_scale <- rayleigh_mean / sqrt(acos(-1)/2)

# We use the `fpaircase` function to compute the distributions of expected distances (temporal, spatial) between a case and their infector based on the above parameters.

## distance functions for each of the 2 types
## set up a vector of the same length as the maximum pairwise distance of the SIs.
## Will need this to use within fpaircase when defining 'p'
max(D_dates)
date_vect <- seq(0,max(D_dates), 1)


f_temporal <- fpaircase(type = "temporal", gamma_shape = gam_parms[[1]], gamma_scale = gam_parms[[2]])

f_spatial <- fpaircase(type = "spatial", sd_spatial = rayleigh_scale)

# Plots of the distributions
par(mfrow=c(2,1))
plot(f_temporal, xlim = c(0,365))
plot(f_spatial, xlim = c(0,5))


#Cutoff distances defined as the quantiles of the distributions of expected pairwise distances
#We will be using the above distributions of expected distances between a case and their infector to define cutoff distances above which pairs of cases are considered not linked by transmission.

#First, we assume a certain level of reporting.

## reporting rate
pi <- 1.0
## quantiles
q <- c(.50, .75, .90, .95, .99)

## colours used to plot these
cols <- rainbow(length(q))
# We now plot the distributions of expected distances (temporal, spatial) between a case and their closest observed ancestor, assuming that only pi% of cases are reported. We also show the cutoffs corresponding to the quantiles defined above.

par(mfrow=c(2,1))
plot(f_temporal, q, xlim = c(0,365), pi = pi,
     lines_arg = list(col=cols, lwd=2))
plot(f_spatial, q, xlim = c(0,15), pi = pi,
     lines_arg = list(col=cols, lwd=2))

#We can overlay the graph above on top of the histogram of observed distances to see where these cutoffs fall with respect to our observations.

## function used to generate a plot with distribution of observed and expected distances

plot_overlay <- function(dist, f, q, pi, xlab, breaks, resol = 1,
                         q_color = cols, hist_bordercolor = "grey",
                         hist_color = "lightgrey"){
  
  ## dist contains the observed distances
  ## f is the distribution of expected distances between a case and their infector
  ## q is the quantile or vector of quantiles of interest
  ## pi is the reporting rate
  ## xlab is the x axis label
  ## breaks is the breaks used for plotting the histogram of dist
  ## resol is the resolution used for plotting f
  ## q_color is the colour or vector of colours used to show the cutofs associated with the quantile(s) q
  ## hist_bordercolor is the colour used for the border of the histogram of dist
  ## hist_color is the colour used for the filling of the histogram of dist
  
  qtl <- get_quantiles(f, q, pi = pi)
  hist(dist, col = hist_color, border = hist_bordercolor, 
       main = "", xlab = xlab, 
       breaks = breaks)
  par(new = TRUE)
  x <- seq(min(breaks), max(breaks), resol)
  y <- f(x, pi = pi)
  plot(x, y, type = "l", axes = FALSE, main = "", xlab = "", ylab = "")
  ## add vertical lines corresponding to quantiles
  abline(v = qtl, col = q_color, lwd = 2)
}

# use the function above to create our plot: 
par(mfrow=c(2, 1), mar=c(5, 5, 0.5, 5))
## temporal
plot_overlay(dist = as.vector(D_all$dates), 
             f = f_temporal, 
             q = q, 
             pi = pi, 
             xlab = "Pairwise distance in time (days)", 
             breaks = seq(0,3500, 50),
             resol = 1)
## spatial
plot_overlay(dist = as.vector(D_all$geo), 
             f = f_spatial, 
             q = q, 
             pi = pi, 
             xlab = "Pairwise distance in space (km)", 
             breaks = seq(0,350,5),
             resol = 0.1)


#Just getting the quantiles - set a high level of precision
#tictoc::tic()
#qtl <- get_quantiles(f_temporal, q, pi = pi, precision = 0.00001, n_steps = 14000000)
#tictoc::toc()
#qtl

#tictoc::tic()
#qtl_sp <- get_quantiles(f_spatial, q, pi = pi, precision = 0.0000001, n_steps = 90000000)
#tictoc::toc()
#qtl_sp


###### Produce a table with all the values in it. 

pi_range <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0.1)
q <- c(.50, .75, .90, .95, .99)


si_table_vimes_gamma <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(si_table_vimes_gamma) <- q
rownames(si_table_vimes_gamma) <- pi_range
#Just getting the quantiles

tictoc::tic()
for(i in 1:length(pi_range)){
  si_table_vimes_gamma[i,] <- get_quantiles(f_temporal, q, pi = pi_range[i], 
                                            precision = 0.00001, n_steps = 1000000)
  }
tictoc::toc()

#write.csv(si_table_vimes_gamma, "tests_sh/vimes_si_gamma.csv")

# Produce a table for the distance kernel

dist_table_vimes <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(dist_table_vimes) <- q
rownames(dist_table_vimes) <- pi_range
#Just getting the quantiles

tictoc::tic()
for(i in 1:length(pi_range)){
  dist_table_vimes[i,] <- get_quantiles(f_spatial, q, pi = pi_range[i],
                                        precision = 0.0000001, n_steps = 900000)
}
tictoc::toc()
#write.csv(dist_table_vimes, "tests_sh/vimes_distance.csv")
