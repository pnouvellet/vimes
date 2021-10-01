
#Running through a workflow for vimes using the new simulated function rather than the inbuilt vimes functions. Produced to enable comparison of outputs to original methods. 
# Going to use the gamma distribution for comparison

#Firstly we bring in and prepare the data to be in a suitable format. 

rm(list=ls())
#install.packages("devtools")
library(devtools)
#install_github("reconhub/vimes")
library(vimes)
library(fields)
library(dplyr)

# Bring in the data we are going to use

source("tests_sh/prep_data_for_vimes.R")

## select the complete cases for time. 
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])

# Computing pairwise distances between cases in time, space,

#Distances between dates are computed as numbers of days:
#head(vd$Time_diff, 5)
D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix

## Distances between locations are computed using the great circle distance:
## The great-circle distance or orthodromic distance is the shortest distance
## between two points on the surface of a sphere, measured along the surface 
## of the sphere (as opposed to a straight line through the sphere's interior).

#head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. 

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
D_all <- vimes_data(dates = D_dates, geo = D_geo)
par(mar = c(2,2,2,2))

plot(D_all, nclass = 60)

# Enter the values for the serial interval and transmission distance that are needed

# Just enter the mean and sd of the distributions - the inbuilt function will convert 
# these to the parameters for the distribution s

# LN_mean <- 2.82
# LN_SD <- 0.954

si_mean <- 27.8175438596491
si_sd <- 26.8565433014125

rayleigh_mean <- 0.88

# Call the simulation function and define the key values for the simulation. 
# pi = reporting rate
# n = number of 'animals' to include in simulation
# quants = value of quantiles to be evaluated

pi = 1.0
n = 1000000
quants <- c(.50, .75, .90, .95, .99)


#  Now use the function to produce estimates of the cutoffs and the density

source("R/get_quantiles_sim.R")

quants_temporal <- get_quantiles_sim(d_type = "temporal",
                                     distrib = "gamma",  n = n, rrpi = pi, 
                                     params = c(si_mean, si_sd), 
                                     q = quants)

quants_spatial <- get_quantiles_sim(d_type = "spatial", n = n, rrpi = pi, 
                                    params = rayleigh_mean,  q = quants)


# Plotting these
par(mfrow=c(2,1))
plot(quants_temporal$density_sim, xlim = c(0,365))
plot(quants_spatial$density_sim, xlim = c(0,5))

## colours used to plot these
cols <- rainbow(length(quants))


par(mfrow=c(2,1))
plot(quants_temporal$density_sim, xlim = c(0,365))
abline(v = quants_temporal$threshold_sim, col = cols, lwd = 1.5 )

plot(quants_spatial$density_sim, xlim = c(0,15))
abline(v = quants_spatial$threshold_sim, col = cols, lwd=2)


# We can overlay the graph above on top of the histogram of observed distances to
# see where these cutoffs fall with respect to our observations.

## function used to generate a plot with distribution of observed and expected distances

plot_overlay <- function(dist, dens, q_values, xlab, breaks, resol = 1,
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
  
  hist(dist, col = hist_color, border = hist_bordercolor, 
       main = "", xlab = xlab, 
       breaks = breaks)
  par(new = TRUE)
  #x <- seq(min(breaks), max(breaks), resol)
  #y <- f(x, pi = pi)
  plot(dens, type = "l", axes = FALSE, main = "", xlab = "", ylab = "", xlim = c(0, max(breaks)))
  ## add vertical lines corresponding to quantiles
  abline(v = q_values, col = q_color, lwd = 2)
}
### use the function above to create our plot: 
par(mfrow=c(2, 1), mar=c(5, 5, 0.5, 5))


## temporal
plot_overlay(dist = as.vector(D_all$dates), 
             dens = quants_temporal$density_sim,
             q_values = quants_temporal$threshold_sim, 
             xlab = "Pairwise distance in time (days)", 
             breaks = seq(0,3500, 50),
             resol = 1)
## spatial
plot_overlay(dist = as.vector(D_all$geo), 
             dens = quants_spatial$density_sim,
             q_value = quants_spatial$threshold_sim, 
             xlab = "Pairwise distance in space (km)", 
             breaks = seq(0,350,5),
             resol = 0.1)

quants_temporal$threshold_sim

quants_spatial$threshold_sim


##### Put a number of different values in a table to compare to the vimes values, 

pi_range <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0.1)
q <- c(.50, .75, .90, .95, .99)


si_table_sim_gamma <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(si_table_sim_gamma) <- q
rownames(si_table_sim_gamma) <- pi_range
#Just getting the quantiles

for(i in 1:length(pi_range)){
  si_table_sim_gamma[i,] <- get_quantiles_sim(d_type = "temporal",
                                              distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                              params = c(si_mean, si_sd), 
                                              q = quants)$threshold_sim
}

#write.csv(si_table_sim_gamma, "tests_sh/sim_si_gamma_1mill.csv")

#repeat the process to see if we get the same results whilst keeping n the same

si_table_sim_gamma_2 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(si_table_sim_gamma_2) <- q
rownames(si_table_sim_gamma_2) <- pi_range
#Just getting the quantiles

for(i in 1:length(pi_range)){
  si_table_sim_gamma_2[i,] <- get_quantiles_sim(d_type = "temporal",
                                              distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                              params = c(si_mean, si_sd), 
                                              q = quants)$threshold_sim
}

## Look at the difference between the two tables to see how large the percentage diff is.

comp_1mil <- si_table_sim_gamma/si_table_sim_gamma_2
comp_1mil <- comp_1mil*100 - 100
comp_1mil <- round(comp_1mil, 2)

# Less than 1% difference between the values, but there are differences. 

# Repeat the process with 5 million creatures

n = 5000000
si_table_sim_gamma_5mil <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(si_table_sim_gamma_5mil) <- q
rownames(si_table_sim_gamma_5mil) <- pi_range

for(i in 1:length(pi_range)){
  si_table_sim_gamma_5mil[i,] <- get_quantiles_sim(d_type = "temporal",
                                              distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                              params = c(si_mean, si_sd), 
                                              q = quants)$threshold_sim
}

#write.csv(si_table_sim_gamma_5mil, "tests_sh/sim_si_gamma_5mill.csv")

#repeat the process to see if we get the same results whilst keeping n the same

si_table_sim_gamma_5mil_2 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(si_table_sim_gamma_5mil_2) <- q
rownames(si_table_sim_gamma_5mil_2) <- pi_range

for(i in 1:length(pi_range)){
  si_table_sim_gamma_5mil_2[i,] <- get_quantiles_sim(d_type = "temporal",
                                                distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                                params = c(si_mean, si_sd), 
                                                q = quants)$threshold_sim
}

## Look at the difference between the two tables to see how large the percentage diff is.

comp_5mil <- si_table_sim_gamma_5mil/si_table_sim_gamma_5mil_2
comp_5mil <- comp_5mil*100 - 100
comp_5mil <- round(comp_5mil, 2)

max(comp_1mil)
max(comp_5mil)

#write.csv(comp_1mil, "tests_sh/compare_1mil_gamma.csv")
#write.csv(comp_5mil, "tests_sh/compare_5mil_gamma.csv")


##################### Repeat with 10 million creatures

n = 10000000

si_table_sim_gamma_10mil <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(si_table_sim_gamma_10mil) <- q
rownames(si_table_sim_gamma_10mil) <- pi_range
#Just getting the quantiles

tictoc::tic()
for(i in 1:length(pi_range)){
  si_table_sim_gamma_10mil[i,] <- get_quantiles_sim(d_type = "temporal",
                                              distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                              params = c(si_mean, si_sd), 
                                              q = quants)$threshold_sim
}
tictoc::toc()

#write.csv(si_table_sim_gamma_10mil, "tests_sh/sim_si_gamma_10mill.csv")

#repeat the process to see if we get the same results whilst keeping n the same

si_table_sim_gamma_10mil_2 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(si_table_sim_gamma_10mil_2) <- q
rownames(si_table_sim_gamma_10mil_2) <- pi_range

#Just getting the quantiles
tictoc::tic()
for(i in 1:length(pi_range)){
  si_table_sim_gamma_10mil_2[i,] <- get_quantiles_sim(d_type = "temporal",
                                                distrib = "gamma",  n = n, rrpi = pi_range[i], 
                                                params = c(si_mean, si_sd), 
                                                q = quants)$threshold_sim
}
tictoc::toc()

## Look at the difference between the two tables to see how large the percentage diff is.

comp_10mil <- si_table_sim_gamma_10mil/si_table_sim_gamma_10mil_2
comp_10mil <- comp_10mil*100 - 100
comp_10mil <- round(comp_10mil, 2)
range(comp_10mil)

#write.csv(comp_10mil, "tests_sh/compare_10mil_gamma.csv")


##########################################################################
## Looks at the distance kernel

dist_table_sim <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(dist_table_sim) <- q
rownames(dist_table_sim) <- pi_range
#Just getting the quantiles

for(i in 1:length(pi_range)){
  dist_table_sim[i,] <- get_quantiles_sim(d_type = "spatial",
                                              n = n, rrpi = pi_range[i], 
                                              params = c(rayleigh_mean), 
                                              q = quants)$threshold_sim
}

#write.csv(dist_table_sim, "tests_sh/sim_dist_1mill.csv")

#repeat the process to see if we get the same results whilst keeping n the same

dist_table_sim_2 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(dist_table_sim_2) <- q
rownames(dist_table_sim_2) <- pi_range
#Just getting the quantiles

for(i in 1:length(pi_range)){
  dist_table_sim_2[i,] <- get_quantiles_sim(d_type = "spatial",
                                                n = n, rrpi = pi_range[i], 
                                                params = c(rayleigh_mean), 
                                                q = quants)$threshold_sim
}

## Look at the difference between the two tables to see how large the percentage diff is.

comp_1mil_dist <- dist_table_sim/dist_table_sim_2
comp_1mil_dist <- comp_1mil_dist*100 - 100
comp_1mil_dist <- round(comp_1mil_dist, 2)
range(comp_1mil_dist)

# Less than 1% difference between the values, but there are differences. 

# see what happens with 5 million creatures. 
n = 5000000

dist_table_sim_5mil <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(dist_table_sim_5mil) <- q
rownames(dist_table_sim_5mil) <- pi_range

for(i in 1:length(pi_range)){
  dist_table_sim_5mil[i,] <- get_quantiles_sim(d_type = "spatial",
                                              n = n, rrpi = pi_range[i], 
                                              params = c(rayleigh_mean), 
                                              q = quants)$threshold_sim
}

#write.csv(dist_table_sim_5mil, "tests_sh/sim_dist_5mill.csv")

#repeat the process to see if we get the same results whilst keeping n the same

dist_table_sim_5mil_2 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(dist_table_sim_5mil_2) <- q
rownames(dist_table_sim_5mil_2) <- pi_range

for(i in 1:length(pi_range)){
  dist_table_sim_5mil_2[i,] <- get_quantiles_sim(d_type = "spatial",
                                                n = n, rrpi = pi_range[i], 
                                                params = c(rayleigh_mean), 
                                                q = quants)$threshold_sim
}

## Look at the difference between the two tables to see how large the percentage diff is.

comp_5mil_dist <- dist_table_sim_5mil/dist_table_sim_5mil_2
comp_5mil_dist <- comp_5mil_dist*100 - 100
comp_5mil_dist <- round(comp_5mil_dist, 2)

range(comp_1mil_dist)
range(comp_5mil_dist)

#################  And finally with 10 million critters

n=10000000

dist_table_sim_10mil <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(dist_table_sim_10mil) <- q
rownames(dist_table_sim_10mil) <- pi_range
#Just getting the quantiles

for(i in 1:length(pi_range)){
  dist_table_sim_10mil[i,] <- get_quantiles_sim(d_type = "spatial",
                                              n = n, rrpi = pi_range[i], 
                                              params = c(rayleigh_mean), 
                                              q = quants)$threshold_sim
}

#write.csv(dist_table_sim_10mil, "tests_sh/sim_dist_10mill.csv")

#repeat the process to see if we get the same results whilst keeping n the same

dist_table_sim_10mil_2 <- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
colnames(dist_table_sim_10mil_2) <- q
rownames(dist_table_sim_10mil_2) <- pi_range
#Just getting the quantiles

for(i in 1:length(pi_range)){
  dist_table_sim_10mil_2[i,] <- get_quantiles_sim(d_type = "spatial",
                                                n = n, rrpi = pi_range[i], 
                                                params = c(rayleigh_mean), 
                                                q = quants)$threshold_sim
}

## Look at the difference between the two tables to see how large the percentage diff is.

comp_10mil_dist <- dist_table_sim_10mil/dist_table_sim_10mil_2
comp_10mil_dist <- comp_10mil_dist*100 - 100
comp_10mil_dist <- round(comp_10mil_dist, 2)

range(comp_1mil_dist) 
range(comp_5mil_dist)
range(comp_10mil_dist)

#write.csv(comp_1mil_dist, "tests_sh/comp_1mil_dist.csv")
#write.csv(comp_5mil_dist, "tests_sh/comp_5mil_dist.csv")
#write.csv(comp_10mil_dist, "tests_sh/comp_10mil_dist.csv")
