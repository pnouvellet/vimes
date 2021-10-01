# Adapted for my code. Original saved in Vimes_vignette_ref

rm(list=ls())

#install.packages("devtools")
library(devtools)
#install_github("reconhub/vimes")
library(vimes)

# Bring in the data we are going to use
source("tests_sh/prep_data_for_vimes.R")

## select the complete cases for time. 
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])
plot(new)

#We can plot the epidemic curve using the `incidence` package:
library(incidence) 
## weekly - two versions of doing the same thing here
plot(incidence(vd$Time_diff,7))
plot(incidence(vd$Date_started,7))
## monthly
plot(incidence(vd$Time_diff,365/12))
## bimonthly
plot(incidence(vd$Time_diff,365/6))


#And we can plot the locations of the cases:
plot(new, xlab = "", ylab = "")

# Computing pairwise distances between cases in time, space,

#Distances between dates are computed as numbers of days:
head(vd$Time_diff, 5)
D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix

## Distances between locations are computed using the great circle distance:
## The great-circle distance or orthodromic distance is the shortest distance
## between two points on the surface of a sphere, measured along the surface 
## of the sphere (as opposed to a straight line through the sphere's interior).
library(fields)
head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. 

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
D_all <- vimes_data(dates = D_dates, geo = D_geo)
par(mar = c(2,2,2,2))

plot(D_all, nclass = 60)

# Defining cutoff distances above which cases are considered not linked by transmission 

## Distributions of expected distances between cases for rabies
## Using the distribution parameters that I found from Serengeti data set
## For the serial interval this was a log normal
 LN_mean <- 2.82
 LN_SD <- 0.954

## spatial kernel parameters
## from wikipedia: In probability theory and statistics, 
##the Rayleigh distribution is a continuous probability distribution for positive-valued 
##random variables. It is a chi distribution in two degrees of freedom.
##A Rayleigh distribution is often observed when the overall magnitude of a vector 
##is related to its directional components.

rayleigh_mean <- 0.88
## find Rayleigh parameters to match this mean
rayleigh_scale <- rayleigh_mean / sqrt(acos(-1)/2)

## We use the `fpaircase` function to compute the distributions of expected
## distances (temporal, spatial) between a case and their
## infector based on the above parameters.

## distance functions for each of the 2 types
## set up a vector of the same length as the maximum pairwise distance of the SIs.
## Will need this to use within fpaircase when defining 'p'
max(D_dates)
date_vect <- seq(0,max(D_dates), 1)


f_temporal <- fpaircase(type = "empiric", p = dlnorm(x = date_vect, meanlog = LN_mean, 
                                                     sdlog = LN_SD, log = FALSE))

f_spatial <- fpaircase(type = "spatial", sd_spatial = rayleigh_scale)

# Plotting these
par(mfrow=c(2,1))
plot(f_temporal, xlim = c(0,365))
plot(f_spatial, xlim = c(0,5))

## Cutoff distances defined as the quantiles of the distributions of expected pairwise distances

## We will be using the above distributions of expected distances between a case
## and their infector to define cutoff distances above which pairs of cases are
## considered not linked by transmission.

## First, we assume a certain level of reporting.

## reporting rate
pi <- 0.5

# Then, we define the quantile we want to use to define the cutoff
# distances. Here, we consider many different quantiles to assess sensitivity of
# our results to this choice.

## quantiles
q <- c(.50, .75, .90, .95, .99)
# q <- 0.95  # start with just one quantile

## colours used to plot these
cols <- rainbow(length(q))

# We now plot the distributions of expected distances (temporal, spatial) between a case and their closest observed ancestor,
# assuming that only pi% of cases are reported. We also show the cutoffs
# corresponding to the quantiles defined above.

## distance functions for each of the 2 types, accounting for reporting
## probability pi with quantiles q overlaid on the graphs
par(mfrow=c(2,1))
plot(f_temporal, q, xlim = c(0,365), pi = pi,
     lines_arg = list(col=cols, lwd=2))
plot(f_spatial, q, xlim = c(0,15), pi = pi,
     lines_arg = list(col=cols, lwd=2))

# We can overlay the graph above on top of the histogram of observed distances to
# see where these cutoffs fall with respect to our observations.

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
### use the function above to create our plot: 
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

# just getting the quantiles
qtl <- get_quantiles(f_temporal, q, pi = pi)
qtl
# Running vimes to identify clusters of cases linked by transmission

# We run vimes to identify clusters of cases linked by transmission, for various
# cutoff choices, and various reporting rates. We plot the results by colouring
# cases identified as belonging to the same outbreak cluster, i.e. cases
# identified as being linked by local transmission. Cases shown in grey are
# identified as singletons who are not linked by transmission to any other
# observed case.

### function used to get results for a certain cutoff and reporting rate
get_res <- function(D_all, q, pi, f_temporal, f_spatial,
                    type = c("all", "temporal","spatial")) {
  
  type <- match.arg(type)
  
  ## get the cutoffs
  cuts <- c(temporal = get_quantiles(f_temporal, q, pi = pi), 
            spatial = get_quantiles(f_spatial, q, pi = pi))
  
  if (type == "all") {
    ## use vimes
    out <- vimes(D_all, cutoff = cuts,
                 graph.opt = vimes.graph.opt(col.pal = funky))
  } else if (type == "temporal") {
    out <- vimes(vimes_data(dates = D_all$dates), cutoff = cuts["temporal"],
                 graph.opt = vimes.graph.opt(col.pal = funky))
  } else if (type == "spatial") {
    out <- vimes(vimes_data(geo = D_all$geo), cutoff = cuts["spatial"],
                 graph.opt = vimes.graph.opt(col.pal = funky))
  } 
  
  return(out)
  
}


## Try just generating 95% first

ninefive <- expand.grid(p = 0.95,
                        pi = pi)   ## expand grid is a function in vimes that 
## creates a data frame from all combinations of the supplied vectors or factors.
ninefive
quantile_pretty <- signif(ninefive$p*100, 4)  ## signif rounds to the specified number of decimal places
quantile_pretty <- paste0(quantile_pretty, "%")
res <- vector(1L, mode = "list") ## makes a vector 1 lists. 
## Then use the values in combi (which in this part is 0.95% and the reporting rate of 0.2) 
## and use the function defined above to apply the vimes function
for (i in 1:nrow(ninefive)) {
  res[[i]] <- get_res(D_all, ninefive[i, 1],
                      ninefive[i, 2], f_temporal,
                      f_spatial)
}


### visualise the output
par(mfrow = c(1, 1), mar=c(1,1,3,1))
for (i in 1:length(res)) {
  plot(res[[i]]$graph, vertex.label = "",
       main = paste("cutoff:", quantile_pretty[i]))
}

## Now with all the quantiles

### use the function above to generate results for several combinations of p and pi 
combi <- expand.grid(p = q, pi = pi)   ## expand grid is a function in vimes that 
## creates a data frame from all combinations of the supplied vectors or factors.
combi

quantile_pretty <- signif(combi$p*100, 4)  ## signif rounds to the specified number of decimal places
quantile_pretty <- paste0(quantile_pretty, "%")
res <- vector(length(q), mode = "list") ## makes a vector of 9 lists. 

## Then use the values in combi (which are all the quantiles and the reporting rate) 
## and use the function defined above to apply the vimes function
for (i in 1:nrow(combi)) {
  res[[i]] <- get_res(D_all, combi[i, 1],
                      combi[i, 2], f_temporal,
                      f_spatial)
}


### visualise the output
par(mfrow = c(2, 3), mar=c(1,1,3,1))
for (i in 1:length(res)) {
  plot(res[[i]]$graph, vertex.label = "",
       main = paste("cutoff:", quantile_pretty[i]))
}


## ninety nine cutoff
ninenine <- res[[5]]

cluster_membership_99 <- ninenine$clusters$membership
cluster_size_99 <- ninenine$clusters$size

## add the cluster number to the dataframe
SE_Tanz$cluster_number_99 <- ninenine$clusters$membership

library(dplyr)



