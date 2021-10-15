## Trying to use our function within the Vimes workflow. 

# Running through the vimes vignette. Original saved in Vimes_vignette_ref

rm(list=ls())

library(devtools)
#install_github("reconhub/vimes")
library(vimes)

# Bring in the data we are going to use
source("tests_sh/prep_data_for_vimes.R")

## select the complete cases for time. 
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
library(fields)
head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. 

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
D_all <- vimes_data(dates = D_dates, geo = D_geo)  # no changes needed to this function
par(mar = c(2,2,2,2))

plot(D_all, nclass = 60)

# Defining cutoff distances above which cases are considered not linked by transmission 

## Distributions of expected distances between cases for rabies
## Using the distribution parameters that I found from Serengeti data set
## For the serial interval this was a log normal
# LN_mean <- 2.82
# LN_SD <- 0.954

# Now using the mean and sd of the data instead of the parameters and the function estimates parameters from those.
si_mean <- 27.8
si_sd <- 36.9

## spatial kernel parameters
rayleigh_mean <- 0.88
## find Rayleigh parameters to match this mean
# rayleigh_scale <- rayleigh_mean / sqrt(acos(-1)/2)

# Here we need to use our functions to work out the probability of the observed distances between cases
# We use our function instead of fpaircase

# max(D_dates)
# date_vect <- seq(0,max(D_dates), 1)

## First, we assume a certain level of reporting.

## reporting rate
pi <- 0.7

# Then, we define the quantile we want to use to define the cutoff
# distances. Here, we consider many different quantiles to assess sensitivity of
# our results to this choice.

## quantiles
# quants <- c(.50, .75, .90, .95, .99)
 quants <- 0.95  # start with just one quantile

## colours used to plot these
cols <- rainbow(length(quants))

source("R/get_quantiles_sim.R")

si_sim_cutoffs <- get_quantiles_sim(d_type = "temporal", distrib = "lognormal", n = 1000000, rrpi = pi,
                                    params =c(si_mean, si_sd), q = quants)
si_sim_cutoffs

bd_sim_cutoffs <- get_quantiles_sim(d_type = "spatial", n = 1000000, rrpi = pi, params = rayleigh_mean,
                                    q = quants)
bd_sim_cutoffs
# Running vimes to identify clusters of cases linked by transmission

# First going to see if can get the output using vimes without putting in a function

## get the cutoffs
  # cuts <- c(temporal = get_quantiles_sim(d_type = "temporal", n = 10000, rrpi = 1, si_log_mean = LN_mean, si_log_sd = LN_SD, q = quants), 
  #           spatial = get_quantiles_sim(d_type = "spatial", n = 10000,  rrpi = 1, dist_ray = rayleigh_scale, q = quants))
  # 
  # out <- vimes(vimes_data(dates = D_all$dates), cutoff = cuts[1],
  #                graph.opt = vimes.graph.opt(col.pal = funky))
  # 
  
  get_res <- function(D_all, type = c("all", "temporal","spatial"), n, rrpi = pi, si_log_mean, si_log_sd, dist_ray, q ) {
    
    type <- match.arg(type)
    
    ## get the cutoffs
    # cuts <- c(temporal = get_quantiles_sim(d_type = "temporal", n=  10000, rrpi = 1, si_log_mean = LN_mean, si_log_sd = LN_SD, quants), 
    #           spatial = get_quantiles_sim(d_type = "spatial", n = 10000, rrpi = 1, dist_ray = rayleigh_scale, q = quants))
    
    cuts <- c(temporal = get_quantiles_sim(d_type = "temporal", n = n, rrpi = pi, si_log_mean = si_log_mean, si_log_sd = si_log_sd, q = q), 
              spatial = get_quantiles_sim(d_type = "spatial", n = n , rrpi = pi, dist_ray = dist_ray, q = q))
    
    
    
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
  
  
# Try just generating 95% first

ninefive <- expand.grid(p = 0.95,
                        pi = pi)   ## expand grid is a function in vimes that 
## creates a data frame from all combinations of the supplied vectors or factors.
ninefive

quantile_pretty <- signif(ninefive$p*100, 4)  ## signif rounds to the specified number of decimal places
quantile_pretty <- paste0(quantile_pretty, "%")

res_95 <- vector(1L, mode = "list") ## makes a vector 1 lists. 
## Then use the values in combi (which in this part is 0.95% and the reporting rate of 0.2) 
## and use the function defined above to apply the vimes function
for (i in 1:nrow(ninefive)) {
  res_95[[i]] <- get_res(D_all, n = 10000000, rrpi = pi, si_log_mean = LN_mean, si_log_sd = LN_SD, 
  dist_ray = rayleigh_scale, q = quants )
}


### visualise the output
par(mfrow = c(1, 1), mar=c(1,1,3,1))
for (i in 1:length(res_95)) {
  plot(res_95[[i]]$graph, vertex.label = "",
       main = paste("cutoff:", quantile_pretty[i]))
}

SE_Tanz$cluster_number_95 <- res_95[[1]]$clusters$membership


library(dplyr)
clusts <- SE_Tanz %>%
  group_by(cluster_number_95) %>% filter(row_number() != 1)

table(clusts$ID)

## This matches the Vimes output when we use a high number of iterations


############## Now see if we can do for multiple combos

quants <- c(.50, .75, .90, .95, .99)
### use the function above to generate results for several combinations of p and pi
combi <- expand.grid(p = quants, pi = pi)   ## expand grid is a function in vimes that
## creates a data frame from all combinations of the supplied vectors or factors.
combi


 quantile_pretty <- signif(combi$p*100, 4)  ## signif rounds to the specified number of decimal places
 quantile_pretty <- paste0(quantile_pretty, "%")
 res <- vector(length(q), mode = "list") ## makes a vector of 9 lists. 
 
## Then use the values in combi (which are all the quantiles and the reporting rate) 
## and use the function defined above to apply the vimes function
 for (i in 1:nrow(combi)) {
   res[[i]] <- get_res(D_all, n = 5000000, rrpi = combi[i,2], si_log_mean = LN_mean, si_log_sd = LN_SD, 
                       dist_ray = rayleigh_scale, q = combi[i,1])
 }


### visualise the output
par(mfrow = c(3, 3), mar=c(1,1,3,1))
 for (i in 1:length(res)) {
   plot(res[[i]]$graph, vertex.label = "",
        main = paste("cutoff:", quantile_pretty[i]))
 }


## The fifth element of the list in this case is the 99% cutoff 

ninenine <- res[[5]]

cluster_membership_99 <- ninenine$clusters$membership
cluster_size_99 <- ninenine$clusters$size

## add the cluster number to the dataframe
SE_Tanz$cluster_number_99 <- res[[5]]$clusters$membership

clusts_99 <- SE_Tanz %>%
  group_by(cluster_number_99) %>% filter(row_number() != 1)

table(clusts_99$ID)


