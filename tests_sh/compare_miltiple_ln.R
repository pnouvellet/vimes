# Here we are finding the difference between vimesand multiple 
# runs of the simulation method 

rm(list = ls())

library(tidyverse)
library(vimes)
library(fields)
library(dplyr)


# Read in the vimes results.
vimes_ln <- read.csv("tests_sh/vimes_si_ln.csv", row.names = 1, check.names = F)

# Bring in the data we are going to use for the simulations

source("tests_sh/prep_data_for_vimes.R")

## select the complete cases for time. 
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])

# Computing pairwise distances between cases in time, space,

#Distances between dates are computed as numbers of days:
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

n = 1000000

#  Now use the function to produce estimates of the cutoffs and the density

 source("R/get_quantiles_sim.R")

pi_range <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0.1)
q <- c(.50, .75, .90, .95, .99)

list_size <- 100

# First look at 10,000
n <- 10000
tabs_list_1m <- list()

tictoc::tic()
for (j in 1:list_size) {
  si_tab<- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
  for(i in 1:length(pi_range)){
    si_tab[i,] <- get_quantiles_sim(d_type = "temporal",
                                    distrib = "lognormal",  n = n, rrpi = pi_range[i], 
                                    params = c(si_mean, si_sd), 
                                    q = q)$threshold_sim
  }
  tabs_list_1m[[j]] <- si_tab
}
tictoc::toc()


# comp_function <- function(x){
#   z <- round((((vimes_ln/x)*100)-100),2)
# }


comp_function <- function(x){
  z <- ((vimes_ln - x)/vimes_ln)*100
  z
}


comp_10k <- purrr::map(tabs_list_1m, comp_function)  
comp_10k[[1]]  
comp_10k[[2]]


# extract the summary stats from these tables
# some useful tips on how to do this are at: 
# https://stackoverflow.com/questions/7651539/mean-of-elements-in-a-list-of-data-frames

medians_10k_ln = plyr::aaply(plyr::laply(comp_10k, as.matrix), c(2, 3), median)
IQR25_10k_ln <- plyr::aaply(plyr::laply(comp_10k, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.25))
IQR75_10k_ln <- plyr::aaply(plyr::laply(comp_10k, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.75))
range_lower <- plyr::aaply(plyr::laply(comp_10k, as.matrix), c(2, 3), function(x)quantile(x, probs = 0))
range_upper <- plyr::aaply(plyr::laply(comp_10k, as.matrix), c(2, 3), function(x)quantile(x, probs = 1))

#write.csv(medians_10k_ln, "tests_sh/comp_ln_10k_medians.csv")
#write.csv(IQR25_10k_ln, "tests_sh/comp_ln_10k_IQR25.csv")
#write.csv(IQR75_10k_ln, "tests_sh/comp_ln_10k_IQR75.csv")

############################################################

tabs_list_1m <- list()

n = 100000

tictoc::tic()
for (j in 1:list_size) {
  # to produce one table use:
  si_tab<- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
  
  for(i in 1:length(pi_range)){
    si_tab[i,] <- get_quantiles_sim(d_type = "temporal",
                                    distrib = "lognormal",  n = n, rrpi = pi_range[i], 
                                    params = c(si_mean, si_sd), 
                                    q = q)$threshold_sim
  }
  tabs_list_1m[[j]] <- si_tab
}
tictoc::toc()

## look at a couple of lists to check different
#tabs_list_1m[[1]]
#tabs_list_1m[[2]]


### Compare for 10000
comp_100k <- purrr::map(tabs_list_1m, comp_function)  
comp_100k[[1]]  
comp_100k[[2]]

# extract the summary stats from these tables
# some useful tips on how to do this are at: 
# https://stackoverflow.com/questions/7651539/mean-of-elements-in-a-list-of-data-frames

medians_100k_ln = plyr::aaply(plyr::laply(comp_100k, as.matrix), c(2, 3), median)
IQR25_100k_ln <- plyr::aaply(plyr::laply(comp_100k, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.25))
IQR75_100k_ln <- plyr::aaply(plyr::laply(comp_100k, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.75))
range_lower <- plyr::aaply(plyr::laply(comp_100k, as.matrix), c(2, 3), function(x)quantile(x, probs = 0))
range_upper <- plyr::aaply(plyr::laply(comp_100k, as.matrix), c(2, 3), function(x)quantile(x, probs = 1))

#par(mfrow = c(nrow = 5, ncol = 6))
#Box_100k_ln <- plyr::aaply(plyr::laply(comp_100k, as.matrix), c(2, 3), boxplot) # would ideally need to set these to be all the same axis if to be of use 

#write.csv(medians_100k_ln, "tests_sh/comp_ln_100k_medians.csv")
#write.csv(IQR25_100k_ln, "tests_sh/comp_ln_100k_IQR25.csv")
#write.csv(IQR75_100k_ln, "tests_sh/comp_ln_100k_IQR75.csv")


####################### Compare for 1 million

tabs_list_1m <- list()


tictoc::tic()
for (j in 1:list_size) {
  # to produce one table use:
si_tab<- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
#colnames(si_tab[j]) <- q
#rownames(si_tab[j]) <- pi_range
#Just getting the quantiles

for(i in 1:length(pi_range)){
  si_tab[i,] <- get_quantiles_sim(d_type = "temporal",
                                           distrib = "lognormal",  n = n, rrpi = pi_range[i], 
                                           params = c(si_mean, si_sd), 
                                           q = q)$threshold_sim
}
tabs_list_1m[[j]] <- si_tab
}
tictoc::toc()


# x = tabs_list_1m[[1]]
# tabs_list_1m[[2]]
# tabs_list_1m[[3]]

comp_function <- function(x){
  z <- round((((vimes_ln/x)*100)-100),2)
}

comp_1mil <- purrr::map(tabs_list_1m, comp_function)  
vimes_ln
comp_1mil[[1]]  
comp_1mil[[2]]

# extract the summary stats from these tables
# some useful tips on how to do this are at: 
# https://stackoverflow.com/questions/7651539/mean-of-elements-in-a-list-of-data-frames

medians_1mil_ln = plyr::aaply(plyr::laply(comp_1mil, as.matrix), c(2, 3), median)
IQR25_1mil_ln <- plyr::aaply(plyr::laply(comp_1mil, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.25))
IQR75_1mil_ln <- plyr::aaply(plyr::laply(comp_1mil, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.75))
range_lower <- plyr::aaply(plyr::laply(comp_1mil, as.matrix), c(2, 3), function(x)quantile(x, probs = 0))
range_upper <- plyr::aaply(plyr::laply(comp_1mil, as.matrix), c(2, 3), function(x)quantile(x, probs = 1))

par(mfrow = c(nrow = 5, ncol = 6))
Box_1mil_ln <- plyr::aaply(plyr::laply(comp_1mil, as.matrix), c(2, 3), boxplot) # would ideally need to set these to be all the same axis if to be of use 

 #write.csv(medians_1mil_ln, "tests_sh/comp_ln_1mil_medians.csv")
 #write.csv(IQR25_1mil_ln, "tests_sh/comp_ln_1mil_IQR25.csv")
 #write.csv(IQR75_1mil_ln, "tests_sh/comp_ln_1mil_IQR75.csv")


###### Repeat for 5 million runs

n <- 5000000
tabs_list_1m <- list()

tictoc::tic()
for (j in 1:list_size) {
  si_tab<- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
  for(i in 1:length(pi_range)){
    si_tab[i,] <- get_quantiles_sim(d_type = "temporal",
                                    distrib = "lognormal",  n = n, rrpi = pi_range[i], 
                                    params = c(si_mean, si_sd), 
                                    q = q)$threshold_sim
  }
  tabs_list_1m[[j]] <- si_tab
}
tictoc::toc()

comp_5mil <- purrr::map(tabs_list_1m, comp_function)  

# extract the summary stats from these tables
medians_5mil_ln = plyr::aaply(plyr::laply(comp_5mil, as.matrix), c(2, 3), median)
IQR25_5mil_ln <- plyr::aaply(plyr::laply(comp_5mil, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.25))
IQR75_5mil_ln <- plyr::aaply(plyr::laply(comp_5mil, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.75))

Box_5mil_ln <- plyr::aaply(plyr::laply(comp_1mil, as.matrix), c(2, 3), boxplot) # would ideally need to set these to be all the same axis if to be of use 

#write.csv(medians_5mil_ln, "tests_sh/comp_ln_5mil_medians.csv")
#write.csv(IQR25_5mil_ln, "tests_sh/comp_ln_5mil_IQR25.csv")
#write.csv(IQR75_5mil_ln, "tests_sh/comp_ln_5mil_IQR75.csv")

############### And repeat for 10 million 

n <- 10000000
tabs_list_1m <- list()

tictoc::tic()
for (j in 1:list_size) {
  si_tab<- as.data.frame(matrix(nrow = length(pi_range), ncol = length(q)))
  for(i in 1:length(pi_range)){
    si_tab[i,] <- get_quantiles_sim(d_type = "temporal",
                                    distrib = "lognormal",  n = n, rrpi = pi_range[i], 
                                    params = c(si_mean, si_sd), 
                                    q = q)$threshold_sim
  }
  tabs_list_1m[[j]] <- si_tab
}
tictoc::toc()

comp_10mil <- purrr::map(tabs_list_1m, comp_function)  

# extract the summary stats from these tables
medians_10mil_ln = plyr::aaply(plyr::laply(comp_10mil, as.matrix), c(2, 3), median)
IQR25_10mil_ln <- plyr::aaply(plyr::laply(comp_10mil, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.25))
IQR75_10mil_ln <- plyr::aaply(plyr::laply(comp_10mil, as.matrix), c(2, 3), function(x)quantile(x, probs = 0.75))

#write.csv(medians_10mil_ln, "tests_sh/comp_ln_10mil_medians.csv")
#write.csv(IQR25_10mil_ln, "tests_sh/comp_ln_10mil_IQR25.csv")
#write.csv(IQR75_10mil_ln, "tests_sh/comp_ln_10mil_IQR75.csv")
