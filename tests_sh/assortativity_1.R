## 25/11/2021
## Looking at assortativity
## We are going to use all the cases in our data for the numbers. 
## Domestic will be species 1 - this is dogs and cats. 
## Wildlife will be all wildlife species and will be species 2.

rm(list=ls())
library(devtools)
library(vimes)
library(fields)
library(tidyverse)

# Bring in the data we are going to use
source("tests_sh/prep_data_for_vimes.R")


## number of observed cases for each species
s1_obs <- 313 # no of cases observed for dogs/species 1
s2_obs <- 236 # no of cases observed for jackals/species 2

## Specify the reporting rates of the different species. 
s1_rr <- 0.60 # rr for dogs/species 1
s2_rr <- 0.60 # rr for jackals/species 2

n = 10000000 # set number of simulations - using 10 million initially #as algorithm very quick

q <- 0.95 # set the level of the quantile we want to use later

# generate the parameters for use within the simulation
si_mean <- 27.8175438596491
si_sd <- 26.8565433014125
rayleigh_mean <- 0.88

## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(si_mean, si_sd)
params_s1s2 <- c(si_mean, si_sd)
params_s2s2 <- c(si_mean, si_sd)

#shape_1 <- 1 # if keep the shape_! = 1 within the function we won't need to specify externally
shape_2 <- 1

source("tests_sh/get_quantiles_multi_assort.R")

# We want to run a number of different values for the shape_2 parameter.
# number of values that we want to check
n_grid <- 10

shape_vect <- round(10^(seq(0,2,length.out = n_grid)),2) # use log values as there is a smaller change for the unit 
# change at higher values.
shape_vect

si_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(si_res_df) <- shape_vect
colnames(si_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

#shape_vect <- shape_vect[1:3]

for(i in 1:length(shape_vect)){

out_gamma <- get_quantiles_multi_assort(d_type = "temporal", distrib = "gamma",
                                        s1_obs = s1_obs, s2_obs = s2_obs, 
                                        s1_rr = s1_rr, s2_rr = s2_rr, 
                                        params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                        params_s1s2 = params_s1s2,
                                        n = n, q = q, shape_2 = shape_vect[i])

si_res_df[i,"all_cut"] <- out_gamma$threshold_sim[[1]]
si_res_df[i, "s1_cut"] <- out_gamma$threshold_s1s1[[1]]
si_res_df[i, "mix_cut"] <- out_gamma$threshold_s1s2[[1]]
si_res_df[i, "s2_cut"] <- out_gamma$threshold_s2s2[[1]]
si_res_df[i, "s1_prop_all"] <- out_gamma$prop_s1s1
si_res_df[i, "mix_prop_all"] <- out_gamma$prop_s1s2
si_res_df[i, "s2_prop_all"] <- out_gamma$prop_s2s2
si_res_df[i, "s1_prop_cut"] <- out_gamma$prop_s1s1_below_quant
si_res_df[i, "mix_prop_cut"] <- out_gamma$prop_s1s2_below_quant
si_res_df[i, "s2_prop_cut"] <- out_gamma$prop_s2s2_below_quant

}


## Now repeat for the spatial kernel 

dist_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(dist_res_df) <- shape_vect
colnames(dist_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

for(i in 1:length(shape_vect)){
  
  out_dist <- get_quantiles_multi_assort(d_type = "spatial",
                                          s1_obs = s1_obs, s2_obs = s2_obs, 
                                          s1_rr = s1_rr, s2_rr = s2_rr, 
                                          params_s1s1 = params_s1s1_spatial,
                                          params_s2s2 = params_s2s2_spatial,
                                          params_s1s2 = params_s1s2_spatial,
                                          n = n, q = q, shape_2 = shape_vect[i])
  
  dist_res_df[i,"all_cut"] <- out_dist$threshold_sim[[1]]
  dist_res_df[i, "s1_cut"] <- out_dist$threshold_s1s1[[1]]
  dist_res_df[i, "mix_cut"] <- out_dist$threshold_s1s2[[1]]
  dist_res_df[i, "s2_cut"] <- out_dist$threshold_s2s2[[1]]
  dist_res_df[i, "s1_prop_all"] <- out_dist$prop_s1s1
  dist_res_df[i, "mix_prop_all"] <- out_dist$prop_s1s2
  dist_res_df[i, "s2_prop_all"] <- out_dist$prop_s2s2
  dist_res_df[i, "s1_prop_cut"] <- out_dist$prop_s1s1_below_quant
  dist_res_df[i, "mix_prop_cut"] <- out_dist$prop_s1s2_below_quant
  dist_res_df[i, "s2_prop_cut"] <- out_dist$prop_s2s2_below_quant
  
}

# save these results
#write.csv(si_res_df, "tests_sh/assort_si_95_0.6_10mil_313_236.csv")
#write.csv(dist_res_df, "tests_sh/assort_dist_95_0.6_10mil_313_236.csv")

#si_res_df <- read.csv("tests_sh/assort_si_95_0.6_10mil_313_236.csv")
#dist_res_df <- read.csv("tests_sh/assort_dist_95_0.6_10mil_313_236.csv")

# Now need to run vimes for each of the cut-off values 
# To do this we need to cuts to be vectors within a list within a list. 

si_cut_df <- si_res_df[,c("s1_cut", "mix_cut", "s2_cut")]
dist_cut_df <- dist_res_df[,c("s1_cut", "mix_cut", "s2_cut")]

si_cuts_list <- as.list(as.data.frame(t(si_cut_df)))
dist_cuts_list <- as.list(as.data.frame(t(dist_cut_df)))


cuts_list <- vector("list", length = n_grid)
for (i in 1:length(cuts_list)){
  cuts_list[[i]] <- c(si_cuts_list[i], dist_cuts_list[i])
}


### finish prepping the data for use within vimes
### select the complete cases for time. 
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])

D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix of pairwise distances
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. rdist computes the distances between points

## create the vimes_data object.
D_all <- vimes_data(dates = D_dates, geo = D_geo)

# These data are then fed into vimes::vimes

# Also need the species vector 
species_vect <- SE_Tanz$Species
# we have dogs, cats and wildlife. Change it so jsut domestic and wildlife. 
species_vect <- forcats::fct_recode(species_vect, "s1" = "Dog", "s1" = "Cat",
                                    "s2" = "Wildlife" )
species_vect <- droplevels(species_vect)
levels(species_vect)
## we need a species vector from the data.
species_vect <- SE_Tanz$Species
# we have dogs, cats and wildlife. Change it so just domestic and wildlife. 
species_vect <- forcats::fct_recode(species_vect, "s1" = "Dog", "s1" = "Cat",
                                    "s2" = "Wildlife" )
species_vect <- droplevels(species_vect)
levels(species_vect)


# We now want to run vimes to get all the clusters with the different cut_offs

source("tests_sh/vimes_prune_multi_function.R")
source("tests_sh/vimes_multi_function.R")


res_1 <- vimes_multi(D_all, cutoff = cuts_list[[1]], species_vect = species_vect,
                                  graph.opt = vimes.graph.opt(col.pal = funky))


vimes_res_list <- purrr::map(cuts_list, vimes_multi, x = D_all, method = c("basic"),log_dens = NULL, 
           species_vect = species_vect, graph.opt = vimes.graph.opt(col.pal = funky))


source("tests_sh/transmission_functions.R")
source("tests_sh/trans_table_fun.R")

res_graph <- res_1$graph
res_gdf <- igraph::as_data_frame(res_graph)

res_cases_details <- cases_deets_function(res_gdf)
res_cases_details$props_df
res_trans <- multi_res_cases_details$transmissions
