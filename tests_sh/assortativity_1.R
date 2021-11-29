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

#shape_vect <- round(10^(seq(0,2,length.out = n_grid)),2) # use log values as there is a smaller change for the unit 
#shape_vect <- round(10^(seq(0,1,length.out = n_grid)),2) # use log values as there is a smaller change for the unit 
#shape_vect <- c(seq(2, 5, length.out = n_grid))

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

params_s1s1_spatial <- c(rayleigh_mean)
params_s2s2_spatial <- c(rayleigh_mean)
params_s1s2_spatial <- c(rayleigh_mean)

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
#write.csv(si_res_df, "tests_sh/assort_si_95_0.6_10mil_313_236_1_10.csv")
#write.csv(dist_res_df, "tests_sh/assort_dist_95_0.6_10mil_313_236_1_10.csv")
#write.csv(si_res_df, "tests_sh/assort_si_95_0.6_10mil_313_236_2_5.csv")
#write.csv(dist_res_df, "tests_sh/assort_dist_95_0.6_10mil_313_236_2_5.csv")


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

## Extract the number of each type of transmission

trans_fun <- function(x){
  graph <- x[["graph"]]
  graph_df <- igraph::as_data_frame(graph)
  trans <- cases_deets_function(graph_df)
  tt <- trans$transmissions
  tt$trans_type  <-  NA
  tt[which(tt$trans %in% c("11", "14", "41", "44")), "trans_type"] <- "s1s1"
  tt[which(tt$trans %in% c("12", "21", "42", "24")), "trans_type"] <- "mixed"
  tt[which(tt$trans == "22"), "trans_type"] <- "s2s2"
  vt <- as.data.frame(table(tt$trans_type))
}

#tt <-  trans_fun(res_1)
#tt

vimes_res_trans <- purrr::map(vimes_res_list, trans_fun)
#vimes_res_trans[[1]]

trans_res_df <- purrr::reduce(vimes_res_trans, left_join, by = "Var1")
colnames(trans_res_df) <- c("trans_type", as.character(shape_vect))  
trans_res_df$trans_type <- as.character(trans_res_df$trans_type)
trans_res_df[4,1] <- "total"
trans_res_df[4,2:11] <- colSums(trans_res_df[1:3, 2:11])


### Now get the proportions from the simulations
colnames(si_res_df) <- paste(colnames(si_res_df), "si", sep = "_")
colnames(dist_res_df) <- paste(colnames(dist_res_df), "dist", sep = "_")

sim_props <- si_res_df %>% 
  cbind(dist_res_df) %>%
  dplyr::select(s1_prop_all_si, mix_prop_all_si, s2_prop_all_si, 
                s1_prop_all_dist, mix_prop_all_dist, s2_prop_all_dist)

sim_props[,"s1_props_mean"] <- rowMeans(sim_props[,c("s1_prop_all_si", "s1_prop_all_dist")])
sim_props[,"mixed_props_mean"] <- rowMeans(sim_props[,c("mix_prop_all_si", "mix_prop_all_dist")])
sim_props[,"s2_props_mean"] <- rowMeans(sim_props[,c("s2_prop_all_si", "s2_prop_all_dist")])

sim_props <- as.data.frame(t(sim_props))
sim_props <- sim_props %>% rownames_to_column 
sim_props <- sim_props[which(sim_props$rowname %in% c("s1_props_mean", "mixed_props_mean",
                                                      "s2_props_mean")),]

sim_props$rowname <- as.character(sim_props$rowname)
sim_props <- rename(sim_props, "trans_type" = "rowname")

trans_res_df <- rbind(trans_res_df, sim_props)

trans_res_df[8, 2:11] <- round(trans_res_df[which(trans_res_df$trans_type == "s1_props_mean"),2:11]*
  trans_res_df[which(trans_res_df$trans_type == "total"),2:11],2)
trans_res_df[8,1] <- "exp_s1s1"


trans_res_df[9, 2:11] <- round(trans_res_df[which(trans_res_df$trans_type == "mixed_props_mean"),2:11]*
  trans_res_df[which(trans_res_df$trans_type == "total"),2:11],2)
trans_res_df[9,1] <- "exp_mixed"

trans_res_df[10, 2:11] <- round(trans_res_df[which(trans_res_df$trans_type == "s2_props_mean"),2:11]*
  trans_res_df[which(trans_res_df$trans_type == "total"),2:11],2)
trans_res_df[10,1] <- "exp_s2s2"

trans_res_df[11,2:11] <- round(colSums(trans_res_df[c(8,9,10), 2:11]),0)
trans_res_df[11,1] <- "exp_total"

trans_res_df[11, 2:11] - trans_res_df[4, 2:11]

# single value
# Xi_sq = (trans_res_df[which(trans_res_df$trans_type == "s1s1"),"1"] - trans_res_df[which(trans_res_df$trans_type == "exp_s1s1"), "1"])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_s1s1"), "1"] +
#   
#   (trans_res_df[which(trans_res_df$trans_type == "mixed"),"1"] - trans_res_df[which(trans_res_df$trans_type == "exp_mixed"), "1"])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_mixed"), "1"] +
#   
#   (trans_res_df[which(trans_res_df$trans_type == "s2s2"),"1"] - trans_res_df[which(trans_res_df$trans_type == "exp_s2s2"), "1"])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_s2s2"), "1"]
  
## Adding to the table

trans_res_df[12,1] <-  "Xi_sq"

trans_res_df[12, 2:11] <-  
  (trans_res_df[which(trans_res_df$trans_type == "s1s1"),2:11] - trans_res_df[which(trans_res_df$trans_type == "exp_s1s1"), 2:11])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_s1s1"), 2:11] +
  
  (trans_res_df[which(trans_res_df$trans_type == "mixed"),2:11] - trans_res_df[which(trans_res_df$trans_type == "exp_mixed"), 2:11])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_mixed"), 2:11] +
  
  (trans_res_df[which(trans_res_df$trans_type == "s2s2"),2:11] - trans_res_df[which(trans_res_df$trans_type == "exp_s2s2"), 2:11])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_s2s2"), 2:11]

# Plot the values
plot(shape_vect, trans_res_df[12,2:11], xlab = "Value of shape 2", ylab = "Chi squared value")


#write.csv(trans_res_df, "tests_sh/assort_res_shape_1_100.csv")
#write.csv(trans_res_df, "tests_sh/assort_res_shape_1_10.csv")
#write.csv(trans_res_df, "tests_sh/assort_res_shape_2_5.csv")

par(mfrow = c(2,1))
shape <- 3.6
r <- rbeta(n = 1e4, shape1 = 1, shape2 = shape)
hist(r, breaks = seq(0,1,by=.02), main = "Species 1", xlab = "", col = "red")

p <- rbeta(n = 1e4, shape1 = shape, shape2 = 1)
hist(p, breaks = seq(0,1, by = 0.02), main = "Species 2", xlab = "", col ="blue")


#####################

# Get the proportions from each of the vimes results

props_fun <- function(x){
  graph <- x[["graph"]]
  graph_df <- igraph::as_data_frame(graph)
  props <- cases_deets_function(graph_df)
  props$props_df
}


vimes_res_props <- purrr::map(vimes_res_list, props_fun)
vimes_res_props[[1]]
vt <- vimes
## Fill a dataframe with these proportions

prop_res_df <- purrr::reduce(vimes_res_props, left_join, by = "trans_type")
colnames(prop_res_df) <- c("trans_type", as.character(shape_vect))  

prop_res_summary <- as.data.frame(matrix(ncol = ncol(prop_res_df), nrow = 3))
colnames(prop_res_summary) <- colnames(prop_res_df)  
prop_res_summary[,"trans_type"] <- c("s1s1", "mixed", "s2s2")

s1s1 <- c("dog-dog", "dog-cat", "cat-cat", "cat-dog")
mixed <- c("dog-wild", "cat-wild", "wild-dog", "wild-cat")

#prop_res_df %>% filter(trans_type %in% s1s1) %>%
#  dplyr::select(!trans_type) %>%
#  colSums(.)

prop_res_summary[which(prop_res_summary == "s1s1"),2:11] <- prop_res_df %>% 
  filter(trans_type %in% s1s1) %>%
  dplyr::select(!trans_type) %>%
  colSums(.)

prop_res_summary[which(prop_res_summary == "mixed"),2:11] <- prop_res_df %>% 
  dplyr::filter(trans_type %in% mixed) %>%
  dplyr::select(!trans_type) %>%
  colSums(.)

prop_res_summary[which(prop_res_summary == "s2s2"),2:11] <- prop_res_df %>% 
  filter(trans_type == "wild-wild") %>%
  dplyr::select(!trans_type) %>%
  colSums(.)

### Now get the proportions from the simulations
colnames(si_res_df) <- paste(colnames(si_res_df), "si", sep = "_")
colnames(dist_res_df) <- paste(colnames(dist_res_df), "dist", sep = "_")

sim_props <- si_res_df %>% 
  cbind(dist_res_df) %>%
  dplyr::select(s1_prop_all_si, mix_prop_all_si, s2_prop_all_si, 
                s1_prop_all_dist, mix_prop_all_dist, s2_prop_all_dist)

sim_props[,"s1_props_mean"] <- rowMeans(sim_props[,c("s1_prop_all_si", "s1_prop_all_dist")])
sim_props[,"mixed_props_mean"] <- rowMeans(sim_props[,c("mix_prop_all_si", "mix_prop_all_dist")])
sim_props[,"s2_props_mean"] <- rowMeans(sim_props[,c("s2_prop_all_si", "s2_prop_all_dist")])

sim_props <- as.data.frame(t(sim_props))
sim_props <- sim_props %>% rownames_to_column 
sim_props <- sim_props[which(sim_props$rowname %in% c("s1_props_mean", "mixed_props_mean",
                                              "s2_props_mean")),]

## Look at getting a chi square value for the scenarios
Xi_sq = (prop_res_summary[1,"1"] - sim_props[1,"1"])^2/ sim_props[1,"1"] +
  
  (prop_res_summary[2,"1"] - sim_props[2,"1"])^2/sim_props[2,"1"] +
  
  (prop_res_summary[3,"1"] - sim_props[3,"1"])^2/ sim_props[3,"1"]


### However what we actually need is the number of transmissions rather than the proportions.

a1 <- cases_deets_function(res_1)

