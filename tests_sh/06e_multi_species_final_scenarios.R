## 08/12/2021
## Looking at assortativity
## We are going to use all the cases in our data for the numbers. 
## Domestic will be species 1 - this is dogs and cats. 
## Wildlife will be all wildlife species and will be species 2.
## We are going to use a larger distance for S2S2 transmissions to represent the
## larger home ranges of jackals cf dogs

## First scenario to look at is reporting probability of 0.75 for group 1 and 0.50 for group 2
## with the parameters the same for both. 

## Will use the lognormal for the serial interval as this was the one best suited to our data.

rm(list=ls())
library(devtools)
library(vimes)
library(fields)
library(tidyverse)

# Bring in the data we are going to use
source("tests_sh/prep_data_for_vimes.R")

set.seed(1234)

## number of observed cases for each species
s1_obs <- 313 # no of cases observed for dogs/species 1
s2_obs <- 236 # no of cases observed for jackals/species 2

## Specify the reporting rates of the different species. 
s1_rr <- 0.75 # rr for dogs/species 1
s2_rr <- 0.50 # rr for jackals/species 2

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

shape_2 <- 1

source("tests_sh/get_quantiles_multi_assort.R")

# We want to run a number of different values for the shape_2 parameter.
# number of values that we want to check
n_grid <- 91
shape_vect <- seq(1,10,length.out = n_grid) # use log values as there is a smaller change for the unit 

# change at higher values.
shape_vect

si_res_df <- as.data.frame(matrix(ncol = 10, nrow = length(shape_vect)))
row.names(si_res_df) <- shape_vect
colnames(si_res_df) <- c("all_cut", "s1_cut", "mix_cut", "s2_cut",
                         "s1_prop_all", "mix_prop_all", "s2_prop_all",
                         "s1_prop_cut", "mix_prop_cut", "s2_prop_cut")

tictoc::tic()
for(i in 1:length(shape_vect)){
  
  out_ln <- get_quantiles_multi_assort(d_type = "temporal", distrib = "lognormal",
                                       s1_obs = s1_obs, s2_obs = s2_obs, 
                                       s1_rr = s1_rr, s2_rr = s2_rr, 
                                       params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                       params_s1s2 = params_s1s2,
                                       n = n, q = q, shape_2 = shape_vect[i])
  
  si_res_df[i,"all_cut"] <- out_ln$threshold_sim[[1]]
  si_res_df[i, "s1_cut"] <- out_ln$threshold_s1s1[[1]]
  si_res_df[i, "mix_cut"] <- out_ln$threshold_s1s2[[1]]
  si_res_df[i, "s2_cut"] <- out_ln$threshold_s2s2[[1]]
  si_res_df[i, "s1_prop_all"] <- out_ln$prop_s1s1
  si_res_df[i, "mix_prop_all"] <- out_ln$prop_s1s2
  si_res_df[i, "s2_prop_all"] <- out_ln$prop_s2s2
  si_res_df[i, "s1_prop_cut"] <- out_ln$prop_s1s1_below_quant
  si_res_df[i, "mix_prop_cut"] <- out_ln$prop_s1s2_below_quant
  si_res_df[i, "s2_prop_cut"] <- out_ln$prop_s2s2_below_quant
  
}
tictoc::toc()

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
write.csv(si_res_df, "tests_sh/final_scenarios/si_95_0.75_0.5_1_100.csv")
write.csv(dist_res_df, "tests_sh/final_scenarios/dist_95_0.75_0.5_1_100.csv")


#si_res_df <- read.csv("tests_sh/final_scenarios/si_95_0.5_1_10.csv")
#dist_res_df <- read.csv("tests_sh/final_scenarios/assort_dist_95_0.5_1_10.csv")

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


# We now want to run vimes to get all the clusters with the different cut_offs

source("tests_sh/vimes_prune_multi_function.R")
source("tests_sh/vimes_multi_function.R")

vimes_res_list <- purrr::map(cuts_list, vimes_multi, x = D_all, method = c("basic"),log_dens = NULL, 
                             species_vect = species_vect, graph.opt = vimes.graph.opt(col.pal = funky))


source("tests_sh/transmission_functions.R")
source("tests_sh/trans_table_fun.R")

## Extract the number of each type of transmission pairs

trans_fun <- function(x){
  graph <- x[["graph"]]
  graph_df <- igraph::as_data_frame(graph)
  trans <- cases_deets_function(graph_df)
  tt <- trans$transmissions
  tt$trans_type  <-  NA
  tt[which(tt$trans %in% c("11")), "trans_type"] <- "s1s1"
  tt[which(tt$trans %in% c("12", "21")), "trans_type"] <- "mixed"
  tt[which(tt$trans == "22"), "trans_type"] <- "s2s2"
  vt <- as.data.frame(table(tt$trans_type))
}


vimes_res_trans <- purrr::map(vimes_res_list, trans_fun)

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

## Adding chi-sqaured to the table

trans_res_df[12,1] <-  "Xi_sq"

trans_res_df[12, 2:11] <-  
  (trans_res_df[which(trans_res_df$trans_type == "s1s1"),2:11] - trans_res_df[which(trans_res_df$trans_type == "exp_s1s1"), 2:11])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_s1s1"), 2:11] +
  
  (trans_res_df[which(trans_res_df$trans_type == "mixed"),2:11] - trans_res_df[which(trans_res_df$trans_type == "exp_mixed"), 2:11])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_mixed"), 2:11] +
  
  (trans_res_df[which(trans_res_df$trans_type == "s2s2"),2:11] - trans_res_df[which(trans_res_df$trans_type == "exp_s2s2"), 2:11])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_s2s2"), 2:11]

# Plot the values
plot(shape_vect, trans_res_df[12,2:11], xlab = "Value of shape 2", ylab = "Chi squared value")


#########################################################################
####  Now narrow down the shape paramter. 
#### Repeat the above process with a narrower range




par(mfrow = c(2,1))
shape <- 3.6
r <- rbeta(n = 1e4, shape1 = 1, shape2 = shape)
hist(r, breaks = seq(0,1,by=.02), main = "Species 1", xlab = "", col = "red")

p <- rbeta(n = 1e4, shape1 = shape, shape2 = 1)
hist(p, breaks = seq(0,1, by = 0.02), main = "Species 2", xlab = "", col ="blue")

###########################################################
## Will now extract some results from the vimes results using the assortativity value at 1 and at 3.6


# res_1 is using the shape_2 = 1

out_si_3.6 <- get_quantiles_multi_assort(d_type = "temporal", distrib = "gamma",
                                         s1_obs = s1_obs, s2_obs = s2_obs, 
                                         s1_rr = s1_rr, s2_rr = s2_rr, 
                                         params_s1s1 = params_s1s1, params_s2s2 = params_s2s2,
                                         params_s1s2 = params_s1s2,
                                         n = n, q = q, shape_2 = 3.6)

out_dist_3.6 <- get_quantiles_multi_assort(d_type = "spatial",
                                           s1_obs = s1_obs, s2_obs = s2_obs, 
                                           s1_rr = s1_rr, s2_rr = s2_rr, 
                                           params_s1s1 = params_s1s1_spatial,
                                           params_s2s2 = params_s2s2_spatial,
                                           params_s1s2 = params_s1s2_spatial,
                                           n = n, q = q, shape_2 = 3.6)

cuts_si_3.6 <- c(out_si_3.6$threshold_s1s1, out_si_3.6$threshold_s1s2, out_si_3.6$threshold_s2s2)
cuts_dist_3.6 <- c(out_dist_3.6$threshold_s1s1, out_dist_3.6$threshold_s1s2, out_dist_3.6$threshold_s2s2)

cuts_list_3.6 <- vector("list", length = 2)
cuts_list_3.6[[1]] <- cuts_si_3.6
cuts_list_3.6[[2]] <- cuts_dist_3.6


res_3.6 <- vimes_multi(D_all, cutoff = cuts_list_3.6, species_vect = species_vect,
                       graph.opt = vimes.graph.opt(col.pal = funky))




res_1$clusters$size  #tells us the the size of the assigned clusters
range(res_1$clusters$size) #the range of cluster sizes
hist(res_1$clusters$size, col = "pink", xlab = "Size of cluster", breaks = seq(-1,9,1),
     main = "Histogram of cluster sizes")$counts

res_1$clusters$K # number of clusters including singletons
res_1$cutoff  # the cutoff values that were used. 

## Would be good to be able to plot just the non - singletons and to colour them by species
## presently only worked out how to colour - can't seem to drop the singletons
# 
# graph_1 <- multi_res$graph  # assign the graph to an object
# class(graph_1) # check it's an igraph
# 

# # And plot 
# plot(graph_1, vertex.label = "",
#      vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_1, "species")))])


###########################################################################
# The cluster membership can be joined with the SE_Tanz data to create a new data set. 

res_1_df <- as.data.frame(res_1$clusters$membership) # This is the cluster that they are assigned to. 
colnames(res_1_df) <- "cluster_memb"

source("tests_sh/trans_table_fun.R")
cs_1 <- trans_table_fun(res_1_df)

csl_1 <- cs_1 %>%
  group_by(total, trans_type)%>%
  count()

own_cols <- c("red", "purple", "blue")
own_labels <- c("Domestic only", "Mixed", "Wildlife only")

gg_res_1 <- ggplot(csl_1, aes(x = total, y = n, fill = trans_type))  +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = own_cols, name = "Transmission type",
                    #guide = guide_legend(reverse=TRUE),
                    labels = own_labels) +
  theme_classic() +
  theme(axis.title.x = element_text(size=12, face="plain"),
        axis.title.y = element_text(size=12, face="plain"),
        axis.text.x  = element_text(size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.ticks.length = unit(.2, "cm"),
        legend.text = element_text(size = 12, face = "plain"),
        legend.title = element_text(size = 12, face = "plain"),
        legend.position = c(0.8,0.7)) +
  labs(title = "Random mixing", y = "Number of clusters", x = "Size of cluster") 
gg_res_1

###############################################################################
# extracting the transmissions

# We could just get all the edges from the graph we need to use the full data anyway

g1_df<- igraph::as_data_frame(res_1$graph) # This is useful.
g1_df <- g1_df[,c("from", "to")]

source("tests_sh/transmission_functions.R")

res_1_deets <- cases_deets_function(g1_df)
res_1_trans <- res_1_deets$transmissions
res_1_props <- res_1_deets$props_df
table(res_1_trans$trans)

#########################################################################################

# create a subgraph of the i-graph 
library(igraph)

graph_1 <- res_1$graph
# This is the original plot
L0_1 = layout_with_fr(graph_1)

#plot(graph_1, layout = L0, vertex.label = "") # all cases including singletons
# Set the colours of the cases
graph_species_vect <- vd$Species # get the species from the cases
graph_1 <- igraph::set.vertex.attribute(graph_1, 'species', value = graph_species_vect) # assign the species to the graph 
pal <- c("red", "blue", "yellow") # set a colour palette.


Isolated_1 = which(degree(graph_1)==0)
g_del = delete.vertices(graph_1, Isolated_1)
L02 = L0_1[-Isolated_1,]

plot(g_del, layout=L02, vertex.label = "", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_1, "species")))])


###########################################
## Repeat for the results with shape_2 = 3.6

res_3.6_df <- as.data.frame(res_3.6$clusters$membership) # This is the cluster that they are assigned to. 
colnames(res_3.6_df) <- "cluster_memb"

cs_3.6 <- trans_table_fun(res_3.6_df)

csl_3.6 <- cs_3.6 %>%
  group_by(total, trans_type)%>%
  count()

gg_res_3.6 <- ggplot(csl_3.6, aes(x = total, y = n, fill = trans_type))  +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = own_cols, name = "Transmission type",
                    #guide = guide_legend(reverse=TRUE),
                    labels = own_labels) +
  theme_classic() +
  theme(axis.title.x = element_text(size=12, face="plain"),
        axis.title.y = element_text(size=12, face="plain"),
        axis.text.x  = element_text(size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.ticks.length = unit(.2, "cm"),
        legend.text = element_text(size = 12, face = "plain"),
        legend.title = element_text(size = 12, face = "plain"),
        legend.position = c(0.8,0.7)) +
  labs(title = "Random mixing", y = "Number of clusters", x = "Size of cluster") 
gg_res_3.6

###############################################################################
# extracting the transmissions

# We could just get all the edges from the graph we need to use the full data anyway

g3.6_df<- igraph::as_data_frame(res_3.6$graph) # This is useful.

g3.6_df <- g3.6_df[,c("from", "to")]

res_3.6_deets <- cases_deets_function(g3.6_df)
res_3.6_trans <- res_3.6_deets$transmissions
res_3.6_props <- res_3.6_deets$props_df
table(res_3.6_trans$trans)

#########################################################################################

# create a subgraph of the i-graph 
graph_3.6 <- res_3.6$graph
# This is the original plot
L0_3.6 = layout_with_fr(graph_3.6)

# Set the colours of the cases
graph_3.6 <- igraph::set.vertex.attribute(graph_3.6, 'species', value = graph_species_vect) # assign the species to the graph 

Isolated_3.6 = which(degree(graph_3.6)==0)
g_del_3.6 = delete.vertices(graph_3.6, Isolated_3.6)
L02_3.6 = L0_3.6[-Isolated_3.6,]

plot(g_del_3.6, layout=L02_3.6, vertex.label = "", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_3.6, "species")))])







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

