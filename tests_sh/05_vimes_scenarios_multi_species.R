## 06/12/2021
## Generate the cut-off values for the specified scenarios that I wish to examine with vimes. 
## Then use these values to generate the clusters and evaluate the differences between scenarios. 


rm(list=ls())
library(devtools)
library(vimes)
library(fields)
library(tidyverse)

# Bring in the data we are going to use
source("tests_sh/prep_data_for_vimes.R")

set.seed(1234)

## number of observed cases for each species
s1_obs <- 313 # no of cases observed for domestic/species 1
s2_obs <- 236 # no of cases observed for wildlife/species 2

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


source("R/get_quantiles_multi.R")

# For the serial interval we are keeping the parameters the same for the domestic animals and wildlife
# so we have fewer scenarios to explore

si_cut_offs_gamma <- as.data.frame(matrix(ncol = 8, nrow = 3))

colnames(si_cut_offs_gamma) <- c("dom_rr", "wild_rr", "cut_s1s1", "cut_mix", "cut_s2s2",
                                 "prop_s1s1", "prop_mix", "prop_s2s2")

si_cut_offs_gamma[,1] <- c(0.5, 0.5, 0.7)
si_cut_offs_gamma[,2] <- c(0.5, 0.3, 0.5)


out_si_gamma <- purrr::map2(si_cut_offs_gamma[,1], si_cut_offs_gamma[,2], get_quantiles_multi, 
            d_type = "temporal", distrib = "gamma", s1_obs = s1_obs, s2_obs = s2_obs, 
            n = 10000000, q = 0.95, params_s1s1 = params_s1s1, 
            params_s1s2 = params_s2s2, params_s2s2 = params_s2s2)


length(out_si_gamma)

for (i in 1:length(out_si_gamma)) {
  si_cut_offs_gamma[i,3:8] <- c(out_si_gamma[[i]]$threshold_s1s1,
                                out_si_gamma[[i]]$threshold_s1s2,
                                out_si_gamma[[i]]$threshold_s2s2,
                                out_si_gamma[[i]]$prop_s1s1,
                                out_si_gamma[[i]]$prop_s1s2,
                                out_si_gamma[[i]]$prop_s2s2)
}

## Now with the lognormal

si_cut_offs_ln <- as.data.frame(matrix(ncol = 8, nrow = 3))

colnames(si_cut_offs_ln) <- c("dom_rr", "wild_rr", "cut_s1s1", "cut_mix", "cut_s2s2",
                                 "prop_s1s1", "prop_mix", "prop_s2s2")

si_cut_offs_ln[,1] <- c(0.5, 0.5, 0.7)
si_cut_offs_ln[,2] <- c(0.5, 0.3, 0.5)


out_si_ln <- purrr::map2(si_cut_offs_ln[,1], si_cut_offs_ln[,2], get_quantiles_multi, 
                            d_type = "temporal", distrib = "lognormal", s1_obs = s1_obs, s2_obs = s2_obs, 
                            n = 10000000, q = 0.95, params_s1s1 = params_s1s1, 
                            params_s1s2 = params_s2s2, params_s2s2 = params_s2s2)


for (i in 1:length(out_si_ln)) {
  si_cut_offs_ln[i,3:8] <- c(out_si_ln[[i]]$threshold_s1s1,
                                out_si_ln[[i]]$threshold_s1s2,
                                out_si_ln[[i]]$threshold_s2s2,
                                out_si_ln[[i]]$prop_s1s1,
                                out_si_ln[[i]]$prop_s1s2,
                                out_si_ln[[i]]$prop_s2s2)
}

#### For the spatial parameter we are going to look at more combinations

params_s1s1_sp <- c(rayleigh_mean)
params_s1s2_sp <- c(rayleigh_mean)
params_s2s2_sp <- c(rayleigh_mean)

spatial_cut_offs <- as.data.frame(matrix(ncol = 11, nrow = 9))

colnames(spatial_cut_offs) <- c("dom_rr", "wild_rr", "p_s1s1", "p_s1s2", "p_s2s2",
                                "cut_s1s1", "cut_mix", "cut_s2s2",
                              "prop_s1s1", "prop_mix", "prop_s2s2")

spatial_cut_offs[,1] <- c(rep(0.5, 2), 0.7, rep(0.5,6))
spatial_cut_offs[,2] <- c(0.5, 0.3, 0.5, 0.5, 0.5, rep(0.3, 4))
spatial_cut_offs[,"p_s1s1"] <- rep(rayleigh_mean, 9)
spatial_cut_offs[,"p_s1s2"] <- c(rep(rayleigh_mean, 7), rayleigh_mean*2, rayleigh_mean*2)
spatial_cut_offs[,"p_s2s2"] <- c(rep(rayleigh_mean, 3), rep(c(rayleigh_mean*2, rayleigh_mean*5),3))



out_spatial <- purrr::pmap(spatial_cut_offs[,1], spatial_cut_offs[,2], get_quantiles_multi, 
                         d_type = "spatial", s1_obs = s1_obs, s2_obs = s2_obs, 
                         n = 10000000, q = 0.95, 
                         #s1_rr = spatial_cut_offs[,"dom_rr"],
                         #s2_rr = spatial_cut_offs[,"wild_rr"],
                         params_s1s1 = spatial_cut_offs[,"p_s1s1"], 
                         params_s1s2 = spatial_cut_offs[,"p_s1s2"],
                         params_s2s2 = spatial_cut_offs[,"p_s2s2"])

list_vect <- list(s1_rr = spatial_cut_offs[,1], 
                  s2_rr = spatial_cut_offs[,2], 
                  params_s1s1 = spatial_cut_offs[,"p_s1s1"], 
                  params_s1s2 = spatial_cut_offs[,"p_s1s2"],
                  params_s2s2 = spatial_cut_offs[,"p_s2s2"])



list_vect 

out_spatial <- purrr::pmap(list_vect, get_quantiles_multi, 
                           d_type = "spatial", s1_obs = s1_obs, s2_obs = s2_obs, 
                           n = 10000000, q = 0.95)


for (i in 1:length(out_spatial)) {
  spatial_cut_offs[i,6:11] <- c(out_spatial[[i]]$threshold_s1s1,
                             out_spatial[[i]]$threshold_s1s2,
                             out_spatial[[i]]$threshold_s2s2,
                             out_spatial[[i]]$prop_s1s1,
                             out_spatial[[i]]$prop_s1s2,
                             out_spatial[[i]]$prop_s2s2)
}


### Make a dataframe that has all the combinations of cut_off values that we want. 

merge_sis_gamma <- si_cut_offs_gamma
merge_sis_gamma[4:5,] <- merge_sis_gamma[1,]
merge_sis_gamma[6:9,] <- merge_sis_gamma[2,]
merge_sis_gamma <- merge_sis_gamma[,1:5]
colnames(merge_sis_gamma) <- c("dom_rr_si", "wild_rr_si", "si_cut_s1s1", "si_cut_mix", "si_cut_s2s2")



merge_sis_ln <- si_cut_offs_ln
merge_sis_ln[4:5,] <- merge_sis_ln[1,]
merge_sis_ln[6:9,] <- merge_sis_ln[2,]
merge_sis_ln <- merge_sis_ln[,1:5]
colnames(merge_sis_ln) <- c("dom_rr_si", "wild_rr_si", "si_cut_s1s1", "si_cut_mix", "si_cut_s2s2")

merge_spatial <- spatial_cut_offs[,1:8]
colnames(merge_spatial) <- c("dom_rr_sp","wild_rr_sp",
                             "p_s1s1_sp",  "p_s1s2_sp", "p_s2s2_sp",
                             "cut_s1s1_sp", "cut_mix_sp" ,"cut_s2s2_sp")

merge_sis_gamma <- cbind(merge_sis_gamma, merge_spatial)
which(merge_sis_gamma$dom_rr_si != merge_sis_gamma$dom_rr_sp)
which(merge_sis_gamma$wild_rr_si != merge_sis_gamma$wild_rr_sp)

merge_sis_ln <- cbind(merge_sis_ln, merge_spatial)
which(merge_sis_ln$dom_rr_si != merge_sis_ln$dom_rr_sp)
which(merge_sis_ln$wild_rr_si != merge_sis_ln$wild_rr_sp)


##########################################################################################
##### We now have all the values for the cut-offs that we need. 
##### Next step is to use these within vimes to get the outputs we need


# organise the data we are going to use
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

# make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])

# Computing pairwise distances between cases in time, space,

#Distances between dates are computed as numbers of days:
head(vd$Time_diff, 5)
D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix

head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. 

## Combine the two together in a 'vimes_data format
D_all <- vimes_data(dates = D_dates, geo = D_geo)
par(mar = c(2,2,2,2))
plot(D_all, nclass = 60) # plot of all our pairwise distances.

## For the multi-species vimes we need a vector of the species in our data

species_vect <- vd$Species
table(species_vect)

# we have dogs, cats and wildlife. Change it so just domestic and wildlife. 
species_vect <- forcats::fct_recode(species_vect, "s1" = "Dog", "s1" = "Cat",
                                    "s2" = "Wildlife" )
species_vect <- droplevels(species_vect)
levels(species_vect)

### Also need cut-off vectors

si_cut_df_gamma <- merge_sis_gamma[,c("si_cut_s1s1", "si_cut_mix", "si_cut_s2s2")]
si_cut_df_ln <- merge_sis_ln[,c("si_cut_s1s1", "si_cut_mix", "si_cut_s2s2")]
dist_cut_df <- merge_spatial[,c("cut_s1s1_sp", "cut_mix_sp", "cut_s2s2_sp")]

si_cuts_list_gamma <- as.list(as.data.frame(t(si_cut_df_gamma)))
si_cuts_list_ln <- as.list(as.data.frame(t(si_cut_df_ln)))
dist_cuts_list <- as.list(as.data.frame(t(dist_cut_df)))

#si_cuts_list_gamma[[1]]

cuts_list_gamma <- vector("list", length = length(dist_cuts_list))
for (i in 1:length(cuts_list_gamma)){
  cuts_list_gamma[[i]] <- c(si_cuts_list_gamma[i], dist_cuts_list[i])
}

cuts_list_ln <- vector("list", length = length(dist_cuts_list))
for (i in 1:length(cuts_list_ln)){
  cuts_list_ln[[i]] <- c(si_cuts_list_ln[i], dist_cuts_list[i])
}

cuts_list_gamma[[1]]
cuts_list_ln[[1]]


source("tests_sh/vimes_prune_multi_function.R")
source("tests_sh/vimes_multi_function.R")

vimes_res_list_gamma <- purrr::map(cuts_list_gamma, vimes_multi, x = D_all, method = c("basic"),log_dens = NULL, 
                             species_vect = species_vect, graph.opt = vimes.graph.opt(col.pal = funky))

vimes_res_list_ln <- purrr::map(cuts_list_ln, vimes_multi, x = D_all, method = c("basic"),log_dens = NULL, 
                                   species_vect = species_vect, graph.opt = vimes.graph.opt(col.pal = funky))


### We want to extract the number, size and composition of the clusters
### First look at the gamma distribution results
table_list <- vector(mode = "list", length = 9)

for (i in 1:length(vimes_res_list_gamma)) {
table_list[[i]] <-  table(vimes_res_list_gamma[[i]]$clusters$size) 
}

## table of all the cluster sizes
cluster_size_table_gamma <- plyr::ldply( table_list, rbind)

##### Now want to try and get the composition of the clusters

member_list_gamma <- vector(mode = "list", length = length(vimes_res_list_gamma))
for(i in 1:length(vimes_res_list_gamma)) {
  member_list_gamma[[i]] <- as.data.frame(vimes_res_list_gamma[[i]]$clusters$membership) 
  colnames(member_list_gamma[[i]]) <- "cluster_memb"
}


class(member_list_gamma[[1]])
head(member_list_gamma[[4]])

# create a list of data frames where SE_Tanz is linked to the cluster membership
member_list_gamma <- purrr::map(member_list_gamma, cbind, SE_Tanz) 
head(member_list_gamma[[1]])
# And then get the composition of each cluster
cluster_comp_list_gamma <- purrr::map(member_list_gamma, trans_table_fun)
head(cluster_comp_list_gamma[[3]])

### and a summary of the clusters
csl_fun <- function(df){
  df %>% 
    group_by(total, trans_type) %>%
    count()
}

# csl_fun(cluster_comp_list_gamma[[1]])
cluster_summary_list_gamma <- purrr::map(cluster_comp_list_gamma, csl_fun)
cluster_summary_list_gamma[[7]]

own_cols <- c("red", "purple", "blue")
own_labels <- c("Domestic only", "Mixed", "Wildlife only")

plot_fun <- function(df){
  ggplot(df, aes(x = total, y = n, fill = trans_type))  +
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
  labs(title = "", y = "Number of clusters", x = "Size of cluster")
}


# plot_fun(cluster_summary_list_gamma[[3]])

clust_plots_gamma <- purrr::map(cluster_summary_list_gamma, plot_fun)

par(mfrow = c(3,3))
clust_plots_gamma[[1]]

p1g <- clust_plots_gamma[[1]]
p2g <- clust_plots_gamma[[2]]
p3g <- clust_plots_gamma[[3]]
p4g <- clust_plots_gamma[[4]]
p5g <- clust_plots_gamma[[5]]
p6g <- clust_plots_gamma[[6]]
p7g <- clust_plots_gamma[[7]]
p8g <- clust_plots_gamma[[8]]
p9g <- clust_plots_gamma[[9]]

library(ggpubr)
ggarrange(p1g, p2g, p3g, p4g, p5g, p6g, p7g, p8g, p9g, 
          ncol = 3, nrow = 3,
          common.legend = TRUE, legend = "bottom")


### The last thing would be nice to have an igraph for each scenario
pal <- c("red", "blue", "red") # set a colour palette.
species_vect_graph <- vd$Species # get the species from the cases
#species_vect_graph <- forcats::fct_recode(species_vect_graph, "Domestic" = "Dog", "Domestic" = "Cat",
#                                          "Wildlife" = "Wildlife" )
species_vect_graph <- droplevels(species_vect_graph)
levels(species_vect_graph)


graph_dfs_gamma <- vector(mode = "list", length = length(vimes_res_list_gamma))
for(i in 1:length(vimes_res_list_gamma)){
  graph_dfs_gamma[[i]] <- vimes_res_list_gamma[[i]]$graph
  graph_dfs_gamma[[i]] <- igraph::as_data_frame(graph_dfs_gamma[[i]])
  graph_dfs_gamma[[i]] <- igraph::graph_from_data_frame(graph_dfs_gamma[[i]], directed = F)
  rn <- as.numeric(get.vertex.attribute(graph_dfs_gamma[[i]])$name)
  graph_dfs_gamma[[i]] <- igraph::set.vertex.attribute(graph_dfs_gamma[[i]], 'species', value = species_vect_graph[rn]) # assign the species to the graph 
 } 


par(mfrow = c(3,3))
plot(graph_dfs_gamma[[1]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[1]], "species")))])
plot(graph_dfs_gamma[[2]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[2]], "species")))])
plot(graph_dfs_gamma[[3]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[3]], "species")))])
plot(graph_dfs_gamma[[4]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[4]], "species")))])
plot(graph_dfs_gamma[[5]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[5]], "species")))])
plot(graph_dfs_gamma[[6]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[6]], "species")))])
plot(graph_dfs_gamma[[7]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[7]], "species")))])
plot(graph_dfs_gamma[[8]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[8]], "species")))])
plot(graph_dfs_gamma[[9]], vertex.label ="", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_dfs_gamma[[9]], "species")))])


g1 <- vimes_res_list_gamma[[1]]$graph
g1 <- igraph::set.vertex.attribute(g1, 'species', value = species_vect_graph) # assign the species to the graph 
g1 <- igraph::as_data_frame(g1)
g1 <- igraph::graph_from_data_frame(g1, directed = F)

rn <- as.numeric(get.vertex.attribute(g1)$name)
#species_vect_graph[rn]

g1 <- igraph::set.vertex.attribute(g1, 'species', value = species_vect_graph[rn]) # assign the species to the graph 
plot(g1, vertex.label = "",
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_1, "species")))])

#graph_1 <- multi_res$graph  # assign the graph to an object
#class(graph_1) # check it's an igraph

rn <- as.numeric(get.vertex.attribute(g1)$name)
species_vect_graph[rn]
graph_1 <- igraph::set.vertex.attribute(graph_1, 'species', value = species_vect[rn]) # assign the species to the graph 


# And plot 
plot(graph_1, vertex.label = "",
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_1, "species")))])





#######################
# What we want
# - size of clusters and their distribution
# - composition of clusters
# - types of transmission (but bear in mind many of these won't be direct transmissions)

# Fairly straightforward to get distribution of cluster sizes direct from Vimes object

# Could be added to data so can extract details about each case. 

#hist(multi_res$clusters$size, col = "pink", xlab = "Size of cluster",breaks = seq(0,9,1),
#     main = "Histogram of cluster sizes")$counts

multi_res$clusters$K # number of clusters including singletons
table(multi_res$clusters$size)  #tells us the the size of the assigned clusters
multi_res$cutoff  # the cutoff values that were used. 


multi_res <- vimes_multi(D_all, cutoff = cuts, species_vect = species_vect,
                         graph.opt = vimes.graph.opt(col.pal = funky))


res_95 <- as.data.frame(matrix(ncol = 2, nrow = 549))
colnames(res_95) <- c("ID", "Cluster")
res_95[,"ID"] <- 1:549
res_95[,"Cluster"] <- multi_res$clusters$membership

#use some pre-defined functions to extract some details about the transmissions.

source("tests_sh/transmission_functions.R")
source("tests_sh/trans_table_fun.R")

multi_res_graph <- multi_res$graph
multi_res_gdf <- igraph::as_data_frame(multi_res_graph)

multi_res_cases_details <- cases_deets_function(multi_res_gdf)
multi_res_cases_details$props_df
multi_res_trans <- multi_res_cases_details$transmissions

## Add in which cluster these are in
res_95$ID <- as.character(res_95$ID)

multi_res_trans_2 <- dplyr::select(multi_res_trans_2, !c(weight_1, weight_2, label.color_1, label.color_2, label.color))


###########################################################################
# The cluster membership can be joined with the SE_Tanz data to create a new data set. 

res_df <- as.data.frame(multi_res$clusters$membership) # This is the cluster that they are assigned to. 
colnames(res_df) <- "cluster_memb"


source("tests_sh/trans_table_fun.R")
cs <- trans_table_fun(res_df)

csl <- cs %>%
  group_by(total, trans_type)%>%
  count()





############## PLOT CODES  ######################################
### Below is the code to plot a barplot of the different clusters. 

own_cols <- c("red", "purple", "blue")
own_labels <- c("Domestic only", "Mixed", "Wildlife only")

ggplot(csl, aes(x = total, y = n, fill = trans_type))  +
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
  labs(title = "", y = "Number of clusters", x = "Size of cluster") 


## We could get some info from the transmissions from this info - 
# we know all the transmissions in the domestic only are domestic-domestic and 
# all the ones in wildlife only are wildlife-wildlife
# Still need to distentangle further for the mixed groups


#########################################################################################
#########################################################################################

# create a subgraph of the i-graph 
library(igraph)

# This is the original plot
L0 = layout_with_fr(graph_1)

plot(graph_1, layout = L0, vertex.label = "")

Isolated = which(degree(graph_1)==0)
g_del = delete.vertices(graph_1, Isolated)
L02 = L0[-Isolated,]

plot(g_del, layout=L02, vertex.label = "", 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_1, "species")))])


## NOT SURE WE USED BELOW CURRENTLY



###############################################################################
# extracting the transmissions

# We could just get all the edges from the graph we need to use the full data anyway

g1_df<- igraph::as_data_frame(graph_1) # This is useful.
g1_df <- g1_df[,c("from", "to")]


source("tests_sh/transmission_functions.R")

gamma_95_res <- cases_deets_function(g1_df)

gamma_95_trans <- gamma_95_res$transmissions
gamma_95_props <- gamma_95_res$props_df


# quick check to check that we don't have any duplicates in the transmissions
# combine the two animals
fromto  <- paste(gamma_95_trans[,"from"], gamma_95_trans[,"to"], sep = "") 
tofrom <- paste(gamma_95_trans$to, gamma_95_trans$from,sep = "")
all_trans <- c(fromto,tofrom)

length(which(duplicated(fromto, tofrom)))

## add in which cluster they are each from

gamma_95_res




