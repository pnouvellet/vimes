## 08/12/2021
## Looking at assortativity
## We are going to use all the cases in our data for the numbers. 
## Domestic will be species 1 - this is dogs and cats. 
## Wildlife will be all wildlife species and will be species 2.
## We are going to use a larger distance for S2S2 transmissions to represent the
## larger home ranges of jackals cf dogs

## Reporting probability of 0.25 for group 1 and 0.10 for group 2
## with the parameters the same for both for the serial interval

# for the spatial kernel we will keep the baseline mean for all 

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
s1_rr <- 0.25 # rr for dogs/species 1
s2_rr <- 0.10 # rr for jackals/species 2

n = 10000000 # set number of simulations - using 10 million initially #as algorithm very quick

q <- 0.95 # set the level of the quantile we want to use later

# generate the parameters for use within the simulation
si_mean <- 27.8175438596491
#si_sd <- 26.8565433014125
si_sd <- 36.8565433014125
rayleigh_mean <- 0.87

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
params_s1s2_spatial <- c(rayleigh_mean)
params_s2s2_spatial <- c(rayleigh_mean)


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
#write.csv(si_res_df, "tests_sh/final_scenarios/si_95_0.25_0.10_1_100.csv")
#write.csv(dist_res_df, "tests_sh/final_scenarios/dist_95_0.25_0.10_1_100.csv")


si_res_df <- read.csv("tests_sh/final_scenarios/si_95_0.25_0.10_1_100.csv")
dist_res_df <- read.csv("tests_sh/final_scenarios/dist_95_0.25_0.10_1_100.csv")

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
trans_res_df[4,2:92] <- colSums(trans_res_df[1:3, 2:92])


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

colnames(sim_props) <- colnames(trans_res_df)

trans_res_df <- rbind(trans_res_df, sim_props)

trans_res_df[8, 2:92] <- round(trans_res_df[which(trans_res_df$trans_type == "s1_props_mean"),2:92]*
                                 trans_res_df[which(trans_res_df$trans_type == "total"),2:92],2)
trans_res_df[8,1] <- "exp_s1s1"


trans_res_df[9, 2:92] <- round(trans_res_df[which(trans_res_df$trans_type == "mixed_props_mean"),2:92]*
                                 trans_res_df[which(trans_res_df$trans_type == "total"),2:92],2)
trans_res_df[9,1] <- "exp_mixed"

trans_res_df[10, 2:92] <- round(trans_res_df[which(trans_res_df$trans_type == "s2_props_mean"),2:92]*
                                  trans_res_df[which(trans_res_df$trans_type == "total"),2:92],2)
trans_res_df[10,1] <- "exp_s2s2"

trans_res_df[11,2:92] <- round(colSums(trans_res_df[c(8,9,10), 2:92]),0)
trans_res_df[11,1] <- "exp_total"

trans_res_df[11, 2:92] - trans_res_df[4, 2:92]

## Adding chi-sqaured to the table

trans_res_df[12,1] <-  "Xi_sq"

trans_res_df[12, 2:92] <-  
  (trans_res_df[which(trans_res_df$trans_type == "s1s1"),2:92] - trans_res_df[which(trans_res_df$trans_type == "exp_s1s1"), 2:92])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_s1s1"), 2:92] +
  
  (trans_res_df[which(trans_res_df$trans_type == "mixed"),2:92] - trans_res_df[which(trans_res_df$trans_type == "exp_mixed"), 2:92])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_mixed"), 2:92] +
  
  (trans_res_df[which(trans_res_df$trans_type == "s2s2"),2:92] - trans_res_df[which(trans_res_df$trans_type == "exp_s2s2"), 2:92])^2/ trans_res_df[which(trans_res_df$trans_type == "exp_s2s2"), 2:92]

# Plot the values
plot(shape_vect, trans_res_df[12,2:92], xlab = "Value of shape 2", ylab = "Chi squared value")

## also want the observed proportions
trans_res_df[13,2:92] <- trans_res_df[2,2:92]/trans_res_df[4,2:92]
trans_res_df[13,1] <- "obs_prop_s1s1"

trans_res_df[14,2:92] <- trans_res_df[1,2:92]/trans_res_df[4,2:92]
trans_res_df[14,1] <- "obs_prop_mixed"

trans_res_df[15,2:92] <- trans_res_df[3,2:92]/trans_res_df[4,2:92]
trans_res_df[15,1] <- "obs_prop_s2s2"

sum(trans_res_df[13:15, 2:92])

range(trans_res_df[12, 2:92])
plot(shape_vect, trans_res_df[12,2:92], xlab = "Value of shape 2", ylab = "Chi squared value",
     ylim = c(0,5))

write.csv(trans_res_df, "tests_sh/trans_res_dfs/trans_res_g.csv")
range(trans_res_df[4,2:92])

pchisq(1.841936, df = 1, lower.tail = F)

#########################################################################

# Find the column that has the lowest value for the chi-squared test

colnames(trans_res_df)[apply(trans_res_df, 1, which.min)][12]

grep("2.4", colnames(trans_res_df))

trans_res_df[,16]

#This shows the column with the value of 2.4 has the lowest chi squared value
# We can extract this from our vimes results list. 
# Need to know which of the results we want. This will correspond to the number in the shape_vect
which(shape_vect == 2.4)

par(mfrow = c(2,1))
shape <- 2.4
r <- rbeta(n = 1e4, shape1 = 1, shape2 = shape)
hist(r, breaks = seq(0,1,by=.02), main = "Species 1", xlab = "", col = "red")

p <- rbeta(n = 1e4, shape1 = shape, shape2 = 1)
hist(p, breaks = seq(0,1, by = 0.02), main = "Species 2", xlab = "", col ="blue")

###########################################################
## Will now extract some results from the vimes results using the assortativity value at 1 and at 2.4

res_1 <- vimes_res_list[[1]]
res_1$cutoff

res_2.4 <- vimes_res_list[[15]]
res_2.4$cutoff

### Results with shape = 1

table(res_1$clusters$size)  #tells us the the size of the assigned clusters
res_1$clusters$K
mean(res_1$clusters$size)

178/549

range(res_1$clusters$size) #the range of cluster sizes
hist(res_1$clusters$size, col = "pink", xlab = "Size of cluster", breaks = seq(-1,86,1),
     main = "Histogram of cluster sizes")

res_1$clusters$K # number of clusters including singletons

# The cluster membership can be joined with the SE_Tanz data to create a new data set. 

res_1_df <- as.data.frame(res_1$clusters$membership) # This is the cluster that they are assigned to. 
colnames(res_1_df) <- "cluster_memb"

source("tests_sh/trans_table_fun.R")
cs_1 <- trans_table_fun(res_1_df)

csl_1 <- cs_1 %>%
  group_by(total, trans_type)%>%
  count()

csl_1_trios_up <- csl_1[which(csl_1$total >=3),]

sum(csl_1_trios_up$n) # so 24 clusters of >= 3 in total.
sum(csl_1_trios_up[which(csl_1_trios_up$trans_type %in% c("domestic_only", "wildlife_only")), "n"])

# get the proportion
sum(csl_1_trios_up[which(csl_1_trios_up$trans_type %in% c("domestic_only", "wildlife_only")), "n"])/
  sum(csl_1_trios_up$n) # so 24 clusters of >= 3 in total.

# plot of the clusters
own_cols <- c("red", "purple", "blue")
own_labels <- c("Domestic only", "Mixed", "Wildlife only")

dev.off()
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
  xlim(0,15) +
  ylim(0,55) +
  labs(title = "A", y = "Number of clusters", x = "Size of cluster") 

gg_res_1

# ALternative way of extracting the transmissions

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


species_vect_graph <- vd$Species # get the species from the cases
species_vect_graph <- droplevels(species_vect_graph)
levels(species_vect_graph) 
pal <- c("red", "blue", "red") # set a colour palette.Make sure the colours match the species from the species vector



graph_1 <- res_1$graph
graph_1 <- igraph::as_data_frame(graph_1)
graph_1 <- igraph::graph_from_data_frame(graph_1, directed = F)
rn <- as.numeric(get.vertex.attribute(graph_1)$name)
graph_1 <-  igraph::set.vertex.attribute(graph_1, 'species', value = species_vect_graph[rn]) # assign the species to the graph 

plot(graph_1, vertex.label ="", vertex.size = 10,
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_1, "species")))])


###########################################
## Repeat for the results with shape_2 = 2.4

table(res_2.4$clusters$size)  #tells us the the size of the assigned clusters
res_2.4$clusters$K

range(res_2.4$clusters$size) #the range of cluster sizes
#hist(res_2.4$clusters$size, col = "pink", xlab = "Size of cluster", breaks = seq(-1,9,1),
#     main = "Histogram of cluster sizes")

mean(res_2.4$clusters$size) # number of clusters including singletons
176/549

# The cluster membership can be joined with the SE_Tanz data to create a new data set. 

res_2.4_df <- as.data.frame(res_2.4$clusters$membership) # This is the cluster that they are assigned to. 
colnames(res_2.4_df) <- "cluster_memb"

cs_2.4 <- trans_table_fun(res_2.4_df)

csl_2.4 <- cs_2.4 %>%
  group_by(total, trans_type)%>%
  count()

csl_2.4_trios_up <- csl_2.4[which(csl_2.4$total >=3),]

sum(csl_2.4_trios_up$n) # so 24 clusters of >= 3 in total.
sum(csl_2.4_trios_up[which(csl_2.4_trios_up$trans_type %in% c("domestic_only", "wildlife_only")), "n"])

# get the proportion
sum(csl_2.4_trios_up[which(csl_2.4_trios_up$trans_type %in% c("domestic_only", "wildlife_only")), "n"])/
  sum(csl_2.4_trios_up$n) # so 24 clusters of >= 3 in total.

# plot of the clusters


gg_res_2.4 <- ggplot(csl_2.4, aes(x = total, y = n, fill = trans_type))  +
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
  xlim(0,15) +
  ylim(0,55) +
  labs(title = "B", y = "Number of clusters", x = "Size of cluster") 

gg_res_2.4

#########################################################################################

# create a subgraph of the i-graph 

graph_2.4 <- res_2.4$graph
graph_2.4 <- igraph::as_data_frame(graph_2.4)
graph_2.4 <- igraph::graph_from_data_frame(graph_2.4, directed = F)
rn <- as.numeric(get.vertex.attribute(graph_2.4)$name)
graph_2.4 <-  igraph::set.vertex.attribute(graph_2.4, 'species', value = species_vect_graph[rn]) # assign the species to the graph 

plot(graph_2.4, vertex.label ="", vertex.size = 10,
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_2.4, "species")))])


##########
## put the two bar plots side by side.
dev.off()
ggpubr::ggarrange(gg_res_1, gg_res_2.4, ncol = 2, common.legend = T, legend = "bottom") %>%
  ggpubr::ggexport(filename = "tests_sh/plots/gg7.pdf", width = 11, height = 6)


dev.off()
## and pop the igraphs side-by-side
pdf("tests_sh/plots/i7.pdf", height = 5, width = 9)
par(mfrow = c(1,2))
set.seed(2)
plot(graph_1, vertex.label ="", vertex.size = 10, 
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_1, "species")))])
title(main = "A", adj = 0)
set.seed(2)
plot(graph_2.4, vertex.label ="", vertex.size = 10,
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_2.4, "species")))])
title(main = "B", adj = 0)
dev.off()


## plots of singletons
singles_2.4 <- 
  res_2.4_df %>% group_by(cluster_memb) %>%
  dplyr::filter(n() == 1) %>%
  ungroup()

singles_2.4 <- unlist(singles_2.4$cluster_memb)

vd_singles <- vd[singles_2.4,]

# now need counts of singletons by each month

singles_by_month <- vd_singles %>%
  group_by(month_n) %>%
  count()

dev.off()

plot(singles_by_month, ylim = c(0,20))

g_dates <- c("2011", "2013", "2015", "2017", "2019")
g_breaks <- c(0,24,48,72,96)

# figure to include
dev.off()
pdf("tests_sh/plots/singletons_scen7.pdf", height = 4, width = 5)
ggplot(data = singles_by_month, aes(x = month_n, y = n)) + 
  geom_point(col = "turquoise", size = 3) + 
  theme_bw() + 
  scale_x_continuous(breaks = g_breaks, labels = g_dates, limits = c(0,96)) + 
  xlab("Date") + 
  ylab("Number of singletons") + 
  ggtitle("B") + 
  ylim(0,16)

dev.off()


### clusters > 5 on a map

## code to plot the clusters on a map
## Start with all clusters of 2 or more. 
library(rgdal)

study_regs <- c("Mtwara", "Lindi")

district_shp <- rgdal::readOGR("tests_sh/gis", "TZ_District_2012_pop") # Shape files for the districts of Tanzania
#plot(district_shp)

#study_dis <- district_shp[district_shp$Region_Nam %in% study_regs,]
study_dis <- subset(district_shp, district_shp$Region_Nam %in% study_regs)
#plot(study_dis)

clusters_vect <-cs_2.4$cluster_memb
class(clusters_vect)

cluster_df <- cbind(SE_Tanz, res_2.4_df)
colnames(cluster_df)
cluster_df$cluster_memb <- as.factor(cluster_df$cluster_memb)

cluster_df <- cluster_df[which(cluster_df$cluster_memb %in% clusters_vect),]

# xy <- cluster_df[,c("Longitude", "Latitude")]
# spdf <- SpatialPointsDataFrame(coords = xy, data = cluster_df,
#                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# levels(spdf$Species)
# 
# spdf$cluster_memb <- droplevels(spdf$cluster_memb)

n_cols <-length(unique(spdf$cluster_memb)) # set how many colours we need if colouring by cluster

# cols = c("red", "blue", "yellow") # use this if colouring by species
# use below if colouring by cluster
cols = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n_cols)

#
pts = c(17,19, 15) # setting these to use for species

# plot(study_dis)
# plot(spdf, add = T, pch = pts[spdf$Species], jitter = T, col = cols[spdf$cluster_memb]) #work out how to change point shape and colour by cluster.
# 

# And maybe a plot of the clusters of >=5

clusters_vect_fivess <- unlist(cs_2.4[which(cs_1$total >4),"cluster_memb"])
class(clusters_vect_fivess)

cluster_df_fivess <- cluster_df[which(cluster_df$cluster_memb %in% clusters_vect_fivess),]

xy_fives <- cluster_df_fivess[,c("Longitude", "Latitude")]
spdf_fives <- SpatialPointsDataFrame(coords = xy_fives, data = cluster_df_fivess,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
levels(spdf_fives$Species)

spdf_fives$Species <- droplevels(spdf_fives$Species)

spdf_fives$cluster_memb <- droplevels(spdf_fives$cluster_memb)

n_cols_fives <-length(unique(spdf_fives$cluster_memb)) # set how many colours we need if colouring by cluster

 cols = c("red", "blue", "red") # use this if colouring by species
# use below if colouring by cluster
cols_fives = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n_cols_fives)
cols_fives = colorRampPalette(RColorBrewer::brewer.pal(11, "Paired"))(n_cols_fives)

#
levels(spdf_fives$Species)
pts = c(0,1, 0) # setting these to use for species

#colouring by cluster
plot(study_dis, col = "grey")
plot(spdf_fives, add = T, pch = pts[spdf_fives$Species], jitter = T, cex = 1.5,
     col = cols_fives[spdf_fives$cluster_memb]) #work out how to change point shape and colour by cluster.

#colouring by species
plot(study_dis, col = "beige")
plot(spdf_fives, add = T, jitter = T, cex = 1.2, pch = 15,
     col = cols[spdf_fives$Species]) #work out how to change point shape and colour by cluster.


### would be good to also plot locations of singletons

singles_xy <- vd_singles[,c("Longitude", "Latitude")]
singles_spdf <- SpatialPointsDataFrame(coords = singles_xy, data = vd_singles,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#

dev.off()
pdf("tests_sh/plots/map_scen7.pdf", width = 5, height =5)
par(mar = c(1,1,1,1))
plot(study_dis, col = "beige")
plot(singles_spdf, add = T, col = cols[singles_spdf$Species], jitter = T, pch = 15, cex = 0.5)#,
title(main = "B", adj = 0.1)
dev.off()

table(vd_singles$Species)


