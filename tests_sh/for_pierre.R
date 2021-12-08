## Running the multi vimes stage and extracting the cluster information. 
## using multi_prune and vimes_multi


rm(list=ls())
library(devtools)
library(vimes)
library(fields)
library(dplyr)

source("tests_sh/prep_data_for_vimes.R")

# Bring in the data we are going to use
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

# make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])

# Computing pairwise distances between cases in time, space,

#Distances between dates are computed as numbers of days:
head(vd$Time_diff, 5)
D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix

head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. 

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
D_all <- vimes_data(dates = D_dates, geo = D_geo)
par(mar = c(2,2,2,2))
plot(D_all, nclass = 60) # plot of all our pairwise distances.

## For the multi-species vimes we need a vector of the species in our data

species_vect <- vd$Species
# we have dogs, cats and wildlife. Change it so just domestic and wildlife. 
species_vect <- forcats::fct_recode(species_vect, "s1" = "Dog", "s1" = "Cat",
                                    "s2" = "Wildlife" )
species_vect <- droplevels(species_vect)
levels(species_vect)


### Also need cut-off vectors

cuts <- list()
cuts[[1]] <- c(166, 166, 166) # These are the SI cuts
cuts[[2]] <- c(2.646, 2.641, 2.639) # These are the distance cuts

source("tests_sh/vimes_prune_multi_function.R")
source("tests_sh/vimes_multi_function.R")


multi_res <- vimes_multi(D_all, cutoff = cuts, species_vect = species_vect,
                         graph.opt = vimes.graph.opt(col.pal = funky))


## To get the matrix I fun the vimes_prune bit separately and take results from all_graphs

dates_mat <- all_graphs[[1]]$prune_mat
dists_mat <- all_graphs[[2]]$prune_mat

#write.csv(dates_mat, "tests_sh/temp_trash/dates_matrix.csv")
#write.csv(dists_mat, "tests_sh/temp_trash/distances_matrix.csv")


plot(multi_res$graph, vertex.label = "")
multi_res$clusters$membership

res_95 <- as.data.frame(matrix(ncol = 2, nrow = 549))
colnames(res_95) <- c("ID", "Cluster")
res_95[,"ID"] <- 1:549
res_95[,"Cluster"] <- multi_res$clusters$membership

#write.csv(res_95, "tests_sh/temp_trash/res_95.csv")

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

# join by from and to just to check they give same result
multi_res_trans_2 <- left_join(multi_res_trans, res_95, by = c("from" = "ID"))
multi_res_trans_2 <- left_join(multi_res_trans_2, res_95, by = c("to" = "ID"))

## check they aren't any discrepancies

nrow(multi_res_trans_2[which(multi_res_trans_2$Cluster.x != multi_res_trans_2$Cluster.y),])
#Trim unnecessary columns

multi_res_trans_2 <- dplyr::select(multi_res_trans_2, !c(weight_1, weight_2, label.color_1, label.color_2, label.color))

#write.csv(multi_res_trans_2, "tests_sh/temp_trash/to_from_clusters.csv")

## 
#######################
# What we want
# - size of clusters and their distribution
# - composition of clusters
# - types of transmission (but bear in mind many of these won't be direct transmissions)

# Fairly straightforward to get distribution of cluster sizes direct from Vimes object

# Could be added to data so can extract details about each case. 

multi_res$clusters$size  #tells us the the size of the assigned clusters
hist(multi_res$clusters$size, col = "pink", xlab = "Size of cluster",breaks = seq(0,9,1),
     main = "Histogram of cluster sizes")$counts

multi_res$clusters$K # number of clusters including singletons
multi_res$cutoff  # the cutoff values that were used. 

## Would be good to be able to plot just the non - singletons and to colour them by species
## presently only worked out how to colour - can't seem to drop the singletons

graph_1 <- multi_res$graph  # assign the graph to an object
class(graph_1) # check it's an igraph

species_vect <- vd$Species # get the species from the cases

graph_1 <- igraph::set.vertex.attribute(graph_1, 'species', value = species_vect) # assign the species to the graph 

pal <- c("red", "blue", "yellow") # set a colour palette.

# And plot 
plot(graph_1, vertex.label = "",
     vertex.color = pal[as.numeric(as.factor(igraph::vertex_attr(graph_1, "species")))])


###########################################################################
# The cluster membership can be joined with the SE_Tanz data to create a new data set. 

res_df <- as.data.frame(multi_res$clusters$membership) # This is the cluster that they are assigned to. 
colnames(res_df) <- "cluster_memb"


source("tests_sh/trans_table_fun.R")
cs <- trans_table_fun(res_df)

#write.csv(cs, "tests_sh/temp_trash/cluster_size_and_composition.csv")

csl <- cs %>%
  group_by(total, trans_type)%>%
  count()

#write.csv(csl, "tests_sh/temp_trash/cluster_size_comp_summary.csv")

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



#####################################################################################
#### Other thing that might be useful would be to look at the types of transmission

# This is assuming single chain and basing purely on time. 
# Might not be a suitable method? Unlikely to be useful at lower reporting rates
# as won't know what the missing links are. 


##### NEED TO WORK ON THE BELOW

cluster_df <- cbind(vd, res_df)

cluster_df$cluster_memb <- as.factor(cluster_df$cluster_memb)
cluster_df$cluster_memb <- droplevels(cluster_df$cluster_memb)
levels(cluster_df$cluster_memb)

clusters_vect <- cluster_df$cluster_memb

df_list <- list()
for (j in 1:length(clusters_vect)) {
  df <- cluster_df %>%
    dplyr::filter(cluster_memb == clusters_vect[j]) %>%
    dplyr::select(Time_diff, Species) %>%
    arrange(Time_diff)
  for (i in 1:nrow(df)-1) {
    df[i,"trans_type"] <- paste(df[i, "Species"], df[i+1, "Species"], sep ="-")
  }
  df_list[[j]] <- df
}

all_dfs <- plyr::rbind.fill(df_list)

(table(all_dfs$trans_type)/sum(table(all_dfs$trans_type)))*100




