## Running the vimes stage and extracting the clusters. 
## initially using manually defined cut-off values as extremes to test the system 

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


###Also need cut-off vectors

cuts <- list()
cuts[[1]] <- c(120, 120, 120) # initially set all the same
cuts[[2]] <- c(1.5, 1.5, 1.5)

source("tests_sh/vimes_prune_multi_function.R")
source("tests_sh/vimes_multi_function.R")


#think need to tweak the vimes multi so that the species_vect is listed in vimes_multi
#rather than multi_prune

multi_res <- vimes_multi(D_all, cutoff = cuts, species_vect = species_vect,
                        graph.opt = vimes.graph.opt(col.pal = funky))


plot(multi_res$graph, vertex.label = "")
multi_res$clusters$membership

#use some pre-defined functions to extract some details about the transmissions.

source("tests_sh/transmission_functions.R")
source("tests_sh/trans_table_fun.R")


multi_res_graph <- multi_res$graph
multi_res_gdf <- igraph::as_data_frame(multi_res_graph)

multi_res_cases_details <- cases_deets_function(multi_res_gdf)
multi_res_cases_details$props_df
multi_res_trans <- multi_res_cases_details$transmissions

## If the cut-offs are all the same, we should get the same results if we 
## run the original vimes function with the same data and single cutoff. 

cuts_single <- c(120, 1.5)

res_vimes_comp <- vimes(D_all, cutoff = cuts_single,
      graph.opt = vimes.graph.opt(col.pal = funky))

comp_res_graph <- res_vimes_comp$graph
comp_res_gdf <- igraph::as_data_frame(comp_res_graph)

comp_res_Case_details <- cases_deets_function(comp_res_gdf)
comp_res_Case_details$props_df
comp_res_trans <- comp_res_Case_details$transmissions

plot(multi_res$graph, vertex.label = "")
plot(comp_res_graph, vertex.label = "")


## all looks to match. Lets compare the two transmission tables

length(which(comp_res_trans != multi_res_trans))

# Lovely. All is well.