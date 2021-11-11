## Below is the vimes_data function 
## Putting here to allow examination of it.

# Run the function with a subset of our data

rm(list=ls())
library(devtools)
library(vimes)
library(fields)

# Bring in the data we are going to use
source("tests_sh/prep_data_for_vimes.R")

## select the complete cases for time. 
vd <- SE_Tanz[complete.cases(SE_Tanz$Time_diff),]

## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])
#plot(new)

D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix of pairwise distances
#D_dates

#head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. rdist computes the distances between points

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
#D_all <- vimes_data(dates = D_dates, geo = D_geo)

## create the vimes_data object.

D_all <- vimes_data(dates = D_dates, geo = D_geo)

#These data are then fed into vimes::vimes
# for example the following might be used:

# vimes(D_all, cutoff = cuts,
# graph.opt = vimes.graph.opt(col.pal = funky))


# need to set some cut-off values. These will have been generated from the get_quantiles
# function
# for now just manually set
# this will to be set up differently as we will have multiplte levels for the cuts

# cuts <- c(88, 4.5)

cuts <- list()
cuts[[1]] <- c(100,150,150)
cuts[[2]] <- c(3.0, 2.5, 4.0)


#also need the species vector 
species_vect <- SE_Tanz$Species
# we have dogs, cats and wildlife. Change it so jsut domestic and wildlife. 
species_vect <- forcats::fct_recode(species_vect, "s1" = "Dog", "s1" = "Cat",
                                    "s2" = "Wildlife" )
species_vect <- droplevels(species_vect)
levels(species_vect)

#cuts[[1]]

#  vimes::vimes 

# function below

x <- D_all

vimes_multi <- function(x, method = c("basic"),
          cutoff = NULL,
         log_dens = NULL,
        graph_opt = vimes_graph_opt(), ...){

#x = D_all # set an entry for x

## CHECKS ##
## basic checks
if(is.null(x)) stop("x is NULL")
if(!is.list(x)) stop("x is not a list")
if(!inherits(x, "vimes_data")) stop("x is not a vimes_data object")
K <- length(x)  # number of types of data in list - here we have 2 - time and space

cutoff = cuts # set up our cutoff values as not set in the function

if(!is.null(cutoff)) cutoff <- rep(cutoff, length=K)
x.labels <- names(x) # names them as before - dates and geo. 

#x.labels

## check method used
# would have set in function but as not in function will do here
#method = "basic"
method <- match.arg(method)

#graph_opt = vimes:::vimes_graph_opt(col.pal = funky)
# need to set this for the lines below
## MAKE SEPARATE GRAPHS ##
all_graphs <- list() # create an empty list to put the graphs in 
for (i in seq_along(x)) {
  ## call the prune method
   if (method=="basic") {
  all_graphs[[i]] <- vimes_prune_multi(x[[i]],
                                 cutoff = cutoff[[i]], species_vect = species_vect,
                                       graph_opt = vimes_graph_opt())
}
}

## GET MAIN GRAPH ##
## intersect graphs ##
g <- do.call(igraph::intersection,
             lapply(all_graphs, function(e) e$graph))

## set graphical options ##
## disable weights
graph_opt$edge.label <- FALSE
g <- vimes:::set_igraph_opt(g, graph_opt) # this is a vimes function. Can see if with vimes:::set_igraph_opt
# it appears to bascially set the colours and attributes of the graph

## find clusters ##
groups <- igraph::clusters(g)
names(groups) <- c("membership", "size", "K")

## add cluster colors ##
groups$color <- graph_opt$col_pal(groups$K)
names(groups$color) <- 1:groups$K

## ADJUST SEPARATE GRAPHS FEATURES ##
for(i in seq_along(x)){
  ## vertex color ##
  igraph::V(all_graphs[[i]]$graph)$color <- igraph::V(g)$color
  
  ## layout ##
  all_graphs[[i]]$graph$layout <- g$layout
}
names(all_graphs) <- x.labels


## The output will contain the main graph, cluster definitions,
## the cutoff values used, and then similar information for each
## individual graph (one per original distance matrix).

out <- list(graph = g,
            clusters = groups,
            cutoff = cutoff,
            separate_graphs = all_graphs)

return(out)
}



vm_output <- vimes_multi(D_all, cutoff = cuts,
       graph.opt = vimes.graph.opt(col.pal = funky))


plot(vm_output$graph, vertex.label = "")


############################################################  
## We need to take a closer look at the vimes_prune function as this is where the magic happens....

# This one just produces the separate graphs for each data stream 
# It looks like The 'vimes' function then uses 'intersection' to get the main graph. 

# vimes_prune is called like this in the vimes function
#for (i in seq_along(x)) {
## call the prune method
# if (method=="basic") {
# all_graphs[[i]] <- vimes_prune(x[[i]],
#                               cutoff = cutoff[i])#,
#                                     graph_opt = vimes:::vimes_graph_opt(col.pal = 
#                                                                          funky))
# so it does each part of x separately. 

# Lets look st the dates first
x = x[[1]]

#  function(x, cutoff = NULL,
#           graph_opt = vimes_graph_opt(), ...){
# select the first of our cutoffs too 
cutoff = cuts[1]
## CHECKS ##
if (is.null(x)) {
  stop("input data is NULL")
}

## INTERACTIVE MODE FOR CHOOSING CUTOFF ##
#if (is.null(cutoff)) {
#  return(cutoff_choice_interactive(x = x,
#                                   graph_opt = graph_opt))
#}


## BUILD GRAPH ##

## In the following we create a pruned graph, derive corresponding
## clusters, create new graphical attributes for the graph (mostly
## coloring clusters).

new_x <- 1 - (as.matrix(x)> cutoff)  # This line converts x to a matrix, generates T/F if 
# the value in the matrix is above the cut-off, then subtracts this from 1 to get a matrix of 0,1
g <- igraph::graph.adjacency(new_x, mode = "undirected",
                             weighted = TRUE, diag = FALSE)
# plot(g) # this is then turned into an igraph object


## find clusters ##
groups <- igraph::clusters(g)
names(groups) <- c("membership", "size", "K")

## add cluster colors
groups$color <- graph_opt$col_pal(groups$K)
names(groups$color) <- 1:groups$K

## Here we add new graphical properties to the graph that will
## ultimately be returned.

g <- vimes:::set_igraph_opt(g, graph_opt)

## The returned output should be a self-sufficient list containing
## the pruned graph, cluster definition, and cutoff values used.

out <- list(graph = g, clusters = groups, cutoff = cutoff)
#  }

