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
# vd <- vd[c(1:40),]


## make a matrix of the location data so it has the two coordinates for each variable in one place
new <- as.matrix(vd[,c("UTM.Easting", "UTM.Northing")])
plot(new)

D_dates <- dist(vd$Time_diff)  ## this just calculates upper triangle of matrix of pairwise distances
D_dates

head(new)
D_geo <- rdist(new)/1000 ## Divide by 1000 to make in to km. rdist computes the distances between points

#We format, match and plot the distance data using \emph{vimes}:

## Combine the two together in a 'vimes_data format
#D_all <- vimes_data(dates = D_dates, geo = D_geo)

# The function below is the vimes data function

#function(...){
  ## Data passed through ... are meant to be pairwise distances between
  ## labelled cases; we process these inputs by i) turning them into matrices
  ## ii) ensuring matching of labels iii) converting data into 'dist'
  ## objects. Final labels are stored as attributes of the returned list of
  ## 'dist' objects.
  
  
  ## PROCESS TYPES OF INPUT  ##
  ## extract data from list ##
  
  #data <- list(...)
  data <- list(dates = D_dates, geo = D_geo)
 
   data_names <- names(data)
  if (length(data) == 0L) {
    stop("no data to process")
  }
  
  ## escape if data has been processed already
  if (inherits(data[[1]], "vimes_data")) {
    return(data[[1]])
  }
  
  ## if first item is a list, use it as input
  if (is.list(data[[1]])) {
    data <- data[[1]]
  }
  
  
  ## ENSURE MATRICES AND LABELLING ## 
  ## convert all data to matrices
  data <- lapply(data, as.matrix)  # produces a matrix with all the pairs and the distances between them
  K <- length(data) # number of lists 
  
  ## assign labels if missing - appears to just assign the rownames and column names as numbers 1:n
  for (i in seq_along(data)) {
    if (is.null(rownames(data[[i]]))) {
      rownames(data[[i]]) <- colnames(data[[i]]) <- 1:nrow(data[[i]])
    }
  }
  
  
  ## HANDLE NAS AND SORTING ##
  
  ## The policy will be to remove cases with NAs, as cases with NA
  ## distances tend to link all other cases. This may change in
  ## later version of the method.
  
  ## get labels to keep
  ## (i.e. present without NA everywhere)
  lab_to_keep <- Reduce(intersect,
                        lapply(data,
                               function(e) rownames(e)[!apply(is.na(e),1,all)])
  )
  N <- length(lab_to_keep)
  if(N<2){
    warning("Data contain less than 2 cases without missing data.")
    return(NULL)
  }
  
  ## remove NAs, order, store result
  out <- vector(K, mode="list") # empty list to put the data lists in
  for(i in seq_along(data)){
    out[[i]] <- stats::as.dist(data[[i]][lab_to_keep, lab_to_keep]) # makes them into dist objects
      }
  
  
  ## RETURN OUTPUT ##
  
  ## Output will be a list of 'dist' objects; labels and the number
  ## of cases will be stored as attributes; the output will be a
  ## list with the additional class vimes_data
  names(out) <- data_names
  attr(out, "labels") <- lab_to_keep
  attr(out, "N") <- N
  
  class(out) <- c("list", "vimes_data")
  return(out)
#}
  
  # So the function bascially seems to check that data are not all NA and then makes the
  # matrices into dist objects and combines them in a list. 
  
  #These data are then fed into vimes::vimes
  # for example the following might be used:
  
  # vimes(D_all, cutoff = cuts,
  # graph.opt = vimes.graph.opt(col.pal = funky))
  
   
  # need to set some cut-off values. These will have been generated from the get_quantiles
  # function
  # for now just manually set
  # this will to be set up differently as we will have multiplte levels for the cuts
  
  cuts <- c(88, 4.5)

#  vimes::vimes 

# function below

# function(x, method = c("basic"),
#          cutoff = NULL,
#         log_dens = NULL,
#         graph_opt = vimes_graph_opt(), ...){
 
  x = out # set an entry for x
  
   ## CHECKS ##
  ## basic checks
  if(is.null(x)) stop("x is NULL")
  if(!is.list(x)) stop("x is not a list")
  if(!inherits(x, "vimes_data")) stop("x is not a vimes_data object")
  K <- length(x)  # number of types of data in list - here we have 2 - time and space
  
  cutoff = cuts # set up our cutoff values as not set in the function
  
  if(!is.null(cutoff)) cutoff <- rep(cutoff, length=K)
  x.labels <- names(x) # names them as before - dates and geo. 
  
  ## check method used
  # would have set in function but as not in function will do here
  #method = "basic"
  #method <- match.arg(method)
  
  #graph_opt = vimes:::vimes_graph_opt(col.pal = funky)
   # need to set this for the lines below
  ## MAKE SEPARATE GRAPHS ##
  all_graphs <- list() # create an empty list to put the graphs in 
  for (i in seq_along(x)) {
    ## call the prune method
   # if (method=="basic") {
      all_graphs[[i]] <- vimes_prune(x[[i]],
                                     cutoff = cutoff[i])#,
#                                     graph_opt = vimes:::vimes_graph_opt(col.pal = 
 #                                                                          funky))
    }
  #}
  
  ## GET MAIN GRAPH ##
  ## intersect graphs ##
  g <- do.call(igraph::intersection,
               lapply(all_graphs, function(e) e$graph))
  
  ## set graphical options ##
  ## disable weights
  graph_opt$edge.label <- FALSE
  g <- set_igraph_opt(g, graph_opt) # this is a vimes function. Can see if with vimes:::set_igraph_opt
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
#}
  
  
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

