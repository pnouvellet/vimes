vimes_multi <- function(x, method = c("basic"),
                        cutoff = NULL,
                        log_dens = NULL,
                        species_vect = species_vect,
                        graph_opt = vimes_graph_opt(), ...){
  
  #x = D_all # set an entry for x
  
  ## CHECKS ##
  ## basic checks
  if(is.null(x)) stop("x is NULL")
  if(!is.list(x)) stop("x is not a list")
  if(!inherits(x, "vimes_data")) stop("x is not a vimes_data object")
  K <- length(x)  # number of types of data in list - here we have 2 - time and space
  
  #cutoff = cuts # set up our cutoff values as not set in the function
  
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

