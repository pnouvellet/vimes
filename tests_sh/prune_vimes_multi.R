# Vimes_prune function

# we will need to enter a species/groups vector into our function

species_vect <- SE_Tanz$Species

# we have dogs, cats and wildlife. Change it so jsut domestic and wildlife. 
species_vect <- forcats::fct_recode(species_vect, "s1" = "Dog", "s1" = "Cat",
                                    "s2" = "Wildlife" )
species_vect <- droplevels(species_vect)
levels(species_vect)

# just have small one to start
#species_vect <- species_vect[1:40]



## we will also need a vvector of the cut-off values
#cuts_vect <- c(80,150,150)


vimes_prune_multi <-   function(x, cutoff = NULL, species_vect,
         graph_opt = vimes_graph_opt(), ...){
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
  
  # create the species matrix from the species vector
  
  row_mat <- matrix(species_vect, nrow = length(species_vect), ncol = length(species_vect),
                    byrow = T)
  
  
  col_mat <- matrix(species_vect, nrow = length(species_vect), ncol = length(species_vect),
                    byrow = F)
  
  
  sp_mat <- matrix(paste0(row_mat, col_mat), nrow = length(species_vect),
                   ncol = length(species_vect),  byrow = F)
  
  
  #sp_mat

  # now we need to change the matrix values to be 1-3
  # s1s1 = 1, s1s2 and s2s1 = 2 and s2s2 = 3
  # make a matrix that is all 2s initially
  
  sp_mat_numbers <- matrix(2,ncol = length(species_vect), nrow = length(species_vect))
  #sp_mat_numbers
  
  sp_mat_numbers[which(sp_mat == "s1s1")] <- 1
  sp_mat_numbers[which(sp_mat == "s2s2")] <- 3
  
  #head(sp_mat)
  #head(sp_mat_numbers)
  
  
  cuts_mat <- matrix(cutoff[sp_mat_numbers], ncol = length(species_vect),
                     nrow = length(species_vect), byrow = F)
  # head(cuts_mat) 
  
  # x is a dist object that is formed within the higher level of the vimes function
  # temporarily set this as one of the lists
  
  #x<-x[[1]]
  
  new_x <- 1 - (as.matrix(x) > cuts_mat)

  #new_x <- 1 - (as.matrix(x)> cutoff)
  g <- igraph::graph.adjacency(new_x, mode = "undirected",
                               weighted = TRUE, diag = FALSE)
  
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
}
