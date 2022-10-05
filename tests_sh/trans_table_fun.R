trans_table_fun <- function(datafr){

pi1_df <- cbind(SE_Tanz, datafr) %>%
##pi1_df <- datafr %>%
  dplyr::select(District, Latitude, Longitude, ID, Species, Time_diff, cluster_memb) %>%
  droplevels()

#levels(pi1_df$Species)
# Select only those entries that were in a cluster of at least 2

clust_table <- table(pi1_df$cluster_memb)
pi1_df_nosingles <- 
  pi1_df[pi1_df$cluster_memb %in% names(clust_table[clust_table>=2]),]

# Now get the cluster composition by species
cluster_species <- pi1_df_nosingles %>%
  dplyr::select(Species, cluster_memb)
cluster_species <- as.data.frame(t(table(cluster_species)))
cluster_species <- pivot_wider(cluster_species, names_from = "Species", values_from = Freq)
cluster_species[,"total"] <- rowSums(cluster_species[,c(2:4)]) # because using numbers of columns 
# make sure have them correct for different scenarios (cats may not be included in some)

sum(cluster_species[,"total"])  # check matches the number we are expecting.

## assign the transmission - currently doing domestic to include cats. 
## if want dog_only and domestic will need to tweak cos if run all the below, domestic only overwrites dog only 
#cluster_species[which(cluster_species$Dog == cluster_species$total), "trans_type"] <- "dog_only"
cluster_species[which(cluster_species$Wildlife == cluster_species$total), "trans_type"] <- "wildlife_only"
cluster_species[which(cluster_species$Dog + cluster_species$Cat == cluster_species$total), "trans_type"] <- "domestic_only"
cluster_species[is.na(cluster_species$trans_type), "trans_type"] <- "mixed"

return(cluster_species)
}

# csl <- cluster_species %>%
#   group_by(total, trans_type)%>%
#   count()
