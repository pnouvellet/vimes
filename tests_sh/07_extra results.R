# would be nice to have a plot of the singletons and when they occurred. 

# first need to extract which ones are the singletons. 

singles_1 <- 
  res_1_df %>% group_by(cluster_memb) %>%
  dplyr::filter(n() == 1) %>%
  ungroup()

singles_1 <- unlist(singles_1$cluster_memb)

vd_singles <- vd[singles_1,]

# now need counts of singletons by each month

singles_by_month <- vd_singles %>%
  group_by(month_n) %>%
  count()

dev.off()

plot(singles_by_month, ylim = c(0,20))


##############
## code to plot the clusters on a map
library(rgdal)

study_regs <- c("Mtwara", "Lindi")

district_shp <- rgdal::readOGR("tests_sh/gis", "TZ_District_2012_pop") # Shape files for the districts of Tanzania
plot(district_shp)

#study_dis <- district_shp[district_shp$Region_Nam %in% study_regs,]
study_dis <- subset(district_shp, district_shp$Region_Nam %in% study_regs)
plot(study_dis)

clusters_vect <-cs_1$cluster_memb
class(clusters_vect)

cluster_df <- cbind(SE_Tanz, res_1_df)
colnames(cluster_df)
cluster_df$cluster_memb <- as.factor(cluster_df$cluster_memb)

cluster_df <- cluster_df[which(cluster_df$cluster_memb %in% clusters_vect),]

xy <- cluster_df[,c("Longitude", "Latitude")]
spdf <- SpatialPointsDataFrame(coords = xy, data = cluster_df,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
levels(spdf$Species)

spdf$cluster_memb <- droplevels(spdf$cluster_memb)

n_cols <-length(unique(spdf$cluster_memb)) # set how many colours we need if colouring by cluster

# cols = c("red", "blue", "yellow") # use this if colouring by species
# use below if colouring by cluster
cols = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n_cols)

#
pts = c(17,19, 15) # setting these to use for species

plot(study_dis)
plot(spdf, add = T, pch = pts[spdf$Species], jitter = T, col = cols[spdf$cluster_memb]) #work out how to change point shape and colour by cluster.

##################################################
### This map shows all the clusters of 2 or more. 

### would be good to also plot locations of singletons


singles_xy <- vd_singles[,c("Longitude", "Latitude")]
singles_spdf <- SpatialPointsDataFrame(coords = singles_xy, data = vd_singles,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#
pts = c(17,19, 15) # setting these to use for species

plot(study_dis)
plot(singles_spdf, add = T, pch = pts[spdf$Species], jitter = T)#,
#col = cols[spdf$cluster_memb]) #work out how to change point shape and colour by cluster.

# And maybe a plot of the clusters of >=3

clusters_vect_trios <- unlist(cs_1[which(cs_1$total >2),"cluster_memb"])
class(clusters_vect_trios)

cluster_df_trios <- cluster_df[which(cluster_df$cluster_memb %in% clusters_vect_trios),]

xy_trios <- cluster_df_trios[,c("Longitude", "Latitude")]
spdf_trios <- SpatialPointsDataFrame(coords = xy_trios, data = cluster_df_trios,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
levels(spdf_trios$Species)

spdf_trios$cluster_memb <- droplevels(spdf_trios$cluster_memb)

n_cols_trios <-length(unique(spdf_trios$cluster_memb)) # set how many colours we need if colouring by cluster

# cols = c("red", "blue", "yellow") # use this if colouring by species
# use below if colouring by cluster
cols_trios = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n_cols_trios)
cols_trios = colorRampPalette(RColorBrewer::brewer.pal(11, "Paired"))(n_cols_trios)


#
pts = c(17,19, 15) # setting these to use for species

plot(study_dis)
plot(spdf_trios, add = T, pch = pts[spdf_trios$Species], jitter = T, 
     col = cols_trios[spdf_trios$cluster_memb]) #work out how to change point shape and colour by cluster.







