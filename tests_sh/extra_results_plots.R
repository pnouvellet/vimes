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
     col = cols[spdf$cluster_memb]) #work out how to change point shape and colour by cluster.

#
