setwd("~/Documents/Coursera/Capstone Project Detroit/data")
library(sqldf)
viols_list <- read.csv('violations_building_list.csv', header=T)
crimes_list <- read.csv('crimes_building_list.csv', header=T)
calls_list <- read.csv('calls_building_list.csv', header=T)
colnames(viols_list)[1:2] <- c('Id','Address')
colnames(crimes_list)[1:2] <- c('Id','Address')
colnames(calls_list)[1:2] <- c('Id','Address')
# give unique building ids for each building list
viols_list$Id <- c(1:nrow(viols_list))
crimes_list$Id <- c((nrow(viols_list)+1): (nrow(viols_list)+nrow(crimes_list)))
calls_list$Id <- c((nrow(viols_list)+nrow(crimes_list)+1): (nrow(viols_list)+nrow(crimes_list)+nrow(calls_list)))
# row bind buildings from those incidents
temp_list <- rbind(viols_list[,c(1,3,4)], crimes_list[,c(1,3,4)], calls_list[,c(1,3,4)])
write.csv(temp_list, 'temp_list.csv')

summary(temp_list)

library(ggmap)
Calls_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                          size = c(640, 640), zoom = 11)
ggmap(Calls_map) + geom_point(aes(x=Lon,y=Lat), data=temp_list, color = ("orange")) 

#######
# Run generate_building_list.ipynb to cluster buildings using DBSCAN
#######
bld_list <- read.csv('building_list.csv')
cluster_list <- read.csv('cluster_list.csv')
cluster_list$X <- cluster_list$X + 1
colnames(cluster_list) <- c('cluster_id','cluster_lat','cluster_lon')
# Check building numebrs in clusters
Agg = aggregate(Id~cluster_id, data=bld_list, length)
names(Agg) = c('cluster_id','count')
orderAgg = Agg[order(Agg$count, decreasing = T),]
orderAgg

# Now we re-summarize viols, crimes and calls
viols_list$cluster_id <- bld_list$cluster_id[1:nrow(viols_list)]
viols_list <- viols_list[,5:28]
viols_MaxTotalFee_agg <- aggregate(MaxTotalFee~cluster_id, data = viols_list, FUN = mean)
viols_list$MaxTotalFee <- NULL
viols_new <- aggregate(.~cluster_id, data = viols_list, FUN = sum)
viols_new$MaxTotalFee <- viols_MaxTotalFee_agg$MaxTotalFee

crimes_list$cluster_id <- bld_list$cluster_id[(nrow(viols_list)+1): (nrow(viols_list)+nrow(crimes_list))]
crimes_list <- crimes_list[,5:26]
crimes_new <- aggregate(.~cluster_id, data = crimes_list, FUN = sum)

calls_list$cluster_id <- bld_list$cluster_id[(nrow(viols_list)+nrow(crimes_list)+1): (nrow(viols_list)+nrow(crimes_list)+nrow(calls_list))]
calls_list <- calls_list[,5:23]
calls_new <- aggregate(.~cluster_id, data = calls_list, FUN = sum)

# re-combine the viols, crimes, calls
new_list <- plyr::join(viols_new, crimes_new, by='cluster_id', type='full')
new_list <- join(new_list, calls_new, by='cluster_id', type='full')
new_list$Lat <- cluster_list$cluster_lat
new_list$Lon <- cluster_list$cluster_lon
new_list[is.na(new_list)] <- 0
write.csv(new_list, 'viol_crm_311_list.csv')
###########
# Label blighted properties
new_list <- read.csv('viol_crm_311_list.csv', header=T)
permits_list <- read.csv('permit_building_list.csv', header=T)

# If a cluster is within the range(38 ft) of any building with blight permit, then we label this cluster blighted=1
for (i in (1:nrow(new_list)) ){
  a <- new_list$Lat[i] - permits_list$LAT
  b <- new_list$Lon[i] - permits_list$LON
  new_list$blighted[i] <- ifelse(sum( sqrt(a^2 + b^2)<permits_list$PARCEL_DEGREE+0.0001 ) > 0, 1, 0)
}
new_list$X <- NULL
write.csv(new_list, 'final_clean_data.csv')

