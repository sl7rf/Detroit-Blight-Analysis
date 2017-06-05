# Data Preporcessing
library(splitstackshape)
library(sqldf)
library(plyr)
library(reshape)
setwd("~/Documents/Coursera/Capstone Project Detroit/data")
Permits <- read.csv('detroit-demolition-Permits.tsv', sep = '\t', header = T, stringsAsFactors=FALSE) 

### EXPLORE PERMITS DATASET ### 
Permits <- as.data.frame(Permits)
sum(Permits$site_location=='') # 803 empty site_locations

# Parsing latitude and longitude
split_site_loc <- cSplit(Permits, 'site_location','\n')
sum(is.na(split_site_loc$site_location_3)) # 858 empty coordinates

split_site_loc$site_lat_lng <- gsub("[()]", "", split_site_loc$site_location_3) # Remove parenthesis 
split_site_lat_lng <- cSplit(split_site_loc, 'site_lat_lng', ',')
Permits$LAT <- split_site_lat_lng$site_lat_lng_1
Permits$LON <- split_site_lat_lng$site_lat_lng_2

# remove NAs
Permits <- Permits[!is.na(Permits$LAT),]

# Check the statistics of coordinates
summary(Permits$LAT)
summary(Permits$LON)
# Remove the outlier
Permits <- Permits[Permits$LON > -83.3 & Permits$LON < -82.8,]
Permits <- Permits[Permits$LAT > 42.2 & Permits$LAT < 42.5,]

# Permits distribution
cntPmtAgg = aggregate(PERMIT_NO ~ LAT+LON, data=Permits, length)
names(cntPmtAgg) = c('lat','lng','freq')
orderCntPmtAgg = cntPmtAgg[order(cntPmtAgg$freq, decreasing = T),]
orderCntPmtAgg[1:10,]

library(ggmap)
pmt_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                        size = c(640, 640), zoom = 11)
ggmap(pmt_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCntPmtAgg[1:1000,], 
                            color = ("green")) 


# Convert parcel size into a circle and compute parcel radius
Permits$PARCEL_RADIUS <- sqrt(Permits$PARCEL_SIZE/pi)

# Create a building list: Aggreate by address and set the precision 0.0001 degree for each building
###
# Each degree of latitude is approximately 69 miles (111 kilometers) apart. 
# Length of 1 degree of Longitude = cosine (latitude in decimal degrees) * length of degree (miles) at equator
eps = 0.0001
distance = cos(42.38/180*pi)*11.1 # for 0.0001 degree difference longitude at 42.38 latitude
# define a radius: 0.0001 degree standard dev
rr = sqrt(2)*distance # 11.6 meter ~ 38 ft
###

pmt_bld_list <- sqldf('SELECT SITE_ADDRESS, count(PERMIT_NO) as N_Permits, 
                      avg(PARCEL_RADIUS) as PARCEL_RADIUS, avg(LAT) as LAT, avg(LON) AS LON 
                      FROM Permits 
                      WHERE PARCEL_RADIUS IS NOT NULL
                      GROUP BY SITE_ADDRESS
                      HAVING stdev(LAT) <= 0.0001 AND stdev(LON) <= 0.0001')
head(pmt_bld_list)
# Convert parcel radius from feet into degree range
pmt_bld_list$PARCEL_DEGREE <- pmt_bld_list$PARCEL_RADIUS/38*0.0001
write.csv(pmt_bld_list, 'permit_building_list.csv')
