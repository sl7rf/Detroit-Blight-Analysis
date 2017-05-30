# Data Preporcessing
library(splitstackshape)
library(sqldf)
library(reshape)
setwd("~/Documents/Coursera/Capstone Project Detroit/data")

Calls <- read.csv('detroit-311.csv', header=T, stringsAsFactors=FALSE)
Viols <- read.csv('detroit-blight-violations.csv', header=T, stringsAsFactors=FALSE)
Crimes <- read.csv('detroit-crime.csv', header=T, stringsAsFactors=FALSE)
Permits <- read.csv('detroit-demolition-Permits.tsv', sep = '\t', header = T, stringsAsFactors=FALSE) 

# make data copies
data_Calls <- Calls
data_Viols <- Viols
data_Crimes <- Crimes
data_Permits <- Permits

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
# Convert to factors
pmt_fac_cols <- c("CASE_TYPE", "CASE_DESCRIPTION", "LEGAL_USE", "BLD_PERMIT_TYPE",
            "PERMIT_DESCRIPTION", "BLD_PERMIT_DESC", "BLD_TYPE_USE", "RESIDENTIAL",
            "DESCRIPTION", "BLD_TYPE_CONST_COD", "BLD_ZONING_DIST", "BLD_USE_GROUP",
            "BLD_BASEMENT", "FEE_TYPE", "CSF_CREATED_BY","CONDITION_FOR_APPROVAL")
Permits[pmt_fac_cols] <- lapply(Permits[pmt_fac_cols], as.factor)

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

### EXPLORE VIOLS DATASET ### 
summary(Viols)
# Parsing latitude and longitude for violation dataset
Viols <- as.data.frame(Viols)
split_Viols_address <- colsplit(Viols$ViolationAddress, split='\n', names=c('street', 'city_state', 'viol_lat_lng'))
split_Viols_address$viol_lat_lng <- gsub("[()]", "", split_Viols_address$viol_lat_lng) # Remove parenthesis 
split_Viols_lat_lng <- colsplit(split_Viols_address$viol_lat_lng, split=',', names=c('lat', 'lng'))
Viols$LAT <- split_Viols_lat_lng$lat
Viols$LON <- split_Viols_lat_lng$lng
Viols$ViolationAddress <- split_Viols_address$street

# define a function to convert $$ to number
cur_to_num <- function(x) as.numeric(gsub('[$,]','', as.character(x)), na.rm=FALSE) # '\\$|,'='[$,]'
cur_cols <- c('FineAmt', 'AdminFee', 'LateFee', 'StateFee', 'CleanUpCost', 'JudgmentAmt')
Viols[cur_cols] <- lapply(Viols[cur_cols], cur_to_num)
Viols$totalfee <- Viols$FineAmt + Viols$AdminFee + Viols$StateFee + Viols$LateFee + Viols$CleanUpCost + Viols$JudgmentAmt
# convert char to factors
viol_fac_cols <- c("ViolationCode","Disposition","PaymentStatus",
                   "Void","ViolationCategory","Country")
Viols[viol_fac_cols] <- lapply(Viols[viol_fac_cols], as.factor)
summary(Viols)

# Blight Violation frequency
# By geo coordinates
cntCorAgg = aggregate(TicketID ~ LAT + LON, data=Viols, length)
names(cntCorAgg) = c('lat','lng','freq')
orderCntCorAgg = cntCorAgg[order(cntCorAgg$freq, decreasing = T),]
orderCntCorAgg[1:10,]
# By street address
cntAddAgg = aggregate(TicketID ~ ViolationAddress, data=Viols, length)
names(cntAddAgg) = c('address','freq')
orderCntAddAgg = cntAddAgg[order(cntAddAgg$freq, decreasing = T),]
orderCntAddAgg[1:10,]
# By violation code
cntCdAgg = aggregate(TicketID ~ ViolationCode, data=Viols, length)
names(cntCdAgg) = c('violation code','freq')
orderCntCdAgg = cntCdAgg[order(cntCdAgg$freq, decreasing = T),]
orderCntCdAgg[1:10,]

# Map Ploting 
library(ggmap)
Viols_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                       size = c(640, 640), zoom = 11)
ggmap(Viols_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCntAgg[1:1000,], color = ("red")) 

# create violation building list
viol_bld_list <- sqldf('SELECT ViolationAddress, avg(LAT) as LAT, avg(LON) AS LON, 
                       count(TicketID) as N_Violations, max(totalfee) as Max_TotalFee
                       FROM Viols 
                       GROUP BY ViolationAddress
                       HAVING stdev(LAT) <= 0.0001 AND stdev(LON) <= 0.0001')
head(viol_bld_list)
write.csv(viol_bld_list, 'viols_building_list.csv')


### EXPLORE 311 CALLS DATASET ### 
summary(Calls)
# filter coordinate outliers
Calls <- Calls[Calls$lng > -83.3 & Calls$lng < -82.8 & Calls$lat > 42.2 & Calls$lat < 42.5,]
# remove NAs
sum(is.na(Calls$address))
Calls <- Calls[!is.na(Calls$address),]
# convert to factors
calls_fac_cols <- c('city','issue_type', 'ticket_status')
Calls[calls_fac_cols] <- lapply(Calls[calls_fac_cols], as.factor)
# Parse the street address
Calls$address <- sapply(strsplit(Calls$address,"\\ Detroit"), '[', 1)

summary(Calls)

# 311 call geo distribution
cnt311Agg = aggregate(ticket_id ~ lat+lng, data=Calls, length)
names(cnt311Agg) = c('lat','lng','freq')
orderCnt311Agg = cnt311Agg[order(cnt311Agg$freq, decreasing = T),]
orderCnt311Agg[1:10,]

cntIss311Agg = aggregate(ticket_id ~ issue_type, data=Calls, length)
names(cntIss311Agg) = c('issue_type','count')

library(ggmap)
Calls_map = get_googlemap(center=c(lon=-83.1088, lat=42.3843), maptype = "roadmap", 
                        size = c(640, 640), zoom = 11)
ggmap(Calls_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCnt311Agg[1:1000,], color = ("orange")) 
# create 311 building list
calls_bld_list <- sqldf('SELECT address, avg(lat) as LAT, avg(lng) AS LON, count(ticket_id) as N_Calls, max(rating) as Max_Rating
                       FROM Calls
                        GROUP BY address
                        HAVING stdev(lat) <= 0.0001 AND stdev(lng) <= 0.0001')
calls_bld_list[1:5,]
write.csv(calls_bld_list, 'calls_building_list.csv')


### EXPLORE CRIMES DATASET ###
summary(Crimes)
# filter coordinate outliers
Crimes <- Crimes[Crimes$LON > -83.3 & Crimes$LON < -82.8 & Crimes$LAT > 42.2 & Crimes$LON < 42.5,]
Crimes <- Crimes[!is.na(Crimes$LAT),]

# convert to factors
crm_fac_cols <- c("CATEGORY","STATEOFFENSEFILECLASS","PRECINCT","COUNCIL","NEIGHBORHOOD")
Crimes[crm_fac_cols] <- lapply(Crimes[crm_fac_cols], as.factor)

summary(Crimes)

# Ploting crime distribution
cntCrmAgg = aggregate(CASEID ~ LAT+LON, data=Crimes, length)
names(cntCrmAgg) = c('lat','lng','freq')
orderCntCrmAgg = cntCrmAgg[order(cntCrmAgg$freq, decreasing = T),]
orderCntCrmAgg[1:10,]
# Ploting crime category distribution
cntCtgCrmAgg = aggregate(CASEID ~ CATEGORY, data=Crimes, length)
names(cntCtgCrmAgg) = c('Category','count')
orderCntCtgCrmAgg = cntCtgCrmAgg[order(cntCtgCrmAgg$count, decreasing = T),]
orderCntCtgCrmAgg[1:10,]

library(ggmap)
Calls_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                         size = c(640, 640), zoom = 11)
ggmap(Calls_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCnt311Agg[1:1000,], color = ("orange")) 

# Create building list
crm_bld_list <- sqldf('SELECT ADDRESS, avg(LAT) as LAT, avg(LON) as LON, count(CASEID) as N_Crimes, category 
                      FROM Crimes 
                      GROUP BY ADDRESS
                      HAVING stdev(LAT) <= 0.0001 AND stdev(LON) <= 0.0001')
head(crm_bld_list)
write.csv(crm_bld_list, 'crimes_building_list.csv')
