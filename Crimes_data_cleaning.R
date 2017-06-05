# Data Preporcessing
library(splitstackshape)
library(sqldf)
library(reshape)
setwd("~/Documents/Coursera/Capstone Project Detroit/data")
crimes <- read.csv('detroit-crime.csv', header=T, stringsAsFactors=FALSE)


### EXPLORE crimes DATASET ###
summary(crimes)
# filter coordinate outliers
crimes <- crimes[crimes$LON > -83.3 & crimes$LON < -82.8 & crimes$LAT > 42.2 & crimes$LAT < 42.45,]
crimes <- crimes[!is.na(crimes$LAT),]

# create a subset of crimes data with useful features
crimes <- crimes[c('CASEID','CATEGORY', 'ADDRESS','LON','LAT')]

# Ploting crime distribution
cntCrmAgg = aggregate(CASEID ~ LAT+LON, data=crimes, length)
names(cntCrmAgg) = c('lat','lng','freq')
orderCntCrmAgg = cntCrmAgg[order(cntCrmAgg$freq, decreasing = T),]
orderCntCrmAgg[1:10,]
library(ggmap)
Calls_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                          size = c(640, 640), zoom = 11)
ggmap(Calls_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCntCrmAgg, color = ("orange")) 


# summarize crime category distribution
unique(crimes$CATEGORY)
cntCtgCrmAgg = aggregate(CASEID ~ CATEGORY, data=crimes, length)
names(cntCtgCrmAgg) = c('Category','count')
orderCntCtgCrmAgg = cntCtgCrmAgg[order(cntCtgCrmAgg$count, decreasing = T),]
orderCntCtgCrmAgg

# Replace all the low frequency crimes by 'OtherCrimes'
low_freq_crime <- orderCntCtgCrmAgg[orderCntCtgCrmAgg$count<500, 1]
low_freq_crime <- as.character(low_freq_crime)
crimes$CATEGORY <- ifelse(crimes$CATEGORY %in% low_freq_crime, 'OtherCrimes', crimes$CATEGORY)
unique(crimes$CATEGORY)
# Change crime category to dummy variables
library(dummies)
crimes_dummy <- dummy.data.frame(crimes, names=c('CATEGORY'),  sep='_')
# aggregate crimes by address
crimes_subset <-crimes_dummy[,2:22]
crimes_cnt_address <- aggregate(.~ADDRESS, data = crimes_subset, sum)
# Create building list
crm_bld_list <- sqldf('SELECT ADDRESS, avg(LAT) as Lat, avg(LON) as Lon, count(CASEID) as N_crimes 
                      FROM crimes 
                      GROUP BY ADDRESS
                      HAVING stdev(LAT) <= 0.0001 AND stdev(LON) <= 0.0001')
head(crm_bld_list)
crm_bld_list <- plyr::join(crm_bld_list,crimes_cnt_address,type='inner')
write.csv(crm_bld_list, 'crimes_building_list.csv')
