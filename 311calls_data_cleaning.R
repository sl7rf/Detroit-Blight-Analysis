# Data Preporcessing
library(splitstackshape)
library(sqldf)
library(plyr)
library(reshape)
setwd("~/Documents/Coursera/Capstone Project Detroit/data")

calls <- read.csv('detroit-311.csv', header=T, stringsAsFactors=FALSE)

### EXPLORE 311 calls DATASET ### 
summary(calls)
# filter coordinate outliers
calls <- calls[calls$lng > -83.3 & calls$lng < -82.8 & calls$lat > 42.2 & calls$lat < 42.5,]
# remove NAs
sum(is.na(calls$address))
calls <- calls[!is.na(calls$address),]

# Parse the street address
calls$address <- sapply(strsplit(calls$address,"\\ Detroit"), '[', 1)
summary(calls)

# create a subset with useful features
calls <- calls[c('ticket_id','issue_type','rating','address','lat','lng')]


# 311 call geo distribution
cnt311Agg = aggregate(ticket_id ~ lat+lng, data=calls, length)
names(cnt311Agg) = c('lat','lng','freq')
orderCnt311Agg = cnt311Agg[order(cnt311Agg$freq, decreasing = T),]
orderCnt311Agg[1:10,]
library(ggmap)
calls_map = get_googlemap(center=c(lon=-83.1088, lat=42.3843), maptype = "roadmap", 
                          size = c(640, 640), zoom = 11)
ggmap(calls_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCnt311Agg[1:1000,], color = ("orange")) 

# issue
cntIss311Agg = aggregate(ticket_id ~ issue_type, data=calls, length)
names(cntIss311Agg) = c('issue_type','count')
orderIss311Agg = cntIss311Agg[order(cntIss311Agg$count, decreasing = T),]
orderIss311Agg

# Replace all the low frequency issues by 'OtherIssue'
low_freq_issue <- as.character(orderIss311Agg[orderIss311Agg$count<100, 1])
calls$issue_type <-ifelse(calls$issue_type %in% low_freq_issue, 'OtherIssue', calls$issue_type)
unique(calls$issue_type)

library(dummies)
calls_dummy <- dummy.data.frame(calls, names=c('issue_type'),  sep='_')
# aggregate crimes by address
calls_subset <-calls_dummy[,c(2:17,19)]
calls_cnt_address <- aggregate(.~address, data = calls_subset, sum)

# create 311 building list
calls_bld_list <- sqldf('SELECT address, avg(lat) as Lat, avg(lng) AS Lon, count(ticket_id) as N_calls, max(rating) as Max_Rating
                        FROM calls
                        GROUP BY address
                        HAVING stdev(lat) <= 0.0001 AND stdev(lng) <= 0.0001')
calls_bld_list

calls_bld_list <- plyr::join(calls_bld_list,calls_cnt_address,type='inner')
write.csv(calls_bld_list, 'calls_building_list.csv')

