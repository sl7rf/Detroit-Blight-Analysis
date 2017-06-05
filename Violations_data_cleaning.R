# Data Preporcessing
library(splitstackshape)
library(sqldf)
library(reshape)
setwd("~/Documents/Coursera/Capstone Project Detroit/data")
violations <- read.csv('detroit-blight-violations.csv', header=T, stringsAsFactors=FALSE)
### EXPLORE violations DATASET ### 

summary(violations)
# Parsing latitude and longitude for violation dataset
violations <- as.data.frame(violations)
split_violations_address <- colsplit(violations$ViolationAddress, split='\n', names=c('street', 'city_state', 'viol_lat_lng'))
split_violations_address$viol_lat_lng <- gsub("[()]", "", split_violations_address$viol_lat_lng) # Remove parenthesis 
split_violations_lat_lng <- colsplit(split_violations_address$viol_lat_lng, split=',', names=c('lat', 'lng'))
violations$LAT <- split_violations_lat_lng$lat
violations$LON <- split_violations_lat_lng$lng
violations$ViolationAddress <- split_violations_address$street
split_violations_address <- NULL
split_violations_lat_lng <- NULL

# define a function to convert $$ to number
cur_to_num <- function(x) as.numeric(gsub('[$,]','', as.character(x)), na.rm=FALSE) # '\\$|,'='[$,]'
cur_cols <- c('FineAmt', 'AdminFee', 'LateFee', 'StateFee', 'CleanUpCost', 'JudgmentAmt')
violations[cur_cols] <- lapply(violations[cur_cols], cur_to_num)
violations$TotalFee <- violations$FineAmt + violations$AdminFee + violations$StateFee + violations$LateFee + violations$CleanUpCost + violations$JudgmentAmt

# convert char to factors
fac_cols <- c("ViolationCode","Disposition","PaymentStatus",
                   "Void","ViolationCategory")
violations[fac_cols] <- lapply(violations[fac_cols], as.factor)
summary(violations)

violations <- violations[c('TicketID', 'ViolationCode', 'TotalFee','ViolationAddress','LAT','LON')]

# Blight Violation frequency
# By geo coordinates
cntCorAgg = aggregate(TicketID ~ LAT + LON, data=violations, length)
names(cntCorAgg) = c('lat','lng','freq')
orderCntCorAgg = cntCorAgg[order(cntCorAgg$freq, decreasing = T),]
orderCntCorAgg[1:10,]
# By street address
cntAddAgg = aggregate(TicketID ~ ViolationAddress, data=violations, length)
names(cntAddAgg) = c('address','freq')
orderCntAddAgg = cntAddAgg[order(cntAddAgg$freq, decreasing = T),]
orderCntAddAgg[1:10,]
# Map Ploting 
library(ggmap)
violations_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                               size = c(640, 640), zoom = 11)
ggmap(violations_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCntCorAgg, color = ("red")) 

# Format violation codes
violations$ViolationCode <- gsub('[/]', '-', violations$ViolationCode)
#remove all characters in parentheses
violations$ViolationCode <- gsub('\\(.*','',violations$ViolationCode)  
violations$ViolationCode <- gsub('\\..*','',violations$ViolationCode)  
# remove space
violations$ViolationCode <- gsub('\\ .*','',violations$ViolationCode)  

unique(violations$ViolationCode)
cntCdAgg = aggregate(TicketID ~ ViolationCode, data=violations, length)
names(cntCdAgg) = c('ViolationCode','freq')
orderCntCdAgg = cntCdAgg[order(cntCdAgg$freq, decreasing = T),]
orderCntCdAgg
# Replace all the low frequency violation codes by 'OtherViolationCode'
low_freq_viol_code <- orderCntCdAgg[orderCntCdAgg$freq<500, ]$ViolationCode
violations$ViolationCode <- ifelse(violations$ViolationCode %in% low_freq_viol_code, 'OtherViolationCode', violations$ViolationCode)
unique(violations$ViolationCode)

# Change violationcode to dummy variables
library(dummies)
violations_dummy <- dummy.data.frame(violations, names=c('ViolationCode'),  sep='_')
# aggregate violation code by address
violations_subset <- violations_dummy[, c(2:22,24)]
violation_code_cnt <- aggregate(x = violations_subset[,1:21], by=list(violations_subset$ViolationAddress), FUN =sum)
colnames(violation_code_cnt)[1] <- 'ViolationAddress'
# create violation building list
viol_bld_list <- sqldf('SELECT ViolationAddress, avg(LAT) as Lat, avg(LON) AS Lon, 
                       count(TicketID) as N_Violations, max(TotalFee) as MaxTotalFee
                       FROM violations 
                       GROUP BY ViolationAddress
                       HAVING stdev(LAT) <= 0.0001 AND stdev(LON) <= 0.0001')
head(viol_bld_list)
viol_bld_list <- join(viol_bld_list, violation_code_cnt, type='inner')
viol_bld_list[is.na(viol_bld_list)] <- 0
write.csv(viol_bld_list, 'violations_building_list.csv')

