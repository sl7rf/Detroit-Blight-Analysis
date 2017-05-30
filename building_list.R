setwd("~/Documents/Coursera/Capstone Project Detroit/data")
viols_bld_list <- read.csv('viols_building_list.csv', header=T)
crm_bld_list <- read.csv('crimes_building_list.csv', header=T)
calls_bld_list <- read.csv('calls_building_list.csv', header=T)

colnames(viols_bld_list)[2] <- 'ADDRESS'
colnames(calls_bld_list)[2] <- 'ADDRESS'

# row bind buildings from those incidents
viols_bld_list$TYPE <- 'Violations'
crm_bld_list$TYPE <- 'Crimes'
calls_bld_list$TYPE <- '311 Calls'

all_bld_list <- rbind(viols_bld_list[c('ADDRESS', 'LAT', 'LON', 'TYPE')],
                      crm_bld_list[c('ADDRESS', 'LAT', 'LON', 'TYPE')],
                      calls_bld_list[c('ADDRESS', 'LAT', 'LON', 'TYPE')])
all_bld_list <- cbind(c(1:nrow(all_bld_list)), all_bld_list)
colnames(all_bld_list)[1] <- 'INCIDENT_ID'

# Cluster the buildings whose lat/long coordinates that are around +/-0.0002 degrees appart
library(doParallel)
n.cores <- detectCores()
registerDoParallel(n.cores)
n.cores <- getDoParWorkers()


all_bld_list$Assigned <- 0
i <- 0
input <- all_bld_list
l <- nrow(input)
temp_list <- NULL
while(l > 0) {
  input <- input[input$Assigned==0,]
  lat <- input[1,]$LAT
  lon <- input[1,]$LON
  input$Assigned <- ifelse( (abs(lon-input$LON)<=0.0002 & abs(lat-input$LAT)<=0.0002), 1, 0)
  i <- i+1
  out <- input[input$Assigned==1,]  
  out$BLD_ID <- i
  out$BLD_LAT <- mean(out$LAT)
  out$BLD_LON <- mean(out$LON)
  
  temp_list <- rbind(temp_list, out)
  
  l <- nrow(input[input$Assigned==0,])
  if(l == 0) break
  if(i%%1000==0) print(paste0(i," iterations ",l," records left"))
} 

View(temp_list)
all_bld_list_new <- temp_list
temp_list <- NULL
write.csv(all_bld_list_new, 'all_bld_list_new.csv')

# total we have 80462 building clusters, now we can aggregate the variables by building_id
temp_viols <- all_bld_list_new[all_bld_list_new$TYPE=='Violations', ]
temp_viols <- temp_viols[order(temp_viols$INCIDENT_ID, decreasing = F),]
temp_viols$N_Viols <- viols_bld_list$N_Violations
temp_viols$Max_TotalFee <- viols_bld_list$Max_TotalFee
temp_viols_list <- sqldf('SELECT BLD_ID, BLD_LAT, BLD_LON, sum(N_Viols) as N_Viols, max(Max_TotalFee) as Max_TotalFee
                         FROM temp_viols
                         GROUP BY BLD_ID')

temp_crms <- all_bld_list_new[all_bld_list_new$TYPE=='Crimes', ]
temp_crms <- temp_crms[order(temp_crms$INCIDENT_ID, decreasing = F),]
temp_crms$N_Crimes <- crm_bld_list$N_Crimes
temp_crms$CATEGORY <- crm_bld_list$CATEGORY

# convert CATEGORY into dummy variables
library(dummies)
temp_crms <- dummy.data.frame(temp_crms, names=c('CATEGORY'),  sep='_')
crime_category <- temp_crms[, c(7, 11:56)]
AggCrm <- aggregate(.~ BLD_ID, data = crime_category , FUN = sum )
temp_crms_list <- sqldf('SELECT BLD_ID, BLD_LAT, BLD_LON, sum(N_Crimes) as N_Crimes
                         FROM temp_crms
                         GROUP BY BLD_ID')
temp_crms_list <- cbind(temp_crms_list, AggCrm)

temp_calls <- all_bld_list_new[all_bld_list_new$TYPE=='311 Calls', ]
temp_calls <- temp_calls[order(temp_calls$INCIDENT_ID, decreasing = F),]
temp_calls$N_Calls <- calls_bld_list$N_Calls
temp_calls$Max_Rating <- calls_bld_list$Max_Rating
temp_calls_list <- sqldf('SELECT BLD_ID, BLD_LAT, BLD_LON, sum(N_Calls) as N_Calls, max(Max_Rating) as Max_Rating
                          FROM temp_calls
                          GROUP BY BLD_ID')
all_list <- sqldf('SELECT * FROM temp_viols_list 
                  FULL JOIN temp_crms_list ON temp_viols_list.BLD_ID = temp_crms_list.BLD_ID
                  FULL JOIN temp_calls_list ON temp_viols_list.BLD_ID = temp_calls_list.BLD_ID')

