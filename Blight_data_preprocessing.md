Detroit Blight Data Preprocessing
================
Sasa Li

Set up R and load libraries

``` r
library(splitstackshape)
```

    ## Loading required package: data.table

``` r
library(sqldf)
```

    ## Loading required package: gsubfn

    ## Loading required package: proto

    ## Loading required package: RSQLite

``` r
library(plyr)
library(reshape)
```

    ## 
    ## Attaching package: 'reshape'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     rename, round_any

    ## The following object is masked from 'package:data.table':
    ## 
    ##     melt

``` r
library(dummies)
```

    ## dummies-1.5.6 provided by Decision Patterns

Load Data
---------

We are going to explore 4 data files: crimes, violations, 311 calls and permits

``` r
setwd("~/Documents/Coursera/Capstone Project Detroit/data")
crimes <- read.csv('detroit-crime.csv', header=T, stringsAsFactors=FALSE)
calls <- read.csv('detroit-311.csv', header=T, stringsAsFactors=FALSE)
violations <- read.csv('detroit-blight-violations.csv', header=T, stringsAsFactors=FALSE)
permits <- read.csv('detroit-demolition-permits.tsv', sep = '\t', header = T, stringsAsFactors=FALSE) 
```

Explore crimes dataset
======================

``` r
summary(crimes)
```

    ##      ROWNUM           CASEID           INCINO            CATEGORY        
    ##  Min.   :     1   Min.   :1612272   Length:119931      Length:119931     
    ##  1st Qu.: 29984   1st Qu.:1930308   Class :character   Class :character  
    ##  Median : 59966   Median :1960745   Mode  :character   Mode  :character  
    ##  Mean   : 59966   Mean   :1960655                                        
    ##  3rd Qu.: 89948   3rd Qu.:1991078                                        
    ##  Max.   :119931   Max.   :2021621                                        
    ##                                                                          
    ##  OFFENSEDESCRIPTION STATEOFFENSEFILECLASS INCIDENTDATE      
    ##  Length:119931      Min.   : 1000         Length:119931     
    ##  Class :character   1st Qu.:22001         Class :character  
    ##  Mode  :character   Median :26001         Mode  :character  
    ##                     Mean   :45380                           
    ##                     3rd Qu.:89004                           
    ##                     Max.   :99009                           
    ##                     NA's   :8                               
    ##       HOUR            SCA            PRECINCT       COUNCIL         
    ##  Min.   : 0.00   Min.   : 201.0   Min.   : 2.00   Length:119931     
    ##  1st Qu.: 8.00   1st Qu.: 409.0   1st Qu.: 4.00   Class :character  
    ##  Median :14.00   Median : 711.0   Median : 7.00   Mode  :character  
    ##  Mean   :12.91   Mean   : 765.1   Mean   : 7.58                     
    ##  3rd Qu.:18.00   3rd Qu.:1002.0   3rd Qu.:10.00                     
    ##  Max.   :23.00   Max.   :9999.0   Max.   :99.00                     
    ##                  NA's   :771      NA's   :771                       
    ##  NEIGHBORHOOD        CENSUSTRACT         ADDRESS         
    ##  Length:119931      Min.   :       9   Length:119931     
    ##  Class :character   1st Qu.:    5114   Class :character  
    ##  Mode  :character   Median :    5263   Mode  :character  
    ##                     Mean   :   37983                     
    ##                     3rd Qu.:    5393                     
    ##                     Max.   :99992016                     
    ##                     NA's   :13045                        
    ##       LON                LAT             LOCATION        
    ##  Min.   :  -121.0   Min.   :     0.0   Length:119931     
    ##  1st Qu.:   -83.2   1st Qu.:    42.4   Class :character  
    ##  Median :   -83.1   Median :    42.4   Mode  :character  
    ##  Mean   :  2236.2   Mean   :  2361.4                     
    ##  3rd Qu.:   -83.0   3rd Qu.:    42.4                     
    ##  Max.   :999999.0   Max.   :999999.0                     
    ##  NA's   :59         NA's   :59

Given the summary of latitudes and longitudes, we set a range for them: -83.3 &lt; LON &lt; -82.8, 42.2 &lt; LAT &lt; 42.45

``` r
crimes <- crimes[crimes$LON > -83.3 & crimes$LON < -82.8 & crimes$LAT > 42.2 & crimes$LAT < 42.45,]
crimes <- crimes[!is.na(crimes$LAT),]
```

We are mostly interested in crime category, address and their geocoordinates. So we create a subset with those columns and their unique caseid.Next we are going to aggregate crimes with respect to their geolocations and categories.

``` r
crimes <- crimes[c('CASEID','CATEGORY', 'ADDRESS','LON','LAT')]
# Aggregate crime by geocoordinates
cntCrmAgg = aggregate(CASEID ~ LAT+LON, data=crimes, length)
names(cntCrmAgg) = c('lat','lng','freq')
orderCntCrmAgg = cntCrmAgg[order(cntCrmAgg$freq, decreasing = T),]
orderCntCrmAgg[1:10,]
```

    ##           lat      lng freq
    ## 75659 42.4236 -83.0432   26
    ## 75661 42.4238 -83.0432   25
    ## 75594 42.4236 -83.0434   24
    ## 75627 42.4237 -83.0433   22
    ## 12779 42.3715 -83.2266   21
    ## 75628 42.4238 -83.0433   21
    ## 75626 42.4236 -83.0433   20
    ## 12780 42.3716 -83.2266   19
    ## 67910 42.3393 -83.0671   19
    ## 12825 42.3715 -83.2265   18

``` r
# Plot the crime distribution on map
library(ggmap)
```

    ## Loading required package: ggplot2

``` r
Calls_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                          size = c(640, 640), zoom = 11)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=42.38,-83.1&zoom=11&size=640x640&scale=2&maptype=roadmap&sensor=false

``` r
ggmap(Calls_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCntCrmAgg, color = ("orange")) 
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Blight_data_preprocessing_files/figure-markdown_github/unnamed-chunk-4-1.png) Summarize the crimes by categories

``` r
unique(crimes$CATEGORY)
```

    ##  [1] "ASSAULT"                                    
    ##  [2] "LARCENY"                                    
    ##  [3] "STOLEN VEHICLE"                             
    ##  [4] "WEAPONS OFFENSES"                           
    ##  [5] "TRAFFIC VIOLATIONS-MOTORCYCLE VIOLATIONS"   
    ##  [6] "BURGLARY"                                   
    ##  [7] "AGGRAVATED ASSAULT"                         
    ##  [8] "DAMAGE TO PROPERTY"                         
    ##  [9] "TRAFFIC VIOLATIONS-DRIVING ON SUSPENDED"    
    ## [10] "ROBBERY"                                    
    ## [11] "FRAUD"                                      
    ## [12] "OBSTRUCTING JUDICIARY"                      
    ## [13] "DANGEROUS DRUGS"                            
    ## [14] "HOMICIDE"                                   
    ## [15] "SOLICITATION"                               
    ## [16] "OUIL DISPOSE OF VEHICLE TO AVOID FORFEITURE"
    ## [17] "FAMILY OFFENSE"                             
    ## [18] "ARSON"                                      
    ## [19] "ESCAPE"                                     
    ## [20] "OBSTRUCTING THE POLICE"                     
    ## [21] "RUNAWAY"                                    
    ## [22] "BRIBERY"                                    
    ## [23] "KIDNAPING"                                  
    ## [24] "EXTORTION"                                  
    ## [25] "OTHER BURGLARY"                             
    ## [26] "STOLEN PROPERTY"                            
    ## [27] "HEALTH-SAFETY"                              
    ## [28] "VAGRANCY (OTHER)"                           
    ## [29] "ENVIRONMENT"                                
    ## [30] "EMBEZZLEMENT"                               
    ## [31] "FORGERY"                                    
    ## [32] "CONSPIRACY BY COMPUTER"                     
    ## [33] "ANTITRUST"                                  
    ## [34] "PUBLIC PEACE"                               
    ## [35] "LIQUOR"                                     
    ## [36] "OUIL"                                       
    ## [37] "OBSCENITY"                                  
    ## [38] "SOVEREIGNTY"                                
    ## [39] "TAX REVENUE"                                
    ## [40] "NEGLIGENT HOMICIDE"                         
    ## [41] "GAMBLING"                                   
    ## [42] "IMMIGRATION"                                
    ## [43] "CONGRESS"                                   
    ## [44] "JUSTIFIABLE HOMICIDE"                       
    ## [45] "REVOKED"                                    
    ## [46] "ELECTION LAWS"                              
    ## [47] "DRUNKENNESS"                                
    ## [48] "FELONY DEATH FROM FLEEING VEHICLE"          
    ## [49] "MISCELLANEOUS ARREST"                       
    ## [50] "MILITARY"

``` r
cntCtgCrmAgg = aggregate(CASEID ~ CATEGORY, data=crimes, length)
names(cntCtgCrmAgg) = c('Category','count')
orderCntCtgCrmAgg = cntCtgCrmAgg[order(cntCtgCrmAgg$count, decreasing = T),]
orderCntCtgCrmAgg[1:30,]
```

    ##                                       Category count
    ## 48    TRAFFIC VIOLATIONS-MOTORCYCLE VIOLATIONS 29275
    ## 4                                      ASSAULT 16423
    ## 27                                     LARCENY 14104
    ## 9                           DAMAGE TO PROPERTY  9398
    ## 1                           AGGRAVATED ASSAULT  8256
    ## 6                                     BURGLARY  7749
    ## 45                              STOLEN VEHICLE  6941
    ## 47     TRAFFIC VIOLATIONS-DRIVING ON SUSPENDED  5755
    ## 20                                       FRAUD  4892
    ## 40                                     ROBBERY  3177
    ## 10                             DANGEROUS DRUGS  3099
    ## 33                       OBSTRUCTING JUDICIARY  1400
    ## 50                            WEAPONS OFFENSES  1252
    ## 15                                      ESCAPE   906
    ## 42                                SOLICITATION   849
    ## 5                                      BRIBERY   832
    ## 3                                        ARSON   785
    ## 37 OUIL DISPOSE OF VEHICLE TO AVOID FORFEITURE   748
    ## 41                                     RUNAWAY   637
    ## 44                             STOLEN PROPERTY   349
    ## 34                      OBSTRUCTING THE POLICE   346
    ## 17                              FAMILY OFFENSE   334
    ## 23                                    HOMICIDE   252
    ## 35                              OTHER BURGLARY   232
    ## 22                               HEALTH-SAFETY   216
    ## 26                                   KIDNAPING   191
    ## 19                                     FORGERY   171
    ## 16                                   EXTORTION   157
    ## 38                                PUBLIC PEACE   145
    ## 14                                 ENVIRONMENT   119

Replace all the low frequency crimes by 'OtherCrimes'

``` r
low_freq_crime <- orderCntCtgCrmAgg[orderCntCtgCrmAgg$count<500, 1]
low_freq_crime <- as.character(low_freq_crime)
crimes$CATEGORY <- ifelse(crimes$CATEGORY %in% low_freq_crime, 'OtherCrimes', crimes$CATEGORY)
unique(crimes$CATEGORY)
```

    ##  [1] "ASSAULT"                                    
    ##  [2] "LARCENY"                                    
    ##  [3] "STOLEN VEHICLE"                             
    ##  [4] "WEAPONS OFFENSES"                           
    ##  [5] "TRAFFIC VIOLATIONS-MOTORCYCLE VIOLATIONS"   
    ##  [6] "BURGLARY"                                   
    ##  [7] "AGGRAVATED ASSAULT"                         
    ##  [8] "DAMAGE TO PROPERTY"                         
    ##  [9] "TRAFFIC VIOLATIONS-DRIVING ON SUSPENDED"    
    ## [10] "ROBBERY"                                    
    ## [11] "FRAUD"                                      
    ## [12] "OBSTRUCTING JUDICIARY"                      
    ## [13] "DANGEROUS DRUGS"                            
    ## [14] "OtherCrimes"                                
    ## [15] "SOLICITATION"                               
    ## [16] "OUIL DISPOSE OF VEHICLE TO AVOID FORFEITURE"
    ## [17] "ARSON"                                      
    ## [18] "ESCAPE"                                     
    ## [19] "RUNAWAY"                                    
    ## [20] "BRIBERY"

Change crime category to dummy variables

``` r
crimes_dummy <- dummy.data.frame(crimes, names=c('CATEGORY'),  sep='_')
# aggregate crimes by address
crimes_subset <-crimes_dummy[,2:22]
crimes_cnt_address <- aggregate(.~ADDRESS, data = crimes_subset, sum)
crimes_cnt_address[1:30,]
```

    ##                                     ADDRESS CATEGORY_AGGRAVATED ASSAULT
    ## 1                                         0                           2
    ## 2                                 00 3RD ST                           0
    ## 3                       00 A/O 5445 BALDWIN                           0
    ## 4                         00 A4156 EASTBURM                           0
    ## 5  00 ABANDONNED HOUSE ON FERNHILL E BAUMAN                           0
    ## 6                         00 ABBOTT AND 1ST                           0
    ## 7                00 ABBOTT/ N. JOHN C LODGE                           0
    ## 8       00 ABINGTON (1 HOUSE NORTH OF 9531)                           1
    ## 9                     00 ABINGTON / CHICAGO                           0
    ## 10              00 ABINGTON AND SCHOOLCRAFT                           0
    ## 11                  00 ABINGTON AND TIREMAN                           0
    ## 12               00 ABINGTON AND W. CHICAGO                           0
    ## 13                 00 ABINGTON/ GRAND RIVER                           0
    ## 14                      00 ABINGTON/DAVISON                           0
    ## 15                     00 ABINGTON/PLYMOUTH                           0
    ## 16                  00 ABINGTON/SCHOOLCRAFT                           1
    ## 17                     00 ABINGTON/W WARREN                           0
    ## 18                      00 ACACIA / PREVOST                           0
    ## 19                 00 ACACIA AND GREENFIELD                           0
    ## 20                  00 ACACIA AND MANSFIELD                           0
    ## 21                   00 ACACIA/ ASBURY PARK                           0
    ## 22                       00 ACACIA/MONTROSE                           0
    ## 23                     00 ACACIA/SOUTHFIELD                           0
    ## 24                    00 ACICA AND ROSEMONT                           0
    ## 25       00 ACROSS FROM ) 8664 PLAINVIEW ST                           0
    ## 26              00 ACROSS FROM 1237 ELSMERE                           0
    ## 27           00 ACROSS FROM 130612 APPOLINE                           0
    ## 28             00 ACROSS FROM 14073 MANNING                           0
    ## 29              00 ACROSS FROM 15702 HOLMUR                           0
    ## 30             00 ACROSS FROM 15900 ROSSINI                           0
    ##    CATEGORY_ARSON CATEGORY_ASSAULT CATEGORY_BRIBERY CATEGORY_BURGLARY
    ## 1               0                0                0                 0
    ## 2               0                0                0                 0
    ## 3               0                0                0                 0
    ## 4               0                1                0                 0
    ## 5               0                0                0                 0
    ## 6               0                0                0                 0
    ## 7               0                0                0                 0
    ## 8               0                0                0                 0
    ## 9               0                1                0                 0
    ## 10              0                0                0                 0
    ## 11              0                0                0                 0
    ## 12              0                0                0                 0
    ## 13              0                0                0                 0
    ## 14              0                0                0                 0
    ## 15              0                1                0                 0
    ## 16              0                0                0                 0
    ## 17              0                0                0                 0
    ## 18              0                0                0                 0
    ## 19              0                0                0                 0
    ## 20              0                0                0                 0
    ## 21              0                0                0                 0
    ## 22              0                0                0                 0
    ## 23              0                1                0                 0
    ## 24              0                1                0                 0
    ## 25              0                0                0                 0
    ## 26              1                0                0                 0
    ## 27              0                0                0                 0
    ## 28              1                0                0                 0
    ## 29              0                0                0                 0
    ## 30              0                0                0                 0
    ##    CATEGORY_DAMAGE TO PROPERTY CATEGORY_DANGEROUS DRUGS CATEGORY_ESCAPE
    ## 1                            2                        1               0
    ## 2                            0                        0               0
    ## 3                            0                        0               0
    ## 4                            0                        0               0
    ## 5                            0                        0               0
    ## 6                            1                        0               0
    ## 7                            0                        0               0
    ## 8                            0                        0               0
    ## 9                            0                        0               0
    ## 10                           0                        0               0
    ## 11                           0                        0               0
    ## 12                           0                        0               0
    ## 13                           0                        0               0
    ## 14                           0                        0               0
    ## 15                           0                        0               0
    ## 16                           0                        0               0
    ## 17                           0                        0               0
    ## 18                           0                        0               0
    ## 19                           0                        0               0
    ## 20                           0                        0               0
    ## 21                           0                        0               0
    ## 22                           0                        0               0
    ## 23                           0                        0               0
    ## 24                           0                        0               0
    ## 25                           1                        0               0
    ## 26                           0                        0               0
    ## 27                           0                        0               0
    ## 28                           0                        0               0
    ## 29                           0                        0               0
    ## 30                           0                        0               0
    ##    CATEGORY_FRAUD CATEGORY_LARCENY CATEGORY_OBSTRUCTING JUDICIARY
    ## 1               0                2                              0
    ## 2               0                0                              0
    ## 3               0                0                              0
    ## 4               0                0                              0
    ## 5               0                0                              0
    ## 6               0                0                              0
    ## 7               0                0                              0
    ## 8               0                0                              0
    ## 9               0                0                              0
    ## 10              0                0                              0
    ## 11              0                0                              0
    ## 12              0                0                              0
    ## 13              0                1                              0
    ## 14              0                1                              0
    ## 15              0                0                              0
    ## 16              0                0                              0
    ## 17              0                0                              0
    ## 18              0                0                              0
    ## 19              0                0                              0
    ## 20              0                0                              0
    ## 21              0                0                              0
    ## 22              0                0                              0
    ## 23              0                0                              0
    ## 24              0                0                              0
    ## 25              0                0                              0
    ## 26              0                0                              0
    ## 27              0                0                              0
    ## 28              0                0                              0
    ## 29              0                0                              0
    ## 30              0                0                              0
    ##    CATEGORY_OtherCrimes
    ## 1                     1
    ## 2                     0
    ## 3                     0
    ## 4                     0
    ## 5                     0
    ## 6                     0
    ## 7                     0
    ## 8                     0
    ## 9                     0
    ## 10                    0
    ## 11                    0
    ## 12                    0
    ## 13                    0
    ## 14                    0
    ## 15                    0
    ## 16                    0
    ## 17                    0
    ## 18                    0
    ## 19                    0
    ## 20                    0
    ## 21                    0
    ## 22                    0
    ## 23                    0
    ## 24                    0
    ## 25                    0
    ## 26                    0
    ## 27                    0
    ## 28                    0
    ## 29                    0
    ## 30                    0
    ##    CATEGORY_OUIL DISPOSE OF VEHICLE TO AVOID FORFEITURE CATEGORY_ROBBERY
    ## 1                                                     0                0
    ## 2                                                     0                0
    ## 3                                                     0                0
    ## 4                                                     0                0
    ## 5                                                     0                1
    ## 6                                                     0                0
    ## 7                                                     0                0
    ## 8                                                     0                0
    ## 9                                                     0                0
    ## 10                                                    0                0
    ## 11                                                    0                0
    ## 12                                                    0                0
    ## 13                                                    0                0
    ## 14                                                    0                0
    ## 15                                                    0                0
    ## 16                                                    0                0
    ## 17                                                    0                0
    ## 18                                                    0                1
    ## 19                                                    0                0
    ## 20                                                    0                1
    ## 21                                                    0                1
    ## 22                                                    0                0
    ## 23                                                    0                0
    ## 24                                                    0                0
    ## 25                                                    0                0
    ## 26                                                    0                0
    ## 27                                                    0                0
    ## 28                                                    0                0
    ## 29                                                    0                0
    ## 30                                                    0                0
    ##    CATEGORY_RUNAWAY CATEGORY_SOLICITATION CATEGORY_STOLEN VEHICLE
    ## 1                 0                     0                       0
    ## 2                 0                     0                       0
    ## 3                 0                     0                       0
    ## 4                 0                     0                       0
    ## 5                 0                     0                       0
    ## 6                 0                     0                       0
    ## 7                 0                     0                       0
    ## 8                 0                     0                       0
    ## 9                 0                     0                       0
    ## 10                0                     0                       0
    ## 11                0                     0                       0
    ## 12                0                     0                       0
    ## 13                0                     0                       0
    ## 14                0                     0                       0
    ## 15                0                     0                       0
    ## 16                0                     0                       0
    ## 17                0                     0                       0
    ## 18                0                     0                       0
    ## 19                0                     0                       0
    ## 20                0                     0                       0
    ## 21                0                     0                       0
    ## 22                0                     0                       0
    ## 23                0                     0                       0
    ## 24                0                     0                       0
    ## 25                0                     0                       0
    ## 26                0                     0                       0
    ## 27                0                     0                       0
    ## 28                0                     0                       0
    ## 29                0                     0                       0
    ## 30                0                     0                       0
    ##    CATEGORY_TRAFFIC VIOLATIONS-DRIVING ON SUSPENDED
    ## 1                                                 0
    ## 2                                                 0
    ## 3                                                 0
    ## 4                                                 0
    ## 5                                                 0
    ## 6                                                 0
    ## 7                                                 0
    ## 8                                                 0
    ## 9                                                 0
    ## 10                                                0
    ## 11                                                0
    ## 12                                                1
    ## 13                                                0
    ## 14                                                0
    ## 15                                                0
    ## 16                                                0
    ## 17                                                0
    ## 18                                                0
    ## 19                                                1
    ## 20                                                0
    ## 21                                                0
    ## 22                                                0
    ## 23                                                0
    ## 24                                                0
    ## 25                                                0
    ## 26                                                0
    ## 27                                                0
    ## 28                                                0
    ## 29                                                0
    ## 30                                                0
    ##    CATEGORY_TRAFFIC VIOLATIONS-MOTORCYCLE VIOLATIONS
    ## 1                                                  4
    ## 2                                                  1
    ## 3                                                  1
    ## 4                                                  0
    ## 5                                                  0
    ## 6                                                  0
    ## 7                                                  1
    ## 8                                                  0
    ## 9                                                  0
    ## 10                                                 0
    ## 11                                                 1
    ## 12                                                 0
    ## 13                                                 0
    ## 14                                                 0
    ## 15                                                 0
    ## 16                                                 1
    ## 17                                                 1
    ## 18                                                 0
    ## 19                                                 0
    ## 20                                                 0
    ## 21                                                 0
    ## 22                                                 1
    ## 23                                                 0
    ## 24                                                 0
    ## 25                                                 0
    ## 26                                                 0
    ## 27                                                 1
    ## 28                                                 0
    ## 29                                                 1
    ## 30                                                 1
    ##    CATEGORY_WEAPONS OFFENSES
    ## 1                          0
    ## 2                          0
    ## 3                          0
    ## 4                          0
    ## 5                          0
    ## 6                          0
    ## 7                          0
    ## 8                          0
    ## 9                          0
    ## 10                         1
    ## 11                         0
    ## 12                         0
    ## 13                         0
    ## 14                         0
    ## 15                         0
    ## 16                         0
    ## 17                         0
    ## 18                         0
    ## 19                         0
    ## 20                         0
    ## 21                         0
    ## 22                         0
    ## 23                         0
    ## 24                         0
    ## 25                         0
    ## 26                         0
    ## 27                         0
    ## 28                         0
    ## 29                         0
    ## 30                         0

There are 56935 unique addresses in crimes dataset. From table cntCrmAgg/ordercntCrmAgg, we know there are 103611 unique geocoordinate pairs. So one address could have multiple geocoordinates. Next we are going to create a unique building list and filter out the ones that have STDEV(LAT)&gt;0.0001 or STDEV(LON)&gt;0.0001. The precision of 0.0001 refers to the quantitative scale of individual land/parcel(<https://en.wikipedia.org/wiki/Decimal_degrees>).

``` r
# Use package sqldf 
crm_bld_list <- sqldf('SELECT ADDRESS, avg(LAT) as Lat, avg(LON) as Lon, count(CASEID) as N_crimes 
                      FROM crimes 
                      GROUP BY ADDRESS
                      HAVING stdev(LAT) <= 0.0001 AND stdev(LON) <= 0.0001')
```

    ## Loading required package: tcltk

    ## Warning: Quoted identifiers should have class SQL, use DBI::SQL() if the
    ## caller performs the quoting.

``` r
head(crm_bld_list)
```

    ##                                    ADDRESS     Lat      Lon N_crimes
    ## 1                                00 3RD ST 42.3323 -83.0579        1
    ## 2                      00 A/O 5445 BALDWIN 42.3789 -83.0174        1
    ## 3                        00 A4156 EASTBURM 42.4455 -82.9463        1
    ## 4 00 ABANDONNED HOUSE ON FERNHILL E BAUMAN 42.4378 -83.1124        1
    ## 5                        00 ABBOTT AND 1ST 42.3310 -83.0539        1
    ## 6               00 ABBOTT/ N. JOHN C LODGE 42.3294 -83.0583        1

Then we will inner join the building list with the crime\_address aggregation table.

``` r
crm_bld_list <- plyr::join(crm_bld_list,crimes_cnt_address,type='inner')
```

    ## Joining by: ADDRESS

``` r
write.csv(crm_bld_list, 'crimes_building_list.csv')
```

Explore violations dataset
==========================

``` r
summary(violations)
```

    ##     TicketID      TicketNumber        AgencyName          ViolName        
    ##  Min.   : 18645   Length:307804      Length:307804      Length:307804     
    ##  1st Qu.:101806   Class :character   Class :character   Class :character  
    ##  Median :183824   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :182967                                                           
    ##  3rd Qu.:265211                                                           
    ##  Max.   :339184                                                           
    ##                                                                           
    ##  ViolationStreetNumber ViolationStreetName MailingStreetNumber
    ##  Min.   :   -11064     Length:307804       Length:307804      
    ##  1st Qu.:     4936     Class :character    Class :character   
    ##  Median :    10624     Mode  :character    Mode  :character   
    ##  Mean   :    12001                                            
    ##  3rd Qu.:    15895                                            
    ##  Max.   :222222222                                            
    ##                                                               
    ##  MailingStreetName  MailingCity        MailingState      
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  MailingZipCode     NonUsAddressCode     Country         
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  TicketIssuedDT     TicketIssuedTime    HearingDT        
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##   CourtTime         ViolationCode      ViolDescription   
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  Disposition          FineAmt            AdminFee        
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##    LateFee            StateFee         CleanUpCost       
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  JudgmentAmt        PaymentStatus           Void        ViolationCategory 
    ##  Length:307804      Length:307804      Min.   :0        Min.   :0.000000  
    ##  Class :character   Class :character   1st Qu.:0        1st Qu.:0.000000  
    ##  Mode  :character   Mode  :character   Median :0        Median :0.000000  
    ##                                        Mean   :0        Mean   :0.006553  
    ##                                        3rd Qu.:0        3rd Qu.:0.000000  
    ##                                        Max.   :0        Max.   :1.000000  
    ##                                        NA's   :208671                     
    ##  ViolationAddress   MailingAddress    
    ##  Length:307804      Length:307804     
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
    ##                                       
    ##                                       
    ##                                       
    ## 

Parse latitude and longitude for violation dataset

``` r
violations <- as.data.frame(violations)
split_violations_address <- colsplit(violations$ViolationAddress, split='\n', names=c('street', 'city_state', 'viol_lat_lng'))
split_violations_address$viol_lat_lng <- gsub("[()]", "", split_violations_address$viol_lat_lng) # Remove parenthesis 
split_violations_lat_lng <- colsplit(split_violations_address$viol_lat_lng, split=',', names=c('lat', 'lng'))
violations$LAT <- split_violations_lat_lng$lat
violations$LON <- split_violations_lat_lng$lng
violations$ViolationAddress <- split_violations_address$street
split_violations_address <- NULL
split_violations_lat_lng <- NULL
```

Define a function to convert currency to number

``` r
cur_to_num <- function(x) as.numeric(gsub('[$,]','', as.character(x)), na.rm=FALSE) # '\\$|,'='[$,]'
cur_cols <- c('FineAmt', 'AdminFee', 'LateFee', 'StateFee', 'CleanUpCost', 'JudgmentAmt')
violations[cur_cols] <- lapply(violations[cur_cols], cur_to_num)
violations$TotalFee <- violations$FineAmt + violations$AdminFee + violations$StateFee + violations$LateFee + violations$CleanUpCost + violations$JudgmentAmt
```

Convert char to factors

``` r
fac_cols <- c("ViolationCode","Disposition","PaymentStatus",
                   "Void","ViolationCategory")
violations[fac_cols] <- lapply(violations[fac_cols], as.factor)
summary(violations)
```

    ##     TicketID      TicketNumber        AgencyName          ViolName        
    ##  Min.   : 18645   Length:307804      Length:307804      Length:307804     
    ##  1st Qu.:101806   Class :character   Class :character   Class :character  
    ##  Median :183824   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :182967                                                           
    ##  3rd Qu.:265211                                                           
    ##  Max.   :339184                                                           
    ##                                                                           
    ##  ViolationStreetNumber ViolationStreetName MailingStreetNumber
    ##  Min.   :   -11064     Length:307804       Length:307804      
    ##  1st Qu.:     4936     Class :character    Class :character   
    ##  Median :    10624     Mode  :character    Mode  :character   
    ##  Mean   :    12001                                            
    ##  3rd Qu.:    15895                                            
    ##  Max.   :222222222                                            
    ##                                                               
    ##  MailingStreetName  MailingCity        MailingState      
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  MailingZipCode     NonUsAddressCode     Country         
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  TicketIssuedDT     TicketIssuedTime    HearingDT        
    ##  Length:307804      Length:307804      Length:307804     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##   CourtTime            ViolationCode    ViolDescription   
    ##  Length:307804      9-1-36(a) :106521   Length:307804     
    ##  Class :character   9-1-81(a) : 45036   Class :character  
    ##  Mode  :character   9-1-104   : 38612   Mode  :character  
    ##                     22-2-88   : 28730                     
    ##                     22-2-88(b): 21804                     
    ##                     9-1-110(a):  6779                     
    ##                     (Other)   : 60322                     
    ##                             Disposition        FineAmt           AdminFee 
    ##  Responsible By Default           :172128   Min.   :    0.0   Min.   :20  
    ##  Not responsible By Dismissal     : 53543   1st Qu.:  125.0   1st Qu.:20  
    ##  Not responsible By City Dismissal: 44380   Median :  250.0   Median :20  
    ##  Responsible By Admission         : 16432   Mean   :  360.2   Mean   :20  
    ##  Responsible By Determination     : 10233   3rd Qu.:  250.0   3rd Qu.:20  
    ##  Not responsible By Determination :  7809   Max.   :10000.0   Max.   :20  
    ##  (Other)                          :  3279   NA's   :1973                  
    ##     LateFee          StateFee   CleanUpCost         JudgmentAmt     
    ##  Min.   :   0.0   Min.   :10   Min.   :    0.000   Min.   :    0.0  
    ##  1st Qu.:  10.0   1st Qu.:10   1st Qu.:    0.000   1st Qu.:  140.0  
    ##  Median :  25.0   Median :10   Median :    0.000   Median :  305.0  
    ##  Mean   :  35.8   Mean   :10   Mean   :    0.515   Mean   :  425.2  
    ##  3rd Qu.:  25.0   3rd Qu.:10   3rd Qu.:    0.000   3rd Qu.:  305.0  
    ##  Max.   :1000.0   Max.   :10   Max.   :13123.800   Max.   :11030.0  
    ##                                                    NA's   :1972     
    ##               PaymentStatus      Void        ViolationCategory
    ##  NO PAYMENT APPLIED  :244303   0   : 99133   0:305787         
    ##  NO PAYMENT ON RECORD: 14565   NA's:208671   1:  2017         
    ##  PAID IN FULL        : 44319                                  
    ##  PARTIAL PAYMENT MADE:  4617                                  
    ##                                                               
    ##                                                               
    ##                                                               
    ##         ViolationAddress  MailingAddress          LAT       
    ##  1509 BROADWAY  :   203   Length:307804      Min.   :42.26  
    ##  600 WOODWARD   :   130   Class :character   1st Qu.:42.35  
    ##  9125 JEFFERSON :    94   Mode  :character   Median :42.39  
    ##  16189 SCHAEFER :    91                      Mean   :42.38  
    ##  69 SEWARD      :    91                      3rd Qu.:42.42  
    ##  20125 LIVERNOIS:    73                      Max.   :42.45  
    ##  (Other)        :307122                                     
    ##       LON            TotalFee      
    ##  Min.   :-83.29   Min.   :   55.0  
    ##  1st Qu.:-83.19   1st Qu.:  280.0  
    ##  Median :-83.12   Median :  610.0  
    ##  Mean   :-83.11   Mean   :  851.9  
    ##  3rd Qu.:-83.05   3rd Qu.:  610.0  
    ##  Max.   :-82.91   Max.   :22060.0  
    ##                   NA's   :1973

Create a subset with the features we are interested in

``` r
violations <- violations[c('TicketID', 'ViolationCode', 'TotalFee','ViolationAddress','LAT','LON')]

# Blight Violation frequency
# By geo coordinates
cntCorAgg = aggregate(TicketID ~ LAT + LON, data=violations, length)
names(cntCorAgg) = c('lat','lng','freq')
orderCntCorAgg = cntCorAgg[order(cntCorAgg$freq, decreasing = T),]
orderCntCorAgg[1:10,]
```

    ##            lat       lng  freq
    ## 68921 42.33168 -83.04800 21114
    ## 15197 42.33438 -83.21604  4467
    ## 49675 42.41591 -83.12131  2003
    ## 15032 42.34528 -83.21652  1713
    ## 75028 42.34563 -83.00881  1372
    ## 60914 42.35323 -83.08587  1142
    ## 74901 42.38949 -83.00924   755
    ## 52331 42.37507 -83.11455   742
    ## 11923 42.32893 -83.22547   639
    ## 68533 42.33250 -83.04975   625

``` r
# By street address
cntAddAgg = aggregate(TicketID ~ ViolationAddress, data=violations, length)
names(cntAddAgg) = c('address','freq')
orderCntAddAgg = cntAddAgg[order(cntAddAgg$freq, decreasing = T),]
orderCntAddAgg[1:10,]
```

    ##                address freq
    ## 26356    1509 BROADWAY  203
    ## 88525     600 WOODWARD  130
    ## 106195  9125 JEFFERSON   94
    ## 33878   16189 SCHAEFER   91
    ## 94048        69 SEWARD   91
    ## 58737  20125 LIVERNOIS   73
    ## 1               0 10TH   70
    ## 94499    7057 MICHIGAN   59
    ## 2285       10701 SANTA   58
    ## 87802        59 SEWARD   57

Map Ploting

``` r
library(ggmap)
violations_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                               size = c(640, 640), zoom = 11)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=42.38,-83.1&zoom=11&size=640x640&scale=2&maptype=roadmap&sensor=false

``` r
ggmap(violations_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCntCorAgg, color = ("red")) 
```

![](Blight_data_preprocessing_files/figure-markdown_github/unnamed-chunk-15-1.png) Violation codes are quite messy. We will clean and format the codes.

``` r
violations$ViolationCode <- gsub('[/]', '-', violations$ViolationCode)
#remove all characters in parentheses
violations$ViolationCode <- gsub('\\(.*','',violations$ViolationCode)  
violations$ViolationCode <- gsub('\\..*','',violations$ViolationCode)  
# remove space
violations$ViolationCode <- gsub('\\ .*','',violations$ViolationCode)  
# check unique codes
unique(violations$ViolationCode)
```

    ##   [1] "22-2-20"   "22-2-22"   "22-2-45"   "22-2-88"   "22-2-17"  
    ##   [6] "22-2-83"   "22-2-49"   "22-2-56"   "22-2-23"   "22-2-21"  
    ##  [11] "22-2-43"   "22-2-83a"  "22-2-61"   "22-2-41"   "22-2-44"  
    ##  [16] "22-2-16"   "22-2-84"   "22-2-87"   "9-1-36"    "9-1-355"  
    ##  [21] "9-1-45"    "9-1-209"   "9-1-104"   "9-1-353"   "61-63"    
    ##  [26] "61-130"    "61-90"     "61-47"     "9-1-103"   "22-2-38"  
    ##  [31] "22-2-91"   "22-2-55"   "9-1-102"   "61-116"    "9-1-105"  
    ##  [36] "61-102"    "61-101"    "9-1-106"   "9-1-108"   "9-1-110"  
    ##  [41] "9-1-101"   "61-81"     "61-83"     "61-82"     "61-80"    
    ##  [46] "61-114"    "9-1-13"    "9-1-206"   "22-2-94"   "22-2-86"  
    ##  [51] "22-2-85"   "9-1-43"    "9-1-201"   "9-1-205"   "9-1-107"  
    ##  [56] "9-1-214"   "9-1-204"   "9-1-477"   "9-1-211"   "9-1-220"  
    ##  [61] "9-1-210"   "9-1-81"    "9-1-221"   "9-1-111"   "9-1-208"  
    ##  [66] "9-1-310"   "9-1-476"   "9-1-303"   "9-1-503"   "9-1-304"  
    ##  [71] "61-111"    "22-2-97"   "61-121"    "61-120"    "61-105"   
    ##  [76] "9-1-331"   "9-1-301"   "9-1-83"    "9-1-202"   "9-1-219"  
    ##  [81] "9-1-351"   "61-84"     "22-2-42"   "9-1-216"   "9-1-212"  
    ##  [86] "9-1-207"   "9-1-306"   "9-1-213"   "9-1-432"   "9-1-462"  
    ##  [91] "9-1-474"   "9-1-354"   "9-1-311"   "9-1-501"   "61-4-33"  
    ##  [96] "9-1-440"   "9-1-441"   "9-1-464"   "9-1-442"   "9-1-406"  
    ## [101] "9-1-444"   "9-1-309"   "9-1-469"   "9-1-443"   "9-1-305"  
    ## [106] "9-1-479"   "9-1-439"   "61-4-32"   "9-1-46"    "9-1-433"  
    ## [111] "9-1-478"   "9-1-12"    "9-1-375"   "9-1-307"   "9-1-465"  
    ## [116] "22-2-53"   "22-2-18"   "22-2-96"   "9-1-468"   "9-1-405"  
    ## [121] "9-1-18"    "61-5-21"   "9-1-85"    "9-1-471"   "61-8-127" 
    ## [126] "61-8-27"   "9-1-84"    "9-1-377"   "22-3-2"    "22-2-93"  
    ## [131] "9-1-109"   "9-1-333"   "9-1-112"   "61-4-38"   "61-5-19"  
    ## [136] "61-8-47"   "9-1-352"   "61-45"     "9-1-203"   "61-5-18"  
    ## [141] "61-118"    "61-122"    "9-1-302"   "61-86"     "61-85"    
    ## [146] "9-1-42"    "61-14-176" "22-2-25"   "61-104"    "9-1-215"  
    ## [151] "61-5-20"   "9-1-381"   "61-14-175" "61-115"    "61-4-35"  
    ## [156] "61-4-37"   "61-100"    "9-1-502"   "61-96"     "22-2-19"  
    ## [161] "22-2-92"   "9-1-332"   "61-14-452" "9-1-82"    "9-1-466"  
    ## [166] "9-1-434"   "9-1-308"   "9-1-50"    "9-1-113"   "9-1-16"   
    ## [171] "9-1-41"    "22-3-3"    "22-2-48"   "9-1-411"   "9-1-378"  
    ## [176] "9-1-312"   "9-1-91"    "61-91"     "22-2-24"   "9-1-412"  
    ## [181] "61-5-14"   "61-112"    "9-1-218"   "61-103"    "61-12-437"

Aggregate violations by codes

``` r
cntCdAgg = aggregate(TicketID ~ ViolationCode, data=violations, length)
names(cntCdAgg) = c('ViolationCode','freq')
orderCntCdAgg = cntCdAgg[order(cntCdAgg$freq, decreasing = T),]
orderCntCdAgg[1:30,]
```

    ##     ViolationCode   freq
    ## 141        9-1-36 106588
    ## 29        22-2-88  52025
    ## 180        9-1-81  45066
    ## 87        9-1-104  38612
    ## 152        9-1-43   8175
    ## 93        9-1-110   6812
    ## 16        22-2-45   6585
    ## 86        9-1-103   5427
    ## 88        9-1-105   5277
    ## 7         22-2-22   4072
    ## 14        22-2-43   3645
    ## 22        22-2-61   3049
    ## 162        9-1-45   3005
    ## 181        9-1-82   2241
    ## 23        22-2-83   2224
    ## 2         22-2-17   2208
    ## 96        9-1-113   1824
    ## 94        9-1-111   1567
    ## 176        9-1-50    913
    ## 75          61-81    848
    ## 69        61-5-21    486
    ## 84        9-1-101    438
    ## 66        61-5-18    401
    ## 106       9-1-206    363
    ## 182        9-1-83    359
    ## 6         22-2-21    357
    ## 101       9-1-201    349
    ## 109       9-1-209    346
    ## 90        9-1-107    256
    ## 70          61-63    250

We plan to use some common codes as features in the predictive model later. So we are going to replace all the low frequency violation codes by 'OtherViolationCode' with a threshold freq of 500

``` r
low_freq_viol_code <- orderCntCdAgg[orderCntCdAgg$freq<500, ]$ViolationCode
violations$ViolationCode <- ifelse(violations$ViolationCode %in% low_freq_viol_code, 'OtherViolationCode', violations$ViolationCode)
unique(violations$ViolationCode)
```

    ##  [1] "OtherViolationCode" "22-2-22"            "22-2-45"           
    ##  [4] "22-2-88"            "22-2-17"            "22-2-83"           
    ##  [7] "22-2-43"            "22-2-61"            "9-1-36"            
    ## [10] "9-1-45"             "9-1-104"            "9-1-103"           
    ## [13] "9-1-105"            "9-1-110"            "61-81"             
    ## [16] "9-1-43"             "9-1-81"             "9-1-111"           
    ## [19] "9-1-82"             "9-1-50"             "9-1-113"

Change violationcode to dummy variables

``` r
violations_dummy <- dummy.data.frame(violations, names=c('ViolationCode'),  sep='_')
head(violations_dummy)
```

    ##   TicketID ViolationCode_22-2-17 ViolationCode_22-2-22
    ## 1    26288                     0                     0
    ## 2    19800                     0                     1
    ## 3    19804                     0                     1
    ## 4    20208                     0                     0
    ## 5    20211                     0                     1
    ## 6    20628                     0                     0
    ##   ViolationCode_22-2-43 ViolationCode_22-2-45 ViolationCode_22-2-61
    ## 1                     0                     0                     0
    ## 2                     0                     0                     0
    ## 3                     0                     0                     0
    ## 4                     0                     1                     0
    ## 5                     0                     0                     0
    ## 6                     0                     1                     0
    ##   ViolationCode_22-2-83 ViolationCode_22-2-88 ViolationCode_61-81
    ## 1                     0                     0                   0
    ## 2                     0                     0                   0
    ## 3                     0                     0                   0
    ## 4                     0                     0                   0
    ## 5                     0                     0                   0
    ## 6                     0                     0                   0
    ##   ViolationCode_9-1-103 ViolationCode_9-1-104 ViolationCode_9-1-105
    ## 1                     0                     0                     0
    ## 2                     0                     0                     0
    ## 3                     0                     0                     0
    ## 4                     0                     0                     0
    ## 5                     0                     0                     0
    ## 6                     0                     0                     0
    ##   ViolationCode_9-1-110 ViolationCode_9-1-111 ViolationCode_9-1-113
    ## 1                     0                     0                     0
    ## 2                     0                     0                     0
    ## 3                     0                     0                     0
    ## 4                     0                     0                     0
    ## 5                     0                     0                     0
    ## 6                     0                     0                     0
    ##   ViolationCode_9-1-36 ViolationCode_9-1-43 ViolationCode_9-1-45
    ## 1                    0                    0                    0
    ## 2                    0                    0                    0
    ## 3                    0                    0                    0
    ## 4                    0                    0                    0
    ## 5                    0                    0                    0
    ## 6                    0                    0                    0
    ##   ViolationCode_9-1-50 ViolationCode_9-1-81 ViolationCode_9-1-82
    ## 1                    0                    0                    0
    ## 2                    0                    0                    0
    ## 3                    0                    0                    0
    ## 4                    0                    0                    0
    ## 5                    0                    0                    0
    ## 6                    0                    0                    0
    ##   ViolationCode_OtherViolationCode TotalFee ViolationAddress      LAT
    ## 1                                1     3360  2566 GRAND BLVD 42.36318
    ## 2                                0      280     19014 ASHTON 42.42939
    ## 3                                0      280   18735 STAHELIN 42.42871
    ## 4                                0      280     20125 MONICA 42.44170
    ## 5                                0      280    17397 PRAIRIE 42.42032
    ## 6                                0      280  17153 NORTHLAWN 42.41789
    ##         LON
    ## 1 -83.09168
    ## 2 -83.22039
    ## 3 -83.22755
    ## 4 -83.14502
    ## 5 -83.14533
    ## 6 -83.15364

Aggregate violations by address

``` r
violations_subset <- violations_dummy[, c(2:22,24)]
violation_code_cnt <- aggregate(x = violations_subset[,1:21], by=list(violations_subset$ViolationAddress), FUN=sum)
colnames(violation_code_cnt)[1] <- 'ViolationAddress'
```

Create violation building list

``` r
viol_bld_list <- sqldf('SELECT ViolationAddress, avg(LAT) as Lat, avg(LON) AS Lon, 
                       count(TicketID) as N_Violations, max(TotalFee) as MaxTotalFee
                       FROM violations 
                       GROUP BY ViolationAddress
                       HAVING stdev(LAT) <= 0.0001 AND stdev(LON) <= 0.0001')
```

Inner join those two tables

``` r
viol_bld_list <- join(viol_bld_list, violation_code_cnt, type='inner')
```

    ## Joining by: ViolationAddress

``` r
viol_bld_list[is.na(viol_bld_list)] <- 0
write.csv(viol_bld_list, 'violations_building_list.csv')
```

Explore 311 calls dataset
=========================

``` r
summary(calls)
```

    ##    ticket_id           city            issue_type       
    ##  Min.   :1184398   Length:19680       Length:19680      
    ##  1st Qu.:1591936   Class :character   Class :character  
    ##  Median :1705228   Mode  :character   Mode  :character  
    ##  Mean   :1699224                                        
    ##  3rd Qu.:1838305                                        
    ##  Max.   :1975499                                        
    ##  ticket_status      issue_description      rating      
    ##  Length:19680       Length:19680       Min.   : 1.000  
    ##  Class :character   Class :character   1st Qu.: 2.000  
    ##  Mode  :character   Mode  :character   Median : 3.000  
    ##                                        Mean   : 2.693  
    ##                                        3rd Qu.: 3.000  
    ##                                        Max.   :19.000  
    ##  ticket_closed_date_time acknowledged_at    ticket_created_date_time
    ##  Length:19680            Length:19680       Length:19680            
    ##  Class :character        Class :character   Class :character        
    ##  Mode  :character        Mode  :character   Mode  :character        
    ##                                                                     
    ##                                                                     
    ##                                                                     
    ##  ticket_last_updated_date_time   address               lat       
    ##  Length:19680                  Length:19680       Min.   :41.88  
    ##  Class :character              Class :character   1st Qu.:42.36  
    ##  Mode  :character              Mode  :character   Median :42.39  
    ##                                                   Mean   :42.39  
    ##                                                   3rd Qu.:42.42  
    ##                                                   Max.   :42.45  
    ##       lng           location            image          
    ##  Min.   :-86.55   Length:19680       Length:19680      
    ##  1st Qu.:-83.19   Class :character   Class :character  
    ##  Median :-83.11   Mode  :character   Mode  :character  
    ##  Mean   :-83.11                                        
    ##  3rd Qu.:-83.04                                        
    ##  Max.   :-82.91

Filter outliers and remove NAs

``` r
calls <- calls[calls$lng > -83.3 & calls$lng < -82.8 & calls$lat > 42.2 & calls$lat < 42.5,]
sum(is.na(calls$address))
```

    ## [1] 0

``` r
calls <- calls[!is.na(calls$address),]
```

Parse the street address

``` r
calls$address <- sapply(strsplit(calls$address,"\\ Detroit"), '[', 1)
```

    ## Warning in strsplit(calls$address, "\\ Detroit"): input string 4713 is
    ## invalid in this locale

    ## Warning in strsplit(calls$address, "\\ Detroit"): input string 9474 is
    ## invalid in this locale

    ## Warning in strsplit(calls$address, "\\ Detroit"): input string 11019 is
    ## invalid in this locale

    ## Warning in strsplit(calls$address, "\\ Detroit"): input string 18276 is
    ## invalid in this locale

    ## Warning in strsplit(calls$address, "\\ Detroit"): input string 18888 is
    ## invalid in this locale

Create a subset with useful features for modeling

``` r
calls <- calls[c('ticket_id','issue_type','rating','address','lat','lng')]
```

311 call geo distribution

``` r
cnt311Agg = aggregate(ticket_id ~ lat+lng, data=calls, length)
names(cnt311Agg) = c('lat','lng','freq')
orderCnt311Agg = cnt311Agg[order(cnt311Agg$freq, decreasing = T),]
orderCnt311Agg[1:10,]
```

    ##            lat       lng freq
    ## 12560 42.35786 -83.04424   90
    ## 12450 42.33143 -83.04575   78
    ## 9149  42.38338 -83.10248   65
    ## 1156  42.34385 -83.25467   38
    ## 571   42.34416 -83.26266    9
    ## 327   42.36428 -83.26552    8
    ## 664   42.34387 -83.26194    8
    ## 2824  42.34456 -83.21984    8
    ## 7140  42.36398 -83.13720    7
    ## 8233  42.32886 -83.12067    7

Plot 311 calls distribution on map

``` r
library(ggmap)
calls_map = get_googlemap(center=c(lon=-83.1088, lat=42.3843), maptype = "roadmap", 
                          size = c(640, 640), zoom = 11)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=42.3843,-83.1088&zoom=11&size=640x640&scale=2&maptype=roadmap&sensor=false

``` r
ggmap(calls_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCnt311Agg[1:1000,], color = ("orange")) 
```

![](Blight_data_preprocessing_files/figure-markdown_github/unnamed-chunk-28-1.png)

Aggregate calls by issue type

``` r
# issue
cntIss311Agg = aggregate(ticket_id ~ issue_type, data=calls, length)
names(cntIss311Agg) = c('issue_type','count')
orderIss311Agg = cntIss311Agg[order(cntIss311Agg$count, decreasing = T),]
orderIss311Agg[1:30,]
```

    ##                                                                                     issue_type
    ## 11                                                        Illegal Dumping / Illegal Dump Sites
    ## 22                                                                                  Tree Issue
    ## 15                                                         Running Water in a Home or Building
    ## 2                                                                                Clogged Drain
    ## 13                                                                                    Potholes
    ## 18                                                                          Traffic Sign Issue
    ## 23                                                                            Water Main Break
    ## 8                                                                           Fire Hydrant Issue
    ## 1                                                                            Abandoned Vehicle
    ## 12                                                                         Manhole Cover Issue
    ## 19                                                                        Traffic Signal Issue
    ## 20                Trash Issue - Bulk waste deposited more than 24 hours before designated time
    ## 6                                                                         DPW - Debris Removal
    ## 16                                                                      Street Light Pole Down
    ## 3                                                                   Curbside Solid Waste Issue
    ## 7                                                                    DPW - Other environmental
    ## 21   Trash Issue - Improper placement of refuse container between collections/left at curbside
    ## 14                                                              Residential Snow Removal Issue
    ## 9                                                                                     Graffiti
    ## 5                                                                   Detroit Land Bank Referral
    ## 17                                                      Test (internal use only, public issue)
    ## 10                                        Graffiti Abatement (internal use only, public issue)
    ## 4                                          Customer Service (internal use only, private issue)
    ## NA                                                                                        <NA>
    ## NA.1                                                                                      <NA>
    ## NA.2                                                                                      <NA>
    ## NA.3                                                                                      <NA>
    ## NA.4                                                                                      <NA>
    ## NA.5                                                                                      <NA>
    ## NA.6                                                                                      <NA>
    ##      count
    ## 11    3584
    ## 22    3546
    ## 15    2654
    ## 2     2490
    ## 13    2399
    ## 18    1030
    ## 23     778
    ## 8      678
    ## 1      638
    ## 12     546
    ## 19     390
    ## 20     245
    ## 6      189
    ## 16     182
    ## 3      159
    ## 7       64
    ## 21      41
    ## 14      31
    ## 9       24
    ## 5        5
    ## 17       3
    ## 10       2
    ## 4        1
    ## NA      NA
    ## NA.1    NA
    ## NA.2    NA
    ## NA.3    NA
    ## NA.4    NA
    ## NA.5    NA
    ## NA.6    NA

Replace all the low frequency issues by 'OtherIssue'

``` r
low_freq_issue <- as.character(orderIss311Agg[orderIss311Agg$count<100, 1])
calls$issue_type <-ifelse(calls$issue_type %in% low_freq_issue, 'OtherIssue', calls$issue_type)
unique(calls$issue_type)
```

    ##  [1] "Clogged Drain"                                                               
    ##  [2] "Tree Issue"                                                                  
    ##  [3] "Manhole Cover Issue"                                                         
    ##  [4] "Water Main Break"                                                            
    ##  [5] "Abandoned Vehicle"                                                           
    ##  [6] "Fire Hydrant Issue"                                                          
    ##  [7] "Traffic Signal Issue"                                                        
    ##  [8] "Potholes"                                                                    
    ##  [9] "Traffic Sign Issue"                                                          
    ## [10] "Illegal Dumping / Illegal Dump Sites"                                        
    ## [11] "Running Water in a Home or Building"                                         
    ## [12] "Street Light Pole Down"                                                      
    ## [13] "OtherIssue"                                                                  
    ## [14] "DPW - Debris Removal"                                                        
    ## [15] "Trash Issue - Bulk waste deposited more than 24 hours before designated time"
    ## [16] "Curbside Solid Waste Issue"

Transform issue types into dummy variables

``` r
calls_dummy <- dummy.data.frame(calls, names=c('issue_type'),  sep='_')
```

Aggregate calls by address

``` r
calls_subset <-calls_dummy[,c(2:17,19)]
calls_cnt_address <- aggregate(.~address, data = calls_subset, sum)
```

Create 311 building list

``` r
calls_bld_list <- sqldf('SELECT address, avg(lat) as Lat, avg(lng) AS Lon, count(ticket_id) as N_calls, max(rating) as Max_Rating
                        FROM calls
                        GROUP BY address
                        HAVING stdev(lat) <= 0.0001 AND stdev(lng) <= 0.0001')
```

Inner join the building list with 311 statistics

``` r
calls_bld_list <- plyr::join(calls_bld_list,calls_cnt_address,type='inner')
```

    ## Joining by: address

``` r
write.csv(calls_bld_list, 'calls_building_list.csv')
```

Explore permits dataset
=======================

``` r
permits <- as.data.frame(permits)
summary(permits)
```

    ##   PERMIT_NO         PERMIT_APPLIED     PERMIT_ISSUED     
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  PERMIT_EXPIRES     SITE_ADDRESS         BETWEEN1        
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##   PARCEL_NO          LOT_NUMBER        SUBDIVISION       
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##   CASE_TYPE         CASE_DESCRIPTION    LEGAL_USE        
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  ESTIMATED_COST      PARCEL_SIZE      PARCEL_CLUSTER_SECTOR
    ##  Length:7133        Min.   :      0   Min.   : 1.000       
    ##  Class :character   1st Qu.:   3354   1st Qu.: 2.000       
    ##  Mode  :character   Median :   4095   Median : 5.000       
    ##                     Mean   :  13540   Mean   : 4.656       
    ##                     3rd Qu.:   4932   3rd Qu.: 7.000       
    ##                     Max.   :2554576   Max.   :10.000       
    ##                     NA's   :45        NA's   :11           
    ##     STORIES       PARCEL_FLOOR_AREA PARCEL_GROUND_AREA PRC_AKA_ADDRESS   
    ##  Min.   : 1.000   Min.   :      0   Min.   :    0.0    Length:7133       
    ##  1st Qu.: 1.000   1st Qu.:      0   1st Qu.:  630.0    Class :character  
    ##  Median : 1.500   Median :      0   Median :  768.0    Mode  :character  
    ##  Mean   : 2.126   Mean   :   6830   Mean   :  742.3                      
    ##  3rd Qu.: 2.000   3rd Qu.:      0   3rd Qu.:  912.0                      
    ##  Max.   :26.000   Max.   :5102782   Max.   :27316.0                      
    ##  NA's   :1426     NA's   :45        NA's   :45                           
    ##  BLD_PERMIT_TYPE    PERMIT_DESCRIPTION BLD_PERMIT_DESC   
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  BLD_TYPE_USE       RESIDENTIAL        DESCRIPTION       
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  BLD_TYPE_CONST_COD BLD_ZONING_DIST    BLD_USE_GROUP     
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  BLD_BASEMENT         FEE_TYPE          CSM_CASENO       
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  CSF_CREATED_BY         SEQ_NO   PCF_AMT_PD        PCF_AMT_DUE       
    ##  Length:7133        Min.   :1   Length:7133        Length:7133       
    ##  Class :character   1st Qu.:1   Class :character   Class :character  
    ##  Mode  :character   Median :1   Mode  :character   Mode  :character  
    ##                     Mean   :1                                        
    ##                     3rd Qu.:1                                        
    ##                     Max.   :1                                        
    ##                                                                      
    ##  PCF_UPDATED        OWNER_LAST_NAME    OWNER_FIRST_NAME  
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  OWNER_ADDRESS1     OWNER_ADDRESS2      OWNER_CITY       
    ##  Length:7133        Length:7133        Length:7133       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  OWNER_STATE         OWNER_ZIP         CONTRACTOR_LAST_NAME
    ##  Length:7133        Length:7133        Length:7133         
    ##  Class :character   Class :character   Class :character    
    ##  Mode  :character   Mode  :character   Mode  :character    
    ##                                                            
    ##                                                            
    ##                                                            
    ##                                                            
    ##  CONTRACTOR_FIRST_NAME CONTRACTOR_ADDRESS1 CONTRACTOR_ADDRESS2
    ##  Length:7133           Length:7133         Length:7133        
    ##  Class :character      Class :character    Class :character   
    ##  Mode  :character      Mode  :character    Mode  :character   
    ##                                                               
    ##                                                               
    ##                                                               
    ##                                                               
    ##  CONTRACTOR_CITY    CONTRACTOR_STATE   CONTRACTOR_ZIP 
    ##  Length:7133        Length:7133        Min.   :48009  
    ##  Class :character   Class :character   1st Qu.:48204  
    ##  Mode  :character   Mode  :character   Median :48209  
    ##                                        Mean   :48230  
    ##                                        3rd Qu.:48227  
    ##                                        Max.   :92606  
    ##                                        NA's   :179    
    ##  CONDITION_FOR_APPROVAL site_location      owner_location    
    ##  Length:7133            Length:7133        Length:7133       
    ##  Class :character       Class :character   Class :character  
    ##  Mode  :character       Mode  :character   Mode  :character  
    ##                                                              
    ##                                                              
    ##                                                              
    ##                                                              
    ##  contractor_location     geom          
    ##  Length:7133         Length:7133       
    ##  Class :character    Class :character  
    ##  Mode  :character    Mode  :character  
    ##                                        
    ##                                        
    ##                                        
    ## 

Examine the NAs

``` r
sum(permits$site_location=='') # 803 empty site_locations
```

    ## [1] 803

``` r
split_site_loc <- cSplit(permits, 'site_location','\n')
sum(is.na(split_site_loc$site_location_3)) # 858 empty coordinates
```

    ## [1] 858

Parse lat and lon

``` r
split_site_loc$site_lat_lng <- gsub("[()]", "", split_site_loc$site_location_3) # Remove parenthesis 
split_site_lat_lng <- cSplit(split_site_loc, 'site_lat_lng', ',')
permits$LAT <- split_site_lat_lng$site_lat_lng_1
permits$LON <- split_site_lat_lng$site_lat_lng_2
```

Remove the records with missing coordinates

``` r
permits <- permits[!is.na(permits$LAT),]
# Check the statistics of coordinates
summary(permits$LAT)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   42.26   42.34   42.38   42.38   42.41   69.00

``` r
summary(permits$LON)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -83.29  -83.15  -83.09  -83.06  -83.01   67.00

``` r
# Remove the outlier
permits <- permits[permits$LON > -83.3 & permits$LON < -82.8,]
permits <- permits[permits$LAT > 42.2 & permits$LAT < 42.5,]
```

Take a look at the permits distribution

``` r
cntPmtAgg = aggregate(PERMIT_NO ~ LAT+LON, data=permits, length)
names(cntPmtAgg) = c('lat','lng','freq')
orderCntPmtAgg = cntPmtAgg[order(cntPmtAgg$freq, decreasing = T),]
orderCntPmtAgg[1:10,]
```

    ##           lat       lng freq
    ## 3054 42.33168 -83.04800  527
    ## 1888 42.41591 -83.12131   45
    ## 3040 42.33250 -83.04975   20
    ## 3053 42.33168 -83.04800   19
    ## 700  42.34528 -83.21652   18
    ## 705  42.33438 -83.21604   16
    ## 2223 42.33334 -83.10735    9
    ## 1424 42.27926 -83.14091    7
    ## 3373 42.34555 -83.01520    7
    ## 1147 42.42146 -83.16060    6

Plot the permit distribution on map

``` r
library(ggmap)
pmt_map = get_googlemap(center=c(lon=-83.10, lat=42.38), maptype = "roadmap", 
                        size = c(640, 640), zoom = 11)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=42.38,-83.1&zoom=11&size=640x640&scale=2&maptype=roadmap&sensor=false

``` r
ggmap(pmt_map) + geom_point(aes(x=lng,y=lat,size=freq), data=orderCntPmtAgg[1:1000,], 
                            color = ("green")) 
```

![](Blight_data_preprocessing_files/figure-markdown_github/unnamed-chunk-40-1.png) We have access to parcel size of each property, which we can take advantage of and define a range of lats and lons for the property.

``` r
# Convert parcel size into a circle and compute parcel radius
permits$PARCEL_RADIUS <- sqrt(permits$PARCEL_SIZE/pi)
```

Create the permit building list

``` r
pmt_bld_list <- sqldf('SELECT SITE_ADDRESS, count(PERMIT_NO) as N_permits, 
                      avg(PARCEL_RADIUS) as PARCEL_RADIUS, avg(LAT) as LAT, avg(LON) AS LON 
                      FROM permits 
                      WHERE PARCEL_RADIUS IS NOT NULL
                      GROUP BY SITE_ADDRESS
                      HAVING stdev(LAT) <= 0.0001 AND stdev(LON) <= 0.0001')
```

Convert parcel radius from feet into degree range

``` r
# The percision of 0.0001 decimal degree coresponds to 11m or 38 ft in Detroit.
pmt_bld_list$PARCEL_DEGREE <- pmt_bld_list$PARCEL_RADIUS/38*0.0001
write.csv(pmt_bld_list, 'permit_building_list.csv')
```

Now we have 3 separate building lists ready. Next we will combine them and generate a complete building list.
