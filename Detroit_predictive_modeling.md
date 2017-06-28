Predictive modeling of Detroit blight properties
================
Sasa Li (<sl7rf@mail.missouri.edu>)

Load libraries

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(e1071)
library(doMC)
```

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

``` r
library(caret)
library(rpart)
library(caretEnsemble)
```

    ## 
    ## Attaching package: 'caretEnsemble'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     autoplot

Load data

``` r
setwd("~/Documents/Coursera/Capstone Project Detroit/data")
data <- read.csv('data_for_modeling.csv')
data$Lat <- NULL
data$Lon <- NULL
summary(data)
```

    ##   N_Violations        MaxTotalFee      ViolationCode_22.2.17
    ##  Min.   :    0.000   Min.   :    0.0   Min.   :  0.00000    
    ##  1st Qu.:    0.000   1st Qu.:    0.0   1st Qu.:  0.00000    
    ##  Median :    2.000   Median :  500.0   Median :  0.00000    
    ##  Mean   :    7.787   Mean   :  754.8   Mean   :  0.05586    
    ##  3rd Qu.:    7.000   3rd Qu.:  798.6   3rd Qu.:  0.00000    
    ##  Max.   :21185.000   Max.   :22060.0   Max.   :115.00000    
    ##  ViolationCode_22.2.22 ViolationCode_22.2.43 ViolationCode_22.2.45
    ##  Min.   :  0.000       Min.   :  0.00000     Min.   :  0.0000     
    ##  1st Qu.:  0.000       1st Qu.:  0.00000     1st Qu.:  0.0000     
    ##  Median :  0.000       Median :  0.00000     Median :  0.0000     
    ##  Mean   :  0.103       Mean   :  0.09221     Mean   :  0.1666     
    ##  3rd Qu.:  0.000       3rd Qu.:  0.00000     3rd Qu.:  0.0000     
    ##  Max.   :119.000       Max.   :231.00000     Max.   :201.0000     
    ##  ViolationCode_22.2.61 ViolationCode_22.2.83 ViolationCode_22.2.88
    ##  Min.   :  0.0000      Min.   :  0.00000     Min.   :   0.000     
    ##  1st Qu.:  0.0000      1st Qu.:  0.00000     1st Qu.:   0.000     
    ##  Median :  0.0000      Median :  0.00000     Median :   0.000     
    ##  Mean   :  0.0771      Mean   :  0.05626     Mean   :   1.316     
    ##  3rd Qu.:  0.0000      3rd Qu.:  0.00000     3rd Qu.:   1.000     
    ##  Max.   :638.0000      Max.   :137.00000     Max.   :2864.000     
    ##  ViolationCode_61.81 ViolationCode_9.1.103 ViolationCode_9.1.104
    ##  Min.   : 0.00000    Min.   :  0.0000      Min.   :   0.0000    
    ##  1st Qu.: 0.00000    1st Qu.:  0.0000      1st Qu.:   0.0000    
    ##  Median : 0.00000    Median :  0.0000      Median :   0.0000    
    ##  Mean   : 0.02145    Mean   :  0.1373      Mean   :   0.9768    
    ##  3rd Qu.: 0.00000    3rd Qu.:  0.0000      3rd Qu.:   1.0000    
    ##  Max.   :87.00000    Max.   :822.0000      Max.   :1987.0000    
    ##  ViolationCode_9.1.105 ViolationCode_9.1.110 ViolationCode_9.1.111
    ##  Min.   :  0.0000      Min.   :  0.0000      Min.   :  0.00000    
    ##  1st Qu.:  0.0000      1st Qu.:  0.0000      1st Qu.:  0.00000    
    ##  Median :  0.0000      Median :  0.0000      Median :  0.00000    
    ##  Mean   :  0.1335      Mean   :  0.1723      Mean   :  0.03964    
    ##  3rd Qu.:  0.0000      3rd Qu.:  0.0000      3rd Qu.:  0.00000    
    ##  Max.   :282.0000      Max.   :332.0000      Max.   :222.00000    
    ##  ViolationCode_9.1.113 ViolationCode_9.1.36 ViolationCode_9.1.43
    ##  Min.   :  0.00000     Min.   :   0.000     Min.   :  0.0000    
    ##  1st Qu.:  0.00000     1st Qu.:   0.000     1st Qu.:  0.0000    
    ##  Median :  0.00000     Median :   0.000     Median :  0.0000    
    ##  Mean   :  0.04614     Mean   :   2.696     Mean   :  0.2068    
    ##  3rd Qu.:  0.00000     3rd Qu.:   2.000     3rd Qu.:  0.0000    
    ##  Max.   :104.00000     Max.   :9075.000     Max.   :467.0000    
    ##  ViolationCode_9.1.45 ViolationCode_9.1.50 ViolationCode_9.1.81
    ##  Min.   :  0.00000    Min.   : 0.0000      Min.   :   0.00     
    ##  1st Qu.:  0.00000    1st Qu.: 0.0000      1st Qu.:   0.00     
    ##  Median :  0.00000    Median : 0.0000      Median :   0.00     
    ##  Mean   :  0.07602    Mean   : 0.0231      Mean   :   1.14     
    ##  3rd Qu.:  0.00000    3rd Qu.: 0.0000      3rd Qu.:   1.00     
    ##  Max.   :188.00000    Max.   :66.0000      Max.   :2399.00     
    ##  ViolationCode_9.1.82 ViolationCode_OtherViolationCode    N_crimes      
    ##  Min.   :  0.00000    Min.   :  0.0000                 Min.   :  0.000  
    ##  1st Qu.:  0.00000    1st Qu.:  0.0000                 1st Qu.:  0.000  
    ##  Median :  0.00000    Median :  0.0000                 Median :  0.000  
    ##  Mean   :  0.05669    Mean   :  0.1933                 Mean   :  1.086  
    ##  3rd Qu.:  0.00000    3rd Qu.:  0.0000                 3rd Qu.:  1.000  
    ##  Max.   :122.00000    Max.   :752.0000                 Max.   :241.000  
    ##  CATEGORY_AGGRAVATED.ASSAULT CATEGORY_ARSON     CATEGORY_ASSAULT 
    ##  Min.   : 0.00000            Min.   :0.000000   Min.   : 0.0000  
    ##  1st Qu.: 0.00000            1st Qu.:0.000000   1st Qu.: 0.0000  
    ##  Median : 0.00000            Median :0.000000   Median : 0.0000  
    ##  Mean   : 0.07212            Mean   :0.006122   Mean   : 0.1315  
    ##  3rd Qu.: 0.00000            3rd Qu.:0.000000   3rd Qu.: 0.0000  
    ##  Max.   :13.00000            Max.   :2.000000   Max.   :21.0000  
    ##  CATEGORY_BRIBERY   CATEGORY_BURGLARY  CATEGORY_DAMAGE.TO.PROPERTY
    ##  Min.   :0.000000   Min.   : 0.00000   Min.   : 0.0000            
    ##  1st Qu.:0.000000   1st Qu.: 0.00000   1st Qu.: 0.0000            
    ##  Median :0.000000   Median : 0.00000   Median : 0.0000            
    ##  Mean   :0.007488   Mean   : 0.05426   Mean   : 0.0701            
    ##  3rd Qu.:0.000000   3rd Qu.: 0.00000   3rd Qu.: 0.0000            
    ##  Max.   :9.000000   Max.   :12.00000   Max.   :19.0000            
    ##  CATEGORY_DANGEROUS.DRUGS CATEGORY_ESCAPE   CATEGORY_FRAUD   
    ##  Min.   : 0.00000         Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.: 0.00000         1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median : 0.00000         Median :0.00000   Median :0.00000  
    ##  Mean   : 0.02671         Mean   :0.00931   Mean   :0.03962  
    ##  3rd Qu.: 0.00000         3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :12.00000         Max.   :9.00000   Max.   :6.00000  
    ##  CATEGORY_LARCENY  CATEGORY_OBSTRUCTING.JUDICIARY CATEGORY_OtherCrimes
    ##  Min.   : 0.0000   Min.   :0.00000                Min.   :0.00000     
    ##  1st Qu.: 0.0000   1st Qu.:0.00000                1st Qu.:0.00000     
    ##  Median : 0.0000   Median :0.00000                Median :0.00000     
    ##  Mean   : 0.1304   Mean   :0.01348                Mean   :0.02368     
    ##  3rd Qu.: 0.0000   3rd Qu.:0.00000                3rd Qu.:0.00000     
    ##  Max.   :52.0000   Max.   :3.00000                Max.   :7.00000     
    ##  CATEGORY_OUIL.DISPOSE.OF.VEHICLE.TO.AVOID.FORFEITURE CATEGORY_ROBBERY 
    ##  Min.   :0.000000                                     Min.   :0.00000  
    ##  1st Qu.:0.000000                                     1st Qu.:0.00000  
    ##  Median :0.000000                                     Median :0.00000  
    ##  Mean   :0.009891                                     Mean   :0.04549  
    ##  3rd Qu.:0.000000                                     3rd Qu.:0.00000  
    ##  Max.   :9.000000                                     Max.   :9.00000  
    ##  CATEGORY_RUNAWAY    CATEGORY_SOLICITATION CATEGORY_STOLEN.VEHICLE
    ##  Min.   : 0.000000   Min.   :0.00000       Min.   : 0.00000       
    ##  1st Qu.: 0.000000   1st Qu.:0.00000       1st Qu.: 0.00000       
    ##  Median : 0.000000   Median :0.00000       Median : 0.00000       
    ##  Mean   : 0.004604   Mean   :0.01131       Mean   : 0.05452       
    ##  3rd Qu.: 0.000000   3rd Qu.:0.00000       3rd Qu.: 0.00000       
    ##  Max.   :20.000000   Max.   :5.00000       Max.   :14.00000       
    ##  CATEGORY_TRAFFIC.VIOLATIONS.DRIVING.ON.SUSPENDED
    ##  Min.   : 0.00000                                
    ##  1st Qu.: 0.00000                                
    ##  Median : 0.00000                                
    ##  Mean   : 0.05798                                
    ##  3rd Qu.: 0.00000                                
    ##  Max.   :31.00000                                
    ##  CATEGORY_TRAFFIC.VIOLATIONS.MOTORCYCLE.VIOLATIONS
    ##  Min.   : 0.0000                                  
    ##  1st Qu.: 0.0000                                  
    ##  Median : 0.0000                                  
    ##  Mean   : 0.3039                                  
    ##  3rd Qu.: 0.0000                                  
    ##  Max.   :60.0000                                  
    ##  CATEGORY_WEAPONS.OFFENSES    N_calls          Max_Rating     
    ##  Min.   :0.00000           Min.   : 0.0000   Min.   :  0.000  
    ##  1st Qu.:0.00000           1st Qu.: 0.0000   1st Qu.:  0.000  
    ##  Median :0.00000           Median : 0.0000   Median :  0.000  
    ##  Mean   :0.01391           Mean   : 0.4804   Mean   :  1.195  
    ##  3rd Qu.:0.00000           3rd Qu.: 1.0000   3rd Qu.:  2.000  
    ##  Max.   :3.00000           Max.   :91.0000   Max.   :220.000  
    ##  issue_type_Abandoned.Vehicle issue_type_Clogged.Drain
    ##  Min.   : 0.00000             Min.   : 0.00000        
    ##  1st Qu.: 0.00000             1st Qu.: 0.00000        
    ##  Median : 0.00000             Median : 0.00000        
    ##  Mean   : 0.01558             Mean   : 0.06163        
    ##  3rd Qu.: 0.00000             3rd Qu.: 0.00000        
    ##  Max.   :25.00000             Max.   :10.00000        
    ##  issue_type_Curbside.Solid.Waste.Issue issue_type_DPW...Debris.Removal
    ##  Min.   :0.000000                      Min.   :0.000000               
    ##  1st Qu.:0.000000                      1st Qu.:0.000000               
    ##  Median :0.000000                      Median :0.000000               
    ##  Mean   :0.004022                      Mean   :0.004655               
    ##  3rd Qu.:0.000000                      3rd Qu.:0.000000               
    ##  Max.   :4.000000                      Max.   :4.000000               
    ##  issue_type_Fire.Hydrant.Issue
    ##  Min.   :0.00000              
    ##  1st Qu.:0.00000              
    ##  Median :0.00000              
    ##  Mean   :0.01614              
    ##  3rd Qu.:0.00000              
    ##  Max.   :4.00000              
    ##  issue_type_Illegal.Dumping...Illegal.Dump.Sites
    ##  Min.   : 0.000                                 
    ##  1st Qu.: 0.000                                 
    ##  Median : 0.000                                 
    ##  Mean   : 0.087                                 
    ##  3rd Qu.: 0.000                                 
    ##  Max.   :23.000                                 
    ##  issue_type_Manhole.Cover.Issue issue_type_OtherIssue issue_type_Potholes
    ##  Min.   :0.00000                Min.   : 0.000000     Min.   : 0.00000   
    ##  1st Qu.:0.00000                1st Qu.: 0.000000     1st Qu.: 0.00000   
    ##  Median :0.00000                Median : 0.000000     Median : 0.00000   
    ##  Mean   :0.01283                Mean   : 0.003946     Mean   : 0.05695   
    ##  3rd Qu.:0.00000                3rd Qu.: 0.000000     3rd Qu.: 0.00000   
    ##  Max.   :4.00000                Max.   :11.000000     Max.   :29.00000   
    ##  issue_type_Running.Water.in.a.Home.or.Building
    ##  Min.   : 0.00000                              
    ##  1st Qu.: 0.00000                              
    ##  Median : 0.00000                              
    ##  Mean   : 0.06663                              
    ##  3rd Qu.: 0.00000                              
    ##  Max.   :86.00000                              
    ##  issue_type_Street.Light.Pole.Down issue_type_Traffic.Sign.Issue
    ##  Min.   :0.000000                  Min.   : 0.00000             
    ##  1st Qu.:0.000000                  1st Qu.: 0.00000             
    ##  Median :0.000000                  Median : 0.00000             
    ##  Mean   :0.004478                  Mean   : 0.02479             
    ##  3rd Qu.:0.000000                  3rd Qu.: 0.00000             
    ##  Max.   :3.000000                  Max.   :17.00000             
    ##  issue_type_Traffic.Signal.Issue
    ##  Min.   :0.000000               
    ##  1st Qu.:0.000000               
    ##  Median :0.000000               
    ##  Mean   :0.009057               
    ##  3rd Qu.:0.000000               
    ##  Max.   :9.000000               
    ##  issue_type_Trash.Issue...Bulk.waste.deposited.more.than.24.hours.before.designated.time
    ##  Min.   :0.000000                                                                       
    ##  1st Qu.:0.000000                                                                       
    ##  Median :0.000000                                                                       
    ##  Mean   :0.006173                                                                       
    ##  3rd Qu.:0.000000                                                                       
    ##  Max.   :4.000000                                                                       
    ##  issue_type_Tree.Issue issue_type_Water.Main.Break Blight   
    ##  Min.   : 0.00000      Min.   :0.0000              N:37609  
    ##  1st Qu.: 0.00000      1st Qu.:0.0000              Y: 1920  
    ##  Median : 0.00000      Median :0.0000                       
    ##  Mean   : 0.08743      Mean   :0.0191                       
    ##  3rd Qu.: 0.00000      3rd Qu.:0.0000                       
    ##  Max.   :14.00000      Max.   :6.0000

Sampling and create training/testing sets

Caret package has a set of functions that can streamline the process for creating predictive models.First we set up the train control, then we train different models using 'train' function in Caret with the same trainControl settings.

``` r
set.seed(13)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5, 
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = 'all',
                     allowParallel = TRUE,
                     index=createMultiFolds(train_df$Blight, k=5, times=3)
                     )
```

Next we are going to train some models.

``` r
# parallel computation
registerDoMC(cores = 5)
```

``` r
# Model 1: gbm
set.seed(7)
model_1 <- train(Blight ~.,
                 data = train_df,
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = "gbm",
                 tuneGrid = expand.grid(.n.trees=500, 
                                     .interaction.depth=c(3, 5, 7), 
                                     .n.minobsinnode = c(5,10),
                                     .shrinkage = c(0.01,0.001)
                                     ),
                 metric = 'ROC'
                 )
```

    ## Loading required package: gbm

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:caret':
    ## 
    ##     cluster

    ## Loading required package: splines

    ## Loaded gbm 2.1.3

    ## Loading required package: plyr

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1        1.3859             nan     0.0010    0.0002
    ##      2        1.3855             nan     0.0010    0.0002
    ##      3        1.3851             nan     0.0010    0.0002
    ##      4        1.3847             nan     0.0010    0.0002
    ##      5        1.3843             nan     0.0010    0.0002
    ##      6        1.3839             nan     0.0010    0.0002
    ##      7        1.3835             nan     0.0010    0.0002
    ##      8        1.3831             nan     0.0010    0.0002
    ##      9        1.3827             nan     0.0010    0.0002
    ##     10        1.3824             nan     0.0010    0.0002
    ##     20        1.3784             nan     0.0010    0.0002
    ##     40        1.3708             nan     0.0010    0.0002
    ##     60        1.3635             nan     0.0010    0.0002
    ##     80        1.3565             nan     0.0010    0.0002
    ##    100        1.3496             nan     0.0010    0.0002
    ##    120        1.3430             nan     0.0010    0.0001
    ##    140        1.3366             nan     0.0010    0.0001
    ##    160        1.3304             nan     0.0010    0.0001
    ##    180        1.3244             nan     0.0010    0.0001
    ##    200        1.3187             nan     0.0010    0.0001
    ##    220        1.3131             nan     0.0010    0.0001
    ##    240        1.3078             nan     0.0010    0.0001
    ##    260        1.3026             nan     0.0010    0.0001
    ##    280        1.2975             nan     0.0010    0.0001
    ##    300        1.2926             nan     0.0010    0.0001
    ##    320        1.2880             nan     0.0010    0.0001
    ##    340        1.2834             nan     0.0010    0.0001
    ##    360        1.2791             nan     0.0010    0.0001
    ##    380        1.2748             nan     0.0010    0.0001
    ##    400        1.2706             nan     0.0010    0.0001
    ##    420        1.2667             nan     0.0010    0.0001
    ##    440        1.2629             nan     0.0010    0.0001
    ##    460        1.2590             nan     0.0010    0.0001
    ##    480        1.2554             nan     0.0010    0.0001
    ##    500        1.2518             nan     0.0010    0.0001

``` r
model_1
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 3060 samples
    ##   62 predictor
    ##    2 classes: 'N', 'Y' 
    ## 
    ## Pre-processing: centered (62), scaled (62) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 2447, 2448, 2449, 2447, 2449, 2448, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   shrinkage  interaction.depth  n.minobsinnode  ROC        Sens     
    ##   0.001      3                   5              0.7325474  0.4852058
    ##   0.001      3                  10              0.7331376  0.4841200
    ##   0.001      5                   5              0.7356065  0.4945619
    ##   0.001      5                  10              0.7356004  0.4943462
    ##   0.001      7                   5              0.7372541  0.5034865
    ##   0.001      7                  10              0.7375058  0.5037023
    ##   0.010      3                   5              0.7350112  0.5267704
    ##   0.010      3                  10              0.7365273  0.5291598
    ##   0.010      5                   5              0.7349514  0.5389460
    ##   0.010      5                  10              0.7351829  0.5396003
    ##   0.010      7                   5              0.7343678  0.5498322
    ##   0.010      7                  10              0.7355458  0.5478643
    ##   Spec     
    ##   0.8832858
    ##   0.8815372
    ##   0.8752134
    ##   0.8723776
    ##   0.8660495
    ##   0.8664852
    ##   0.8429244
    ##   0.8396521
    ##   0.8320211
    ##   0.8298361
    ##   0.8176221
    ##   0.8160956
    ## 
    ## Tuning parameter 'n.trees' was held constant at a value of 500
    ## ROC was used to select the optimal model using  the largest value.
    ## The final values used for the model were n.trees = 500,
    ##  interaction.depth = 7, shrinkage = 0.001 and n.minobsinnode = 10.

Model 1 (gbm) yields the best performanc of AUC=0.7379111, TP=0.505, TN=0.867. TP is only slightly better than guessing, while TN is high and it would be helpful to exclude the properties with low blight risks. The final values used for the model were n.trees = 500, interaction.depth = 7, shrinkage = 0.001 and n.minobsinnode = 10.

``` r
# Model 2: knn
set.seed(1)
model_2 <- train(Blight ~.,
                 data = train_df,
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = "knn",
                 metric = 'ROC'
                 )
model_2
```

    ## k-Nearest Neighbors 
    ## 
    ## 3060 samples
    ##   62 predictor
    ##    2 classes: 'N', 'Y' 
    ## 
    ## Pre-processing: centered (62), scaled (62) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 2447, 2448, 2449, 2447, 2449, 2448, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k  ROC        Sens       Spec     
    ##   5  0.6594248  0.6416583  0.5826572
    ##   7  0.6688984  0.6510152  0.5878989
    ##   9  0.6689536  0.6501451  0.5817908
    ## 
    ## ROC was used to select the optimal model using  the largest value.
    ## The final value used for the model was k = 9.

Model 2 (knn) yields the best performance of AUC=0.6689536, TP=0.650, TN=0.582, with k=9.

``` r
# Model 3: xgboost
set.seed(3)
model_3 <- train(Blight ~.,
                 data = train_df,
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = "xgbTree"
                 )
```

    ## Loading required package: xgboost

    ## Warning in train.default(x, y, weights = w, ...): The metric "Accuracy" was
    ## not in the result set. ROC will be used instead.

``` r
model_3
```

    ## eXtreme Gradient Boosting 
    ## 
    ## 3060 samples
    ##   62 predictor
    ##    2 classes: 'N', 'Y' 
    ## 
    ## Pre-processing: centered (62), scaled (62) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 2447, 2448, 2449, 2447, 2449, 2448, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   eta  max_depth  colsample_bytree  subsample  nrounds  ROC      
    ##   0.3  1          0.6               0.50        50      0.7269846
    ##   0.3  1          0.6               0.50       100      0.7244943
    ##   0.3  1          0.6               0.50       150      0.7219788
    ##   0.3  1          0.6               0.75        50      0.7285676
    ##   0.3  1          0.6               0.75       100      0.7247855
    ##   0.3  1          0.6               0.75       150      0.7215917
    ##   0.3  1          0.6               1.00        50      0.7294448
    ##   0.3  1          0.6               1.00       100      0.7273566
    ##   0.3  1          0.6               1.00       150      0.7247174
    ##   0.3  1          0.8               0.50        50      0.7295765
    ##   0.3  1          0.8               0.50       100      0.7269481
    ##   0.3  1          0.8               0.50       150      0.7256291
    ##   0.3  1          0.8               0.75        50      0.7296405
    ##   0.3  1          0.8               0.75       100      0.7274515
    ##   0.3  1          0.8               0.75       150      0.7243216
    ##   0.3  1          0.8               1.00        50      0.7291363
    ##   0.3  1          0.8               1.00       100      0.7276966
    ##   0.3  1          0.8               1.00       150      0.7256817
    ##   0.3  2          0.6               0.50        50      0.7254927
    ##   0.3  2          0.6               0.50       100      0.7217734
    ##   0.3  2          0.6               0.50       150      0.7170345
    ##   0.3  2          0.6               0.75        50      0.7270512
    ##   0.3  2          0.6               0.75       100      0.7201722
    ##   0.3  2          0.6               0.75       150      0.7172881
    ##   0.3  2          0.6               1.00        50      0.7262533
    ##   0.3  2          0.6               1.00       100      0.7195291
    ##   0.3  2          0.6               1.00       150      0.7156491
    ##   0.3  2          0.8               0.50        50      0.7264498
    ##   0.3  2          0.8               0.50       100      0.7237416
    ##   0.3  2          0.8               0.50       150      0.7221184
    ##   0.3  2          0.8               0.75        50      0.7249639
    ##   0.3  2          0.8               0.75       100      0.7187995
    ##   0.3  2          0.8               0.75       150      0.7172338
    ##   0.3  2          0.8               1.00        50      0.7278879
    ##   0.3  2          0.8               1.00       100      0.7228098
    ##   0.3  2          0.8               1.00       150      0.7196577
    ##   0.3  3          0.6               0.50        50      0.7211382
    ##   0.3  3          0.6               0.50       100      0.7155919
    ##   0.3  3          0.6               0.50       150      0.7076710
    ##   0.3  3          0.6               0.75        50      0.7190181
    ##   0.3  3          0.6               0.75       100      0.7131133
    ##   0.3  3          0.6               0.75       150      0.7071993
    ##   0.3  3          0.6               1.00        50      0.7207562
    ##   0.3  3          0.6               1.00       100      0.7126992
    ##   0.3  3          0.6               1.00       150      0.7080754
    ##   0.3  3          0.8               0.50        50      0.7181137
    ##   0.3  3          0.8               0.50       100      0.7119584
    ##   0.3  3          0.8               0.50       150      0.7052119
    ##   0.3  3          0.8               0.75        50      0.7224709
    ##   0.3  3          0.8               0.75       100      0.7162296
    ##   0.3  3          0.8               0.75       150      0.7115115
    ##   0.3  3          0.8               1.00        50      0.7253425
    ##   0.3  3          0.8               1.00       100      0.7183739
    ##   0.3  3          0.8               1.00       150      0.7141080
    ##   0.4  1          0.6               0.50        50      0.7282223
    ##   0.4  1          0.6               0.50       100      0.7242818
    ##   0.4  1          0.6               0.50       150      0.7224204
    ##   0.4  1          0.6               0.75        50      0.7290684
    ##   0.4  1          0.6               0.75       100      0.7241620
    ##   0.4  1          0.6               0.75       150      0.7203099
    ##   0.4  1          0.6               1.00        50      0.7286564
    ##   0.4  1          0.6               1.00       100      0.7252680
    ##   0.4  1          0.6               1.00       150      0.7227113
    ##   0.4  1          0.8               0.50        50      0.7270835
    ##   0.4  1          0.8               0.50       100      0.7209040
    ##   0.4  1          0.8               0.50       150      0.7198729
    ##   0.4  1          0.8               0.75        50      0.7287406
    ##   0.4  1          0.8               0.75       100      0.7248512
    ##   0.4  1          0.8               0.75       150      0.7198794
    ##   0.4  1          0.8               1.00        50      0.7279692
    ##   0.4  1          0.8               1.00       100      0.7261516
    ##   0.4  1          0.8               1.00       150      0.7244073
    ##   0.4  2          0.6               0.50        50      0.7257529
    ##   0.4  2          0.6               0.50       100      0.7151887
    ##   0.4  2          0.6               0.50       150      0.7120649
    ##   0.4  2          0.6               0.75        50      0.7218057
    ##   0.4  2          0.6               0.75       100      0.7163564
    ##   0.4  2          0.6               0.75       150      0.7132741
    ##   0.4  2          0.6               1.00        50      0.7237695
    ##   0.4  2          0.6               1.00       100      0.7178966
    ##   0.4  2          0.6               1.00       150      0.7131091
    ##   0.4  2          0.8               0.50        50      0.7206448
    ##   0.4  2          0.8               0.50       100      0.7139443
    ##   0.4  2          0.8               0.50       150      0.7083445
    ##   0.4  2          0.8               0.75        50      0.7253787
    ##   0.4  2          0.8               0.75       100      0.7159505
    ##   0.4  2          0.8               0.75       150      0.7108226
    ##   0.4  2          0.8               1.00        50      0.7245178
    ##   0.4  2          0.8               1.00       100      0.7192892
    ##   0.4  2          0.8               1.00       150      0.7170122
    ##   0.4  3          0.6               0.50        50      0.7134117
    ##   0.4  3          0.6               0.50       100      0.7096712
    ##   0.4  3          0.6               0.50       150      0.7046248
    ##   0.4  3          0.6               0.75        50      0.7156853
    ##   0.4  3          0.6               0.75       100      0.7096502
    ##   0.4  3          0.6               0.75       150      0.7036892
    ##   0.4  3          0.6               1.00        50      0.7171452
    ##   0.4  3          0.6               1.00       100      0.7108373
    ##   0.4  3          0.6               1.00       150      0.7043486
    ##   0.4  3          0.8               0.50        50      0.7149834
    ##   0.4  3          0.8               0.50       100      0.7099893
    ##   0.4  3          0.8               0.50       150      0.7039023
    ##   0.4  3          0.8               0.75        50      0.7203054
    ##   0.4  3          0.8               0.75       100      0.7129903
    ##   0.4  3          0.8               0.75       150      0.7065397
    ##   0.4  3          0.8               1.00        50      0.7203315
    ##   0.4  3          0.8               1.00       100      0.7120263
    ##   0.4  3          0.8               1.00       150      0.7075584
    ##   Sens       Spec     
    ##   0.5350379  0.8250180
    ##   0.5424333  0.8161049
    ##   0.5530888  0.7995171
    ##   0.5156636  0.8507647
    ##   0.5311135  0.8274296
    ##   0.5367681  0.8117276
    ##   0.5054438  0.8588435
    ##   0.5189372  0.8426965
    ##   0.5224174  0.8346255
    ##   0.5398239  0.8291782
    ##   0.5535359  0.8073781
    ##   0.5604976  0.7903475
    ##   0.5226374  0.8435673
    ##   0.5402688  0.8248116
    ##   0.5437405  0.8119411
    ##   0.5095768  0.8588514
    ##   0.5232839  0.8403007
    ##   0.5274190  0.8304875
    ##   0.5587682  0.7953627
    ##   0.5746425  0.7582835
    ##   0.5785463  0.7462767
    ##   0.5511344  0.8003822
    ##   0.5637606  0.7744312
    ##   0.5720303  0.7534876
    ##   0.5356944  0.8232808
    ##   0.5437426  0.7962349
    ##   0.5526680  0.7752963
    ##   0.5607006  0.7873045
    ##   0.5805142  0.7604593
    ##   0.5866095  0.7458416
    ##   0.5457012  0.8058266
    ##   0.5633220  0.7731069
    ##   0.5750640  0.7543548
    ##   0.5346086  0.8208879
    ##   0.5485299  0.7949305
    ##   0.5578875  0.7800993
    ##   0.5816050  0.7560984
    ##   0.5985729  0.7159606
    ##   0.6059711  0.7070260
    ##   0.5626713  0.7711525
    ##   0.5835508  0.7371249
    ##   0.5922569  0.7144384
    ##   0.5454834  0.7986292
    ##   0.5626677  0.7665652
    ##   0.5746418  0.7462838
    ##   0.5774528  0.7589321
    ##   0.5870253  0.7307847
    ##   0.5989965  0.7074617
    ##   0.5704988  0.7718140
    ##   0.5831222  0.7417051
    ##   0.5920433  0.7172678
    ##   0.5491835  0.7986307
    ##   0.5663658  0.7683203
    ##   0.5748490  0.7473803
    ##   0.5493957  0.8073353
    ##   0.5557152  0.7916476
    ##   0.5617998  0.7827137
    ##   0.5217695  0.8424887
    ##   0.5356901  0.8189207
    ##   0.5493865  0.8010379
    ##   0.5154563  0.8499004
    ##   0.5248103  0.8341891
    ##   0.5245939  0.8276453
    ##   0.5498315  0.8056231
    ##   0.5548353  0.7883810
    ##   0.5604955  0.7864159
    ##   0.5341693  0.8311468
    ##   0.5454756  0.8069367
    ##   0.5535281  0.7971106
    ##   0.5163264  0.8455395
    ##   0.5295941  0.8267688
    ##   0.5337343  0.8180492
    ##   0.5726867  0.7726747
    ##   0.5913947  0.7329719
    ##   0.5876945  0.7262138
    ##   0.5535394  0.7846823
    ##   0.5776940  0.7563227
    ##   0.5861780  0.7377821
    ##   0.5398189  0.8119490
    ##   0.5550552  0.7886046
    ##   0.5618083  0.7665838
    ##   0.5600562  0.7752905
    ##   0.5805029  0.7462859
    ##   0.5903103  0.7205479
    ##   0.5635435  0.7905675
    ##   0.5735475  0.7565306
    ##   0.5787734  0.7371206
    ##   0.5433012  0.8108511
    ##   0.5576703  0.7846823
    ##   0.5607190  0.7628615
    ##   0.5824686  0.7338398
    ##   0.6070512  0.7037380
    ##   0.6074855  0.6954484
    ##   0.5755133  0.7515261
    ##   0.5992229  0.7126819
    ##   0.6000965  0.6963177
    ##   0.5622377  0.7792200
    ##   0.5765997  0.7478167
    ##   0.5824750  0.7288367
    ##   0.5935648  0.7207643
    ##   0.6096748  0.6985171
    ##   0.6133756  0.6860795
    ##   0.5776862  0.7502232
    ##   0.5918290  0.7233794
    ##   0.5985743  0.7052695
    ##   0.5596240  0.7711475
    ##   0.5781205  0.7401714
    ##   0.5889968  0.7301404
    ## 
    ## Tuning parameter 'gamma' was held constant at a value of 0
    ## 
    ## Tuning parameter 'min_child_weight' was held constant at a value of 1
    ## ROC was used to select the optimal model using  the largest value.
    ## The final values used for the model were nrounds = 50, max_depth = 1,
    ##  eta = 0.3, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1
    ##  and subsample = 0.75.

``` r
varImp(model_3)
```

    ## xgbTree variable importance
    ## 
    ##   only 20 most important variables shown (out of 62)
    ## 
    ##                                                       Overall
    ## MaxTotalFee                                          100.0000
    ## N_Violations                                          13.9314
    ## ViolationCode_9.1.36                                   6.6143
    ## ViolationCode_22.2.88                                  5.0729
    ## Max_Rating                                             3.9364
    ## ViolationCode_9.1.103                                  2.5955
    ## N_crimes                                               2.3303
    ## ViolationCode_9.1.104                                  2.2261
    ## CATEGORY_OUIL.DISPOSE.OF.VEHICLE.TO.AVOID.FORFEITURE   1.5967
    ## CATEGORY_BRIBERY                                       1.5182
    ## ViolationCode_9.1.43                                   1.1723
    ## ViolationCode_9.1.110                                  1.0677
    ## CATEGORY_DANGEROUS.DRUGS                               1.0626
    ## ViolationCode_OtherViolationCode                       0.8199
    ## CATEGORY_FRAUD                                         0.6373
    ## CATEGORY_WEAPONS.OFFENSES                              0.5619
    ## ViolationCode_9.1.81                                   0.5570
    ## ViolationCode_9.1.82                                   0.5408
    ## ViolationCode_9.1.105                                  0.5402
    ## CATEGORY_ROBBERY                                       0.5132

Model 3 yields the best performance with AUC=0.7296, where TP=0.5226, NP=0.7582. The top 5 most important features are MaxTotalFee, N\_Violations, ViolationCode\_22.2.88 , ViolationCode\_9.1.36, ViolationCode\_9.1.103. These features are all from violations data.

``` r
# Model 4: svm radial
set.seed(3)
model_4 <- train(Blight ~.,
                 data = train_df,
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = 'svmRadial',
                 tuneGrid = expand.grid(.C=c(0.1, 0.5, 0.75, 1), 
                                        .sigma= c(.01, .015, 0.2))
)
```

    ## Loading required package: kernlab

    ## 
    ## Attaching package: 'kernlab'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     alpha

    ## Warning in train.default(x, y, weights = w, ...): The metric "Accuracy" was
    ## not in the result set. ROC will be used instead.

``` r
model_4
```

    ## Support Vector Machines with Radial Basis Function Kernel 
    ## 
    ## 3060 samples
    ##   62 predictor
    ##    2 classes: 'N', 'Y' 
    ## 
    ## Pre-processing: centered (62), scaled (62) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 2447, 2448, 2449, 2447, 2449, 2448, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   C     sigma  ROC        Sens       Spec     
    ##   0.10  0.010  0.6562956  0.6092362  0.6552791
    ##   0.10  0.015  0.6529036  0.5929417  0.6583371
    ##   0.10  0.200  0.6643792  0.6764670  0.5401164
    ##   0.50  0.010  0.6754635  0.6268669  0.6430573
    ##   0.50  0.015  0.6779216  0.6196838  0.6474388
    ##   0.50  0.200  0.6799383  0.6046546  0.6300796
    ##   0.75  0.010  0.6795696  0.6236039  0.6491753
    ##   0.75  0.015  0.6837404  0.6201202  0.6550841
    ##   0.75  0.200  0.6797189  0.5987787  0.6453395
    ##   1.00  0.010  0.6825501  0.6222889  0.6572563
    ##   1.00  0.015  0.6866243  0.6146814  0.6677446
    ##   1.00  0.200  0.6799060  0.6259834  0.6142855
    ## 
    ## ROC was used to select the optimal model using  the largest value.
    ## The final values used for the model were sigma = 0.015 and C = 1.

Model 4 yields the best performance with AUC=0.6866, TP=0.6147, NP=0.6677. Compared to the previous models, this model has a more balanced accuracy for both blight and non-blight buildings.

``` r
# Model 5: svm linear
set.seed(3)
model_5 <- train(Blight ~.,
                 data = train_df,
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = 'svmLinear'
                 )
```

    ## Warning in train.default(x, y, weights = w, ...): The metric "Accuracy" was
    ## not in the result set. ROC will be used instead.

``` r
model_5
```

    ## Support Vector Machines with Linear Kernel 
    ## 
    ## 3060 samples
    ##   62 predictor
    ##    2 classes: 'N', 'Y' 
    ## 
    ## Pre-processing: centered (62), scaled (62) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 2447, 2448, 2449, 2447, 2449, 2448, ... 
    ## Resampling results:
    ## 
    ##   ROC        Sens       Spec    
    ##   0.7026882  0.7635073  0.493024
    ## 
    ## Tuning parameter 'C' was held constant at a value of 1

Model 5 yields the performance of AUC=0.7027, with TP=0.7635 and NP=0.4930&lt;0.5.

Next we want to examine the correlations among these models and see if we can improve the performance by ensembling.

Make a list of all the models:

``` r
all.models <- list(model_1, model_2, model_3, model_4, model_5)
names(all.models) <- sapply(all.models, function(x) x$method)
sort(sapply(all.models, function(x) min(x$results$ROC)))
```

    ## svmRadial       knn svmLinear   xgbTree       gbm 
    ## 0.6529036 0.6594248 0.7026882 0.7036892 0.7325474

Set the model list as caretList:

``` r
class(all.models) <- 'caretList'
results <- resamples(all.models)
summary(results)
```

    ## 
    ## Call:
    ## summary.resamples(object = results)
    ## 
    ## Models: gbm, knn, xgbTree, svmRadial, svmLinear 
    ## Number of resamples: 15 
    ## 
    ## ROC 
    ##             Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## gbm       0.7000  0.7280 0.7385 0.7375  0.7487 0.7654    0
    ## knn       0.6362  0.6546 0.6700 0.6690  0.6864 0.6980    0
    ## xgbTree   0.6840  0.7205 0.7318 0.7296  0.7410 0.7538    0
    ## svmRadial 0.6227  0.6701 0.6859 0.6866  0.7031 0.7533    0
    ## svmLinear 0.6683  0.6895 0.7004 0.7027  0.7185 0.7307    0
    ## 
    ## Sens 
    ##             Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## gbm       0.4542  0.4959 0.5065 0.5037  0.5195 0.5294    0
    ## knn       0.5948  0.6264 0.6482 0.6501  0.6683 0.7320    0
    ## xgbTree   0.4673  0.5122 0.5212 0.5226  0.5425 0.5686    0
    ## svmRadial 0.5621  0.5873 0.6156 0.6147  0.6378 0.6895    0
    ## svmLinear 0.7231  0.7423 0.7680 0.7635  0.7765 0.8203    0
    ## 
    ## Spec 
    ##             Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## gbm       0.8072  0.8560 0.8693 0.8665  0.8822 0.9085    0
    ## knn       0.5213  0.5630 0.5817 0.5818  0.6049 0.6242    0
    ## xgbTree   0.7574  0.8361 0.8529 0.8436  0.8660 0.8824    0
    ## svmRadial 0.6066  0.6514 0.6667 0.6677  0.6852 0.7353    0
    ## svmLinear 0.4281  0.4714 0.4869 0.4930  0.5197 0.5490    0

``` r
dotplot(results)
```

![](Detroit_predictive_modeling_files/figure-markdown_github/unnamed-chunk-10-1.png)

Compute correlation between results:

``` r
modelCor(results)
```

    ##                 gbm       knn   xgbTree svmRadial svmLinear
    ## gbm       1.0000000 0.4603905 0.9377695 0.7566315 0.8258994
    ## knn       0.4603905 1.0000000 0.4997106 0.8001890 0.5707800
    ## xgbTree   0.9377695 0.4997106 1.0000000 0.7643203 0.8665439
    ## svmRadial 0.7566315 0.8001890 0.7643203 1.0000000 0.6907703
    ## svmLinear 0.8258994 0.5707800 0.8665439 0.6907703 1.0000000

``` r
splom(results)
```

![](Detroit_predictive_modeling_files/figure-markdown_github/unnamed-chunk-11-1.png)

Among all 5 models we have tried, gbm has the largest AUC value, and gbm is highly correlated (correlation&gt;0.75) with the other models except knn.

Predict for test data set:

``` r
library(caTools)
preds <- data.frame(sapply(all.models, function(x){predict(x, newdata=test_df, type = 'prob')[2]}))
sort(data.frame(colAUC(preds, test_df[,63])))
```

    ##            knn.Y svmRadial.Y svmLinear.Y xgbTree.Y     gbm.Y
    ## N vs. Y 0.682047   0.6975554   0.7249369 0.7341487 0.7432438

``` r
confusionMatrix(ifelse(preds$gbm<0.5, 'N', 'Y'), test_df[,63])
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   N   Y
    ##          N 127  35
    ##          Y 141 237
    ##                                           
    ##                Accuracy : 0.6741          
    ##                  95% CI : (0.6327, 0.7135)
    ##     No Information Rate : 0.5037          
    ##     P-Value [Acc > NIR] : 8.628e-16       
    ##                                           
    ##                   Kappa : 0.3462          
    ##  Mcnemar's Test P-Value : 2.479e-15       
    ##                                           
    ##             Sensitivity : 0.4739          
    ##             Specificity : 0.8713          
    ##          Pos Pred Value : 0.7840          
    ##          Neg Pred Value : 0.6270          
    ##              Prevalence : 0.4963          
    ##          Detection Rate : 0.2352          
    ##    Detection Prevalence : 0.3000          
    ##       Balanced Accuracy : 0.6726          
    ##                                           
    ##        'Positive' Class : N               
    ##
