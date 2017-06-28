# Detroit-Blight-Analysis

https://www.coursera.org/learn/datasci-capstone/home/welcome

In this project, we will predict building abandonment (“blight”) based on a number of public datasets, including criminal incidents, permit violations, and more. This project grew out of a roundtable discussion with Socrata and city analytics leaders in Detroit, Kansas City, New Orleans, Boston, and more --- these cities shared a common interest in being proactive about identifying buildings at risk for abandonment and taking steps to prevent the harms that arise as a result. 

# 1. Data exploration & cleaning #

- Understand the information in each incident dataset
- Parse geocoordinates from address tuples and clean data, e.g., seeming a data entry error, removing outliers
- Compute multiple aggregates on unique addresses and examine the frequencies of incidents
- Built interactive exploratory map to visualize violations, 311 calls, crimes and blight permits distributions on the map

# 2. Feature selection/extraction #
- Remove redundant features based on common sense and selecte basic features for each incident dataset at the building level, e.g., # of violations, violation code/type, violation fees, # of 311 calls and ratings, 311 call issue type, # of crimes and crime types...

# 3. Generating a list of building #
- Combine the incident datasets where each dataset is aggregated by unique addresses
- Define property group with a high spatial density as observation units for predictive modeling
- Use DBSCAN to to cluster the lat/long coordinates with a maximum cluster range of 30 meters and a minimum number of cluster elements of 1
- Re-aggregate the incidents that are assigned the to the same cluster
- Label the clusters as Blight if its centroid coordinates is within a blight parcel, where blight parcels are defined by a circumference centered at their lat/long and with a radius such as the resulting area is equal to parcel size.

# 4. Predictive modeling 
- Explore different models such as KNN, GBM, SVM(linear/radial), XGBOOST...
- GBM yields the best performance of 73.5% AUC in cross-validation with all the selected features, and 74.3% AUC in the test dataset. 

# 5. Evaluation of the best model

Confusion Matrix and Statistics

| Prediction    | Not Blight  | Yes Blight  |
| ----------- |:-----------:| -----------:|
| Not Blight    | 127 | 35 |
| Yes Blight    | 141 | 237 |

                Accuracy : 0.6741          
                95% CI : (0.6327, 0.7135)
     No Information Rate : 0.5037          
     P-Value [Acc > NIR] : 8.628e-16                                    
                   Kappa : 0.3462          
                   Mcnemar's Test P-Value : 2.479e-15                                
             Sensitivity : 0.4739          
             Specificity : 0.8713          
          Pos Pred Value : 0.7840          
          Neg Pred Value : 0.6270          
              Prevalence : 0.4963          
          Detection Rate : 0.2352          
    Detection Prevalence : 0.3000          
       Balanced Accuracy : 0.6726          

- The threshold I set for the probability of 0.5, and the resulting overall accuracy is 67.4%, which is much better than random guessing. The TP rate is 0.473, and NP rate is 0.871. In practice we can give different weights for Type 1&2 error and find the threshold that will minimize the cost for misclassification. 
