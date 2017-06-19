# Detroit-Blight-Analysis

https://www.coursera.org/learn/datasci-capstone/home/welcome

In this project, we will predict building abandonment (“blight”) based on a number of public datasets, including criminal incidents, permit violations, and more. This project grew out of a roundtable discussion with Socrata and city analytics leaders in Detroit, Kansas City, New Orleans, Boston, and more --- these cities shared a common interest in being proactive about identifying buildings at risk for abandonment and taking steps to prevent the harms that arise as a result. 

# Data exploration & cleanse #
- Compute multiple aggregates and understand the information in each incident dataset
- Cleaned data, e.g., seeming a data entry error, removing outliers
- Built interactive exploratory map to visualize violations, 311 calls, crimes and blight permits distributions on the map

# Feature selection/extraction #
- Removed redundant features based on common sense and selected basic features for each incident dataset at the building level, e.g., # of violations, violation code/type, violation fees, # of 311 calls and ratings, 311 call issue type, # of crimes and crime types...

# Generating a list of building #
- Combine the incident datasets where each dataset is aggregated by unique addresses
- Use DBSCAN to to cluster the lat/long coordinates with a maximum cluster range of 30 meters and a minimum number of cluster elements of 1
- Re-aggregate the incidents that are assigned the to the same cluster
- Label the clusters as Blight if its centroid coordinates is within a blight parcel, where blight parcels are defined by a circumference centered at their lat/long and with a radius such as the resulting area is equal to parcel size.

# Predictive modelling # ongoing
- Explored different models such as KNN, GBM, SVM, XGBOOST...
- So far the best model GBM yielded performance of 74% AUC in cross-validation with all the selected features
