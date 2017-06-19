

```python
%pylab inline
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from sklearn.cluster import DBSCAN
from geopy.distance import great_circle
from shapely.geometry import MultiPoint
```

    Populating the interactive namespace from numpy and matplotlib



```python
import os
print(os.getcwd())
```

    /Users/sasali/Documents/Coursera/Capstone Project Detroit



```python
os.chdir('/Users/sasali/Documents/Coursera/Capstone Project Detroit/data/')
```


```python
# Load extracted feature data
viols_df = pd.read_csv('violations_building_list.csv',index_col=0)
crimes_df = pd.read_csv('crimes_building_list.csv',index_col=0)
calls_df = pd.read_csv('calls_building_list.csv',index_col=0)
```


```python
print(len(viols_df))
print(len(crimes_df))
print(len(calls_df))
```

    110837
    39792
    17463



```python
# Row bind 3 building lists
temp = viols_df[['Lat', 'Lon']].append(crimes_df[['Lat', 'Lon']], ignore_index=True).append(calls_df[['Lat', 'Lon']], ignore_index=True)
```


```python
temp.tail(4)
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Lat</th>
      <th>Lon</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>168088</th>
      <td>42.358900</td>
      <td>-83.157951</td>
    </tr>
    <tr>
      <th>168089</th>
      <td>42.395245</td>
      <td>-83.159339</td>
    </tr>
    <tr>
      <th>168090</th>
      <td>42.438070</td>
      <td>-83.019492</td>
    </tr>
    <tr>
      <th>168091</th>
      <td>42.414113</td>
      <td>-82.939986</td>
    </tr>
  </tbody>
</table>
</div>



We have 168092 unique incidents. In the temp list, only geocoordinates are included. Next we are going to cluster these incidents based upon the geocolation density. DBSCAN is perfect for this purpose, and we can set the maximum distance for clusters and the minimum number of objects in each cluster. 

Considering the common distance among houses and the range that incidents can have impacts on, we set eps=50 meters max distance that points can be from each other to be considered a cluster, and min_samples to 1 so that every data point gets assigned to either a cluster or forms its own cluster of 1. Nothing will be classified as noise.


```python
coords = temp.as_matrix(columns=['Lat', 'Lon'])
```


```python
seed(7)
ms_per_radian = 6371008.8 # Earch radius in meter
epsilon = 30/ms_per_radian 
db = DBSCAN(eps=epsilon, min_samples=1, algorithm='ball_tree', metric='haversine',n_jobs=4).fit(np.radians(coords))
cluster_labels = db.labels_
num_clusters = len(set(cluster_labels))
clusters = pd.Series([coords[cluster_labels == n] for n in range(num_clusters)])
print('Number of clusters: {}'.format(num_clusters))
```

    Number of clusters: 39529



```python
temp['cluster_id'] = cluster_labels
```


```python
temp.head(4)
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Lat</th>
      <th>Lon</th>
      <th>cluster_id</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>42.369786</td>
      <td>-83.216326</td>
      <td>0</td>
    </tr>
    <tr>
      <th>1</th>
      <td>42.325449</td>
      <td>-83.064139</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2</th>
      <td>42.411997</td>
      <td>-83.167339</td>
      <td>2</td>
    </tr>
    <tr>
      <th>3</th>
      <td>42.441234</td>
      <td>-83.219551</td>
      <td>3</td>
    </tr>
  </tbody>
</table>
</div>




```python
temp.to_csv('../data/building_cluster_list.csv', index=False)
```

We define a function to get the center coordinates for each cluster


```python
def get_centermost_point(cluster):
    centroid = (MultiPoint(cluster).centroid.x, MultiPoint(cluster).centroid.y)
    centermost_point = min(cluster, key=lambda point: great_circle(point, centroid).m)
    return tuple(centermost_point)
centermost_points = clusters.map(get_centermost_point)
```


```python
lats, lons = zip(*centermost_points)
rep_points = pd.DataFrame({'c_lon':lons, 'c_lat':lats})
rep_points['c_id'] = list(range(num_clusters))
```


```python
rep_points.to_csv('../data/cluster_list.csv', index=True)
```


```python
rep_points.head(10)
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>c_lat</th>
      <th>c_lon</th>
      <th>c_id</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>42.369786</td>
      <td>-83.216326</td>
      <td>0</td>
    </tr>
    <tr>
      <th>1</th>
      <td>42.325700</td>
      <td>-83.064300</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2</th>
      <td>42.411997</td>
      <td>-83.167339</td>
      <td>2</td>
    </tr>
    <tr>
      <th>3</th>
      <td>42.441070</td>
      <td>-83.219404</td>
      <td>3</td>
    </tr>
    <tr>
      <th>4</th>
      <td>42.341343</td>
      <td>-83.087699</td>
      <td>4</td>
    </tr>
    <tr>
      <th>5</th>
      <td>42.323726</td>
      <td>-83.081573</td>
      <td>5</td>
    </tr>
    <tr>
      <th>6</th>
      <td>42.330432</td>
      <td>-83.088302</td>
      <td>6</td>
    </tr>
    <tr>
      <th>7</th>
      <td>42.341719</td>
      <td>-83.114206</td>
      <td>7</td>
    </tr>
    <tr>
      <th>8</th>
      <td>42.337700</td>
      <td>-83.112900</td>
      <td>8</td>
    </tr>
    <tr>
      <th>9</th>
      <td>42.337800</td>
      <td>-83.114300</td>
      <td>9</td>
    </tr>
  </tbody>
</table>
</div>



Take a look at how many records in each cluster


```python
cluster_count = temp.groupby(['cluster_id']).size().reset_index(name='count')
```

Append rep_points with the cluster_count 


```python
clusters_list = pd.concat([cluster_count, rep_points[['c_lat','c_lon']]], axis=1, join='inner')
clusters_list = clusters_list.sort_values(by='count', ascending=False)
```


```python
print(sum(cluster_count['count']==1))
```

    18023


The sizes of clusters vary greatly, and almost half od them have size 1. This indicates those cluster areas have low incident frequency, which can be a good sign that blight risk is low. Next we will take a look of the cluster distribution on the Detroit map. 


```python
#data_high_density_cluster = clusters.loc[clusters['count'] > 50]
data_high_density_cluster = clusters_list.head(1000)
data_low_density_cluster = clusters_list.tail(18023)
```

Plot high density clusters


```python
from bokeh.io import output_file, show
from bokeh.models import (
  GMapPlot, GMapOptions, ColumnDataSource, Circle, DataRange1d, PanTool, WheelZoomTool, BoxSelectTool
)

map_options = GMapOptions(lat=42.3, lng=-83.1, map_type="roadmap", zoom=11)

plot = GMapPlot(
    x_range=DataRange1d(), y_range=DataRange1d(), map_options=map_options
)
plot.title.text = "Detroit (high incident density)"

# For GMaps to function, Google requires you obtain and enable an API key:
#
#     https://developers.google.com/maps/documentation/javascript/get-api-key
#
# Replace the value below with your personal API key:
plot.api_key = 'AIzaSyDuroAumF-QXuuuuTXVHBdE3dBZPG0gpUc'

source = ColumnDataSource(
    data=dict(
        lat=data_high_density_cluster['c_lat'],
        lon=data_high_density_cluster['c_lon'],
    )
)

circle = Circle(x="lon", y="lat", size=5, fill_color="blue", fill_alpha=0.7, line_color=None)
plot.add_glyph(source, circle)

plot.add_tools(PanTool(), WheelZoomTool(), BoxSelectTool())
output_file("gmap_plot.html")
show(plot)
```

Plot low density clusters


```python
plot = GMapPlot(
    x_range=DataRange1d(), y_range=DataRange1d(), map_options=map_options
)
plot.title.text = "Detroit (low incident density)"

# For GMaps to function, Google requires you obtain and enable an API key:
#
#     https://developers.google.com/maps/documentation/javascript/get-api-key
#
# Replace the value below with your personal API key:
plot.api_key = 'AIzaSyDuroAumF-QXuuuuTXVHBdE3dBZPG0gpUc'

source = ColumnDataSource(
    data=dict(
        lat=data_low_density_cluster['c_lat'],
        lon=data_low_density_cluster['c_lon'],
    )
)

circle = Circle(x="lon", y="lat", size=5, fill_color="blue", fill_alpha=0.7, line_color=None)
plot.add_glyph(source, circle)

plot.add_tools(PanTool(), WheelZoomTool(), BoxSelectTool())
output_file("gmap_plot.html")
show(plot)
```

    INFO:bokeh.core.state:Session output file 'gmap_plot.html' already exists, will be overwritten.


# Now we re-summarize viols, crimes and calls


```python
new_viols = viols_df
new_viols = new_viols.drop(['Lat','Lon'],axis=1)
new_viols['c_id'] = cluster_labels[range(len(new_viols))]
```


```python
new_viols.head(2)
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>ViolationAddress</th>
      <th>N_Violations</th>
      <th>MaxTotalFee</th>
      <th>ViolationCode_22-2-17</th>
      <th>ViolationCode_22-2-22</th>
      <th>ViolationCode_22-2-43</th>
      <th>ViolationCode_22-2-45</th>
      <th>ViolationCode_22-2-61</th>
      <th>ViolationCode_22-2-83</th>
      <th>ViolationCode_22-2-88</th>
      <th>...</th>
      <th>ViolationCode_9-1-111</th>
      <th>ViolationCode_9-1-113</th>
      <th>ViolationCode_9-1-36</th>
      <th>ViolationCode_9-1-43</th>
      <th>ViolationCode_9-1-45</th>
      <th>ViolationCode_9-1-50</th>
      <th>ViolationCode_9-1-81</th>
      <th>ViolationCode_9-1-82</th>
      <th>ViolationCode_OtherViolationCode</th>
      <th>c_id</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>1</th>
      <td>0 10TH</td>
      <td>70</td>
      <td>3360.0</td>
      <td>40</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>1</td>
      <td>0</td>
      <td>7</td>
      <td>...</td>
      <td>0</td>
      <td>0</td>
      <td>4</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>11</td>
      <td>0</td>
    </tr>
    <tr>
      <th>2</th>
      <td>0 10TH ST</td>
      <td>1</td>
      <td>0.0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>...</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>1</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
<p>2 rows × 25 columns</p>
</div>




```python
new_crimes = crimes_df
new_crimes = new_crimes.drop(['Lat','Lon'],axis=1)
new_crimes['c_id'] = cluster_labels[array(range(len(new_crimes))) + len(new_viols)]
```


```python
new_calls = calls_df
new_calls = new_calls.drop(['Lat','Lon'],axis=1)
new_calls['c_id'] = cluster_labels[array(range(len(new_calls))) + len(new_viols) + len(new_crimes)]
```

Aggregate viols by clusters; Use 'mean' for MaxTotalFee and 'sum' for other columns 


```python
avg_MaxTotalFee = new_viols.groupby(['c_id'])['MaxTotalFee'].mean()
```


```python
new_viols = new_viols.groupby(['c_id']).sum()
new_viols['MaxTotalFee'] = avg_MaxTotalFee
```

Aggregate crimes & calls by clusters; Use 'sum' for all columns 


```python
new_crimes = new_crimes.groupby(['c_id']).sum()
```


```python
new_calls = new_calls.groupby(['c_id']).sum()
```

Join these 3 tables (full)


```python
new_list = pd.concat([new_viols, new_crimes, new_calls], axis=1)
new_list[['Lat','Lon']] = rep_points[['c_lat','c_lon']]
```


```python
new_list = new_list.fillna(0)
```


```python
new_list.columns
```




    Index(['N_Violations', 'MaxTotalFee', 'ViolationCode_22-2-17',
           'ViolationCode_22-2-22', 'ViolationCode_22-2-43',
           'ViolationCode_22-2-45', 'ViolationCode_22-2-61',
           'ViolationCode_22-2-83', 'ViolationCode_22-2-88', 'ViolationCode_61-81',
           'ViolationCode_9-1-103', 'ViolationCode_9-1-104',
           'ViolationCode_9-1-105', 'ViolationCode_9-1-110',
           'ViolationCode_9-1-111', 'ViolationCode_9-1-113',
           'ViolationCode_9-1-36', 'ViolationCode_9-1-43', 'ViolationCode_9-1-45',
           'ViolationCode_9-1-50', 'ViolationCode_9-1-81', 'ViolationCode_9-1-82',
           'ViolationCode_OtherViolationCode', 'N_crimes',
           'CATEGORY_AGGRAVATED ASSAULT', 'CATEGORY_ARSON', 'CATEGORY_ASSAULT',
           'CATEGORY_BRIBERY', 'CATEGORY_BURGLARY', 'CATEGORY_DAMAGE TO PROPERTY',
           'CATEGORY_DANGEROUS DRUGS', 'CATEGORY_ESCAPE', 'CATEGORY_FRAUD',
           'CATEGORY_LARCENY', 'CATEGORY_OBSTRUCTING JUDICIARY',
           'CATEGORY_OtherCrimes',
           'CATEGORY_OUIL DISPOSE OF VEHICLE TO AVOID FORFEITURE',
           'CATEGORY_ROBBERY', 'CATEGORY_RUNAWAY', 'CATEGORY_SOLICITATION',
           'CATEGORY_STOLEN VEHICLE',
           'CATEGORY_TRAFFIC VIOLATIONS-DRIVING ON SUSPENDED',
           'CATEGORY_TRAFFIC VIOLATIONS-MOTORCYCLE VIOLATIONS',
           'CATEGORY_WEAPONS OFFENSES', 'N_calls', 'Max_Rating',
           'issue_type_Abandoned Vehicle', 'issue_type_Clogged Drain',
           'issue_type_Curbside Solid Waste Issue',
           'issue_type_DPW - Debris Removal', 'issue_type_Fire Hydrant Issue',
           'issue_type_Illegal Dumping / Illegal Dump Sites',
           'issue_type_Manhole Cover Issue', 'issue_type_OtherIssue',
           'issue_type_Potholes', 'issue_type_Running Water in a Home or Building',
           'issue_type_Street Light Pole Down', 'issue_type_Traffic Sign Issue',
           'issue_type_Traffic Signal Issue',
           'issue_type_Trash Issue - Bulk waste deposited more than 24 hours before designated time',
           'issue_type_Tree Issue', 'issue_type_Water Main Break', 'Lat', 'Lon'],
          dtype='object')



# Label  blight properties

In the permits list, we have transformed property parcel size into circle radius (in degrees). We will examine each cluster in new_list to see if it falls into any blight building range, if yes, then we label it as 'Y' for variable 'Blight', otherwise 'N'. 


```python
permit_df = pd.read_csv('permit_building_list.csv', index_col = 0)
```


```python
permit_df.head(2)
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>SITE_ADDRESS</th>
      <th>N_Permits</th>
      <th>PARCEL_RADIUS</th>
      <th>LAT</th>
      <th>LON</th>
      <th>PARCEL_DEGREE</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>1</th>
      <td>10 W PARKHURST</td>
      <td>1</td>
      <td>39.408552</td>
      <td>42.421046</td>
      <td>-83.102201</td>
      <td>0.000104</td>
    </tr>
    <tr>
      <th>2</th>
      <td>100 W. ALEXANDRINE</td>
      <td>1</td>
      <td>48.977375</td>
      <td>42.350312</td>
      <td>-83.060799</td>
      <td>0.000129</td>
    </tr>
  </tbody>
</table>
</div>




```python
new = new_list.loc[:,['Lat', 'Lon']].copy()
new.loc[:,'Blight'] = np.nan
```


```python
count = 0
for idx, row in new.iterrows():
    if np.isnan(row['Blight']):
        # compare the cluster center with the blight parcel degree range
        x = np.sqrt( np.square(row['Lat']-permit_df['LAT'])+ np.square(row['Lon']-permit_df['LON']) )-permit_df['PARCEL_DEGREE']
        for ix in x:
            if ix < 0.0001:
                # if the difference is within the precision 0.0001, then we label this cluster as Blight
                new.loc[idx,'Blight'] = 'Y'
                count = count+1
                break
print('Number of blight clusters: ' + str(count) )
```

    Number of blight clusters: 1920



```python
new.loc[new['Blight']!='Y', 'Blight'] = 'N'
new_list['Blight'] = new['Blight']
new_list.head(10)
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>N_Violations</th>
      <th>MaxTotalFee</th>
      <th>ViolationCode_22-2-17</th>
      <th>ViolationCode_22-2-22</th>
      <th>ViolationCode_22-2-43</th>
      <th>ViolationCode_22-2-45</th>
      <th>ViolationCode_22-2-61</th>
      <th>ViolationCode_22-2-83</th>
      <th>ViolationCode_22-2-88</th>
      <th>ViolationCode_61-81</th>
      <th>...</th>
      <th>issue_type_Running Water in a Home or Building</th>
      <th>issue_type_Street Light Pole Down</th>
      <th>issue_type_Traffic Sign Issue</th>
      <th>issue_type_Traffic Signal Issue</th>
      <th>issue_type_Trash Issue - Bulk waste deposited more than 24 hours before designated time</th>
      <th>issue_type_Tree Issue</th>
      <th>issue_type_Water Main Break</th>
      <th>Lat</th>
      <th>Lon</th>
      <th>Blight</th>
    </tr>
    <tr>
      <th>c_id</th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>146.0</td>
      <td>1162.391304</td>
      <td>59.0</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>13.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>42.369786</td>
      <td>-83.216326</td>
      <td>Y</td>
    </tr>
    <tr>
      <th>1</th>
      <td>20.0</td>
      <td>758.571429</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>42.325700</td>
      <td>-83.064300</td>
      <td>N</td>
    </tr>
    <tr>
      <th>2</th>
      <td>33.0</td>
      <td>4133.928571</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>3.0</td>
      <td>4.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>42.411997</td>
      <td>-83.167339</td>
      <td>Y</td>
    </tr>
    <tr>
      <th>3</th>
      <td>14.0</td>
      <td>1684.285714</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>42.441070</td>
      <td>-83.219404</td>
      <td>N</td>
    </tr>
    <tr>
      <th>4</th>
      <td>20.0</td>
      <td>1178.333333</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>42.341343</td>
      <td>-83.087699</td>
      <td>Y</td>
    </tr>
    <tr>
      <th>5</th>
      <td>8.0</td>
      <td>1600.000000</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>42.323726</td>
      <td>-83.081573</td>
      <td>Y</td>
    </tr>
    <tr>
      <th>6</th>
      <td>10.0</td>
      <td>866.666667</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>42.330432</td>
      <td>-83.088302</td>
      <td>Y</td>
    </tr>
    <tr>
      <th>7</th>
      <td>17.0</td>
      <td>703.000000</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>42.341719</td>
      <td>-83.114206</td>
      <td>N</td>
    </tr>
    <tr>
      <th>8</th>
      <td>4.0</td>
      <td>1196.666667</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>42.337700</td>
      <td>-83.112900</td>
      <td>Y</td>
    </tr>
    <tr>
      <th>9</th>
      <td>19.0</td>
      <td>1232.222222</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>4.0</td>
      <td>1.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>42.337800</td>
      <td>-83.114300</td>
      <td>N</td>
    </tr>
  </tbody>
</table>
<p>10 rows × 65 columns</p>
</div>




```python
new_list.to_csv('Data_for_modeling.csv', encoding='utf-8',index=False)
```


```python

```
