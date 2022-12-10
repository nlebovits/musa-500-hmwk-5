---
title: "K-Means Clustering"
subtitle: "MUSA 500, Homework #5"
author: "Minwook Kang, Nissim Lebovits, and Ann Zhang"
date: today
format: 
  html:
    toc: true
    theme: lux
    code-fold: true
    code-summary: "Show the code"
editor: visual
execute:
  cache: true
  warning: false
  error: false
  messages: false
---


## Introduction

Clustering analysis helps us to identify clustering in a dataset. In exploring data, it is particularly helpful for partitioning data into smaller groups for further investigation.

In this assignment, we use k-means clustering, which is widely applied to larger datasets and numeric variables. Here, we examine the same dataset that we used in Homeworks #1 and #2. K-means partitions the dataset into a number $k$ of overlapping clusters, $k$ being predetermined by the researchers. Our dataset includes five variables: MEDHVAL, MEDHHINC, PCTBACHMOR, PCTSINGLES, and PCTVACANT. K-means clustering will help us to identify if there is any clustering in the aggregated effect of these variables. Furthermore, we will examine whether the clustering of these variables is spatially autocorrelated.

By applying k-means clustering, we can answer questions like, How do we categorize the homeowners in question? How many classes are we able to identify from the data, and what are their characteristics? Incorporating spatial data, we can answer questions like, Are specific classes spatially clustered? Can we leverage our understanding of these clusters to create more equitable city policy?


::: {.cell hash='musa-500-hmwk-5_cache/html/data and package import_e60e22511e874b610e975897dc3c9180'}

```{.r .cell-code}
#install.packages(c("NbClust","flexclust"))

library(NbClust)
library(flexclust)
library(dplyr)
library(kableExtra)
library(sf)
```
:::


## Methods

### 1. K-Means Algorithm

First, reseachers deterine the intended number ($k$) of clusters. There are various methods for selecting a best value for $k$. In R, the `NbClust` package helps. In this project, we will utilize this package to generate a scree plot of the sum of squared errors (SSE) within each group, which will yield the ideal value for $k$. 

Next, we iteratively improve cluster assignment until there is no change in clustering membership (i.e., each data point is assigned to a particular clustering center without further change), or until a set number of iterations. This process involves:\
1. Identify $k$ data points as centers for each cluster (usually randomly)
2. Assign each data point to a cluster by calculating its Euclidian distance from the cluster centers and choosing the closest center
3. Given the assigned clusters with data points, recalculate new centers for each cluster, then repeat from step #1. If there is no change, or if you have reached the pre-set number of iterations, stop here.

In this assignment, since we are looking at five different variables, we ensure that all data operate at the sme scale before proceeding to k-means clustering and analysis.

### 2. Limitations of K-Means

1. As mentioned above, k-means clustering requires researchers to specify the number of clusters in advance. However, there are methods to find the ideal $k$, such as the SSE scree plot we mentioned earlier.
2. K-means clustering is technically only for sizable sets of numeric data, although some use the method for binary data as well.
3. K-means clustering assumes clusters to be of the same size (i.e., the same number of data points are assigned to each cluster/cluster center) and same density (which impacts the distance to nearest cluster centers). Additionally, it assumes that clusters are globular. Any deviance from these assumptions may lead to problems when implementing k-means clustering.
4. Because each data point must be assigned to a cluster, the k-means approach struggles to accommodate noise or outliers.
5. K-means might attempt to find local minimums instead of a global minimum, leading to incorrect results.


### 3. Other Available Clustering Algorithms

Other clustering methods include hierarchical clustering and density-based clustering (DBSCAN). In comparison to the other two, though, k-means appears to be the most appropriate choice in our context. Hierarchical clustering is better suited to smaller datasets, ideally under 100. Since this dataset contains 1,720 observations, it is too big for hierarchical clustering. There is no reason why DBSCAN is more or less appropriate in this case.

## Results

### **1.  NbClust & SSE Scree Plot: Optimal Number of Clusters (k)**

The first scree plot shows the SSE of each number of clusters from 1 to 20. In the graph below, there is a distinct drop in the SSE when the number of clusters moves from 1 to 3. After four clusters, the rate of decrease drops off, suggesting that a 3-cluster solution is likely to be a good option for our data.

Given NbClust's ability to use different methods to seek the ideal number of clusters, we have also plotted the number of clusters suggested using 26 criteria. From this plot, we can see that 2 and 3 clusters are most frequently suggested, yet in this situation, having only 2 clusters will not help with further analysis. Hence, our choice of 3 clusters seem appropriate.


::: {.cell hash='musa-500-hmwk-5_cache/html/scree plot and criteria_ba858b6596a64c8c1f90958e1cfcec52'}

```{.r .cell-code}
regression <- read.csv("https://github.com/nlebovits/musa-500-hmwk-5/raw/main/data/RegressionData.csv") 
data <- read.csv("https://github.com/nlebovits/musa-500-hmwk-5/raw/main/data/RegressionData.csv") 
data <- data %>% select(-POLY_ID, -AREAKEY) 
data <- scale(data)

data_see <- (nrow(data)-1)*sum(apply(data,2,var))

#nrow(data)-1
#apply(data,2,var)

kmeans(data, centers=2)
for (i in 2:20) data_see[i] <- sum(kmeans(data, centers=i)$withinss)

#data_see

plot(1:20, data_see, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares erorrs")
```

::: {.cell-output-display}
![](musa-500-hmwk-5_files/figure-html/scree plot and criteria-1.png){width=672}
:::

```{.r .cell-code}
set.seed(100)
nc <- NbClust(data, min.nc=2, max.nc=15, method="kmeans", index="all")
```

::: {.cell-output-display}
![](musa-500-hmwk-5_files/figure-html/scree plot and criteria-2.png){width=672}
:::

::: {.cell-output-display}
![](musa-500-hmwk-5_files/figure-html/scree plot and criteria-3.png){width=672}
:::

```{.r .cell-code}
#table(nc$Best.n[1,])

par(mfrow=c(1,1)) 
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
```

::: {.cell-output-display}
![](musa-500-hmwk-5_files/figure-html/scree plot and criteria-4.png){width=672}
:::
:::


### **2. Three Clusters, Five Variables**


::: {.cell hash='musa-500-hmwk-5_cache/html/aggregated value_44a181fb22e365fc9be20d22959ff24b'}

```{.r .cell-code}
set.seed(1234)
fit.km <- kmeans(data, 3, nstart=1000)

fit.km$size
round(fit.km$centers, 2)
fit.km$cluster

cluster_info = as.data.frame(fit.km$cluster)

table <- cbind(round(aggregate(data, by=list(cluster=fit.km$cluster), mean),1),fit.km$size)
 
colnames(table)[1] = c("CLUSTER")
colnames(table)[8] = c("SIZE")
```
:::

::: {.cell hash='musa-500-hmwk-5_cache/html/aggregated value2_a7236bc639051873a62e8af232a5ea5f'}

```{.r .cell-code}
table <- table[, c(1, 8, 2, 3, 4, 5, 6, 7)]
  
table %>%
  kbl() %>%
  kable_paper("hover", full_width = F)
```

::: {.cell-output-display}
`````{=html}
<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:right;"> CLUSTER </th>
   <th style="text-align:right;"> SIZE </th>
   <th style="text-align:right;"> MEDHVAL </th>
   <th style="text-align:right;"> PCTBACHMOR </th>
   <th style="text-align:right;"> MEDHHINC </th>
   <th style="text-align:right;"> PCTVACANT </th>
   <th style="text-align:right;"> PCTSINGLES </th>
   <th style="text-align:right;"> NBELPOV100 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 651 </td>
   <td style="text-align:right;"> -0.5 </td>
   <td style="text-align:right;"> -0.5 </td>
   <td style="text-align:right;"> -0.7 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> -0.2 </td>
   <td style="text-align:right;"> 0.7 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 159 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> -0.7 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> -0.6 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 910 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> -0.5 </td>
   <td style="text-align:right;"> -0.1 </td>
   <td style="text-align:right;"> -0.4 </td>
  </tr>
</tbody>
</table>

`````
:::
:::



Overall, the clusters makes sense across the 5 variables. We can see that Group 1 has the lowest house value, percentage of bachelor's degree, median household income, and percentage of single family houses, as well as the highest percentage of vacant house and poverty rate out of all cluster group. Group 2 is the opposite, and Group 3 falls in between. The clustering shows the socioeconomic characteristics of each group; taking everything into consideration, we came up with the following group names:

- Group 1: Lower Income
- Group 2: Bougie Community
- Group 3: Middle Class

### **3. Spatial Distribution of Clusters** 


::: {.cell hash='musa-500-hmwk-5_cache/html/Spatial Distribution of Clusters_9e0a344fed67620118ba0c96385feff9'}

```{.r .cell-code}
Regression_cluster <- cbind(regression, cluster_info)

regression_shp <- st_read('C:/Users/Nissim/Desktop/Fall 2022/Spat Stats/Homeworks/ass_1_data_shp/RegressionData.shp') 

#regression_shp <- st_read("/Users/annzhang/Downloads/HW 5/RegressionData.shp") 

regression_shp <- left_join(regression_shp, Regression_cluster, by = "POLY_ID") %>% st_as_sf()

regression_cluster <- regression_shp %>% mutate(cluster = fit.km$cluster) %>% dplyr::select(cluster)

plot(regression_cluster)
```

::: {.cell-output-display}
![](musa-500-hmwk-5_files/figure-html/Spatial Distribution of Clusters-1.png){width=672}
:::
:::


By plotting the three groups on a map (as shown above), we can see some spatial autocorrelation. There seems to be spatial clustering of Group 2 (higher income, more educated communities) in some parts of Center City, the northwestern part of the city, and some parts of the northeast, and clustering of Group 1 (lower income) in the area north to the center city and some parts of West Philly. Since the clustering areas are not concentrated, it is difficult to rename the clusters by geographic locations.

## Discussion

To sum up, we used k-means clustering on 5 variables and created three cluster groups representing three socioeconomic groups and their respective house value. The clustering does a relatively good job of identifying clusters. Although none of the findings are surprising, the map in result section examines the spatial clustering of the cluster groups we created shows a spatial autocorrelation.

