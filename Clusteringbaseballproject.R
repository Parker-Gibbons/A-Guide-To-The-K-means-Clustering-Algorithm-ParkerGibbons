# Clustering Analysis for Baseball Project

# Load necessary libraries
library(ClusterR)      # For clustering functions
library(cluster)       # For additional clustering metrics
library(factoextra)    # For visualization of clusters

# Import the dataset
mlbdata <- read.csv("~/Downloads/mlbdata.csv")

# Subset the dataset for relevant clustering variables
# Selecting payroll data across multiple years, city population, market size, team valuation, and attendance metrics
subsetclust1 <- mlbdata[, c("X23.payroll", "X22.payroll", "X21.payroll", "X20.payroll", 
                            "City.Population", "media.market.size", "Team.valuation", 
                            "X23.attendence.g","X22.attendence.g","X21.attendence.g")]

# Finding the Optimal Number of Clusters Using Elbow Method
# The Elbow Method calculates the total within-cluster sum of squares (WSS) for different cluster counts
# and suggests the optimal number where the rate of WSS reduction slows significantly
fviz_nbclust(
  subsetclust1,
  kmeans,
  method = "wss",      # "wss" stands for "within-cluster sum of squares"
  k.max = 25,          # Max number of clusters to evaluate
  verbose = FALSE
)

# Finding the Optimal Number of Clusters Using Average Silhouette Method
# The Average Silhouette Method suggests the number of clusters where silhouette width is maximized
fviz_nbclust(
  subsetclust1,
  kmeans,
  method = "silhouette",
  k.max = 25           # Maximum number of clusters to consider
)

# Finding the Optimal Number of Clusters Using Gap Statistic Method
# The Gap Statistic compares WSS values against expected WSS under null reference distribution
set.seed(123)          # Setting seed for reproducibility in randomization
gap_stat <- clusGap(subsetclust1, FUN = kmeans, nstart = 20, K.max = 25, B = 50)
print(gap_stat)

# Visualize the gap statistic to determine the optimal number of clusters
fviz_gap_stat(gap_stat)

# Fitting K-Means Clustering Model
# Based on the above methods, we choose an optimal cluster count (e.g., 3 clusters in this example)
set.seed(240)          # Setting seed to ensure reproducibility
kmeans.re <- kmeans(subsetclust1, centers = 3, nstart = 20) # 3 clusters, 20 initializations
kmeans.re

# Adding Cluster Assignments to the Original Dataset
# This allows us to analyze each data point with its assigned cluster
Data2 <- mlbdata
Data2$subsetclust1 <- kmeans.re$cluster  # Adding cluster assignments as a new column

# Cluster Identification for Each Observation
# Outputs the assigned cluster for each data point
kmeans.re$cluster

# Visualizing the Clustered Data
# Using clusplot to plot two variables, payroll for 2023 and city population
# Adjust labels, shading, and colors for clarity in visualization
y_kmeans <- kmeans.re$cluster
clusplot(subsetclust1[, c("X23.payroll", "City.Population")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         plotchar = FALSE,
         span = TRUE,
         main = "Cluster Groupings",
         xlab = 'X23.payroll',
         ylab = 'City.Population')


# Export the Data with Cluster Information
# Save the dataset with cluster numbers to a CSV file for further analysis
write.csv(Data2, file = "CLUSTEREDmlb.csv")

# Calculating Total Within-Cluster Variation
# This metric indicates how tightly points are clustered within each cluster
total_within_cluster_variation <- kmeans.re$tot.withinss
print(total_within_cluster_variation)
