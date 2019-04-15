# Hierarchical Cluster Analysis

##Loading libraries

library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

## Use built-in dataset in R called USArrests
data(USArrests) 

df <- USArrests

## To remove any missing value that might be present in the data
df <- na.omit(df)

## Scaling/standardizing the data to not to depend on arbitrary variable.
df <- scale(df)
head(df)

## Agglomerative Hierarchical Clustering

## Dissimilarity matrix
d <- dist(df, method = "euclidean")

## Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

## Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

## Alternatively we can use the agnes function to get the agglomerative cofficient
## Compute with agnes
hc2 <- agnes(df, method = "complete")
hc_quiz_wt <- agnes(df, method = "weighted")

## Agglomerative coefficient
hc2$ac
hc_quiz_wt$ac

## Plot the obtained dendrogram
plot(hc_quiz_wt, cex = 0.6, hang = -1)
cutree(hc_quiz_wt, h = 10)

## methods to assess and to identify stronger clustering structures
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
## function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}
map_dbl(m, ac)

## From the four methods, ward identifies the strongest clustering structure of the four methods assessed

## Visualizing the dendogram

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

## Divisive Hierarchical Clustering

## compute divisive hierarchical clustering
hc4 <- diana(df)
## Divise coefficient; amount of clustering structure found
hc4$dc

## plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

## Identifying sub-groups (clusters)

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)
# Number of members in each cluster
table(sub_grp)

##Add cluster column to see which cluster does an obs. belong to

USArrests %>%
  mutate(cluster = sub_grp) %>%
  head

##Drawing the dendogram with a border around the clusters

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

##use the fviz_cluster function from the factoextra package to
##visualize the result in a scatter plot.

fviz_cluster(list(data = df, cluster = sub_grp))

##Using cutree with agnes and diana

# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)
# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 4)

##Comparing the two dendograms with the function tanglegram

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

##Customizing the output of tangleram

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

##Determing Optimal Clusters

# Elbow Method
fviz_nbclust(df, FUN = hcut, method = "wss")

# Average Silhouette Method
fviz_nbclust(df, FUN = hcut, method = "silhouette")

# Gap Statistic Method
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# K-means Cluster Analysis

## Use built-in dataset in R called USArrests
data(USArrests) 

df <- USArrests

## To remove any missing value that might be present in the data
df <- na.omit(df)

## Scaling/standardizing the data to not to depend on arbitrary variable.
df <- scale(df)
head(df)

## Compute and visualize the distance matrix
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## Runnung kmeans with nstart option
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

k2$cluster
k2$centers
k2$totss
k2$size
k2$withinss
k2$tot.withinss
k2$betweenss

## Viewing clustering results
fviz_cluster(k2, data = df)

## standard pairwise scatter plots
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

## Use several different values of k and test the differences in the results
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

## plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

## Elbow method to determine the optimal clusters

### Process 1

set.seed(123)

## function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

## Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

## extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

### Process 2

set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

## Average Silhouette Method

### Process 1

## function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

## Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

## extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

### Process 2

fviz_nbclust(df, kmeans, method = "silhouette")

## Gap Statistic Method

### Process 1
## compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
## Print the result
print(gap_stat, method = "firstmax")

### Process 2
fviz_gap_stat(gap_stat)

## Extracting Results

## With most of these approaches suggesting 4 as the number of optimal clusters, we can perform the ô???f¦nal analysis and extract the results using 4 clusters.

## Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)

## Visualize results
fviz_cluster(final, data = df)

## Descriptive Statistics at the cluster level
USArrests %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")