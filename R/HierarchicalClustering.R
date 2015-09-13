library(cluster)
library(data.table) # storing data in data.table
library(fpc) # for cluster.stats method used for evaluation of clustering

setwd('/home/goran/git/movieLens/movieLens/R')

# loading data
itemsData = read.table('data/itemsData.txt', header = T, sep = '\t', quote = "")


# choosing the number of clusters
# in such a way that I am trying to minimize the within/between ration
# and because that ratio monotonically decreases, I decide to break when the
# decrease of that ratio is not significant 
# I am using the so called "Ellbow method" for choosing the number of clusters
# The elbow occurs at k = 11, so I am choosing later k = 11 to be the number of clusters
wb = rep(0, 20) # used to store within/between ratios

d = dist(itemsData[,-1], method = 'binary');# because the data which describes the movies is
# binary - belongs or not to a specific genr 1/0

# using average method
clustMovies = hclust(d, method = 'average');# doing the clustering

for(i in c(2:20)) {
  numClusters = i; # trying every value from 2 to 20 for number clusters k
  groups = cutree(clustMovies, k = numClusters);# assignment to a group for every movie
  
  # storing the withing/between ratios
  wb[i] = cluster.stats(d, groups)$wb
}

numClusters = c(2:20) # all numbers of cluster tried
withinBetweenRatio = wb[2:20] # all within/between ratios obtained

#the graphic which shows the decrease of within/between ratio as the number of clusters grows
# there are other options to break the agglomeration, but as long the elbow method is
# more popular for the k-means clustering, it is also applicable for the Hierarchical 
# Agglomerative Clustering
jpeg('out/hac/plots/elbowChooseNumOfClusters.jpg')
plot(numClusters, withinBetweenRatio)
lines(numClusters, withinBetweenRatio) 
dev.off()
numClusters = 11 # I chose it after analysing the graphic, that's where the elbow occurs

groups = cutree(clustMovies, k = numClusters) # cut the clustering tree, creating 11 clusters
moviesClustered = itemsData;
moviesClustered$clusterId = groups;# appended to moviesData the clusterId

# shows many properties of the clustering, such as between distance, within distance, etc.
stats = cluster.stats(d, groups)

# printing statistics for the clustering
sink(file = "out/hac/stats.txt", type = "output")
cat('Statistics for the hierarchical clustering, like betweeness, number of items in cluster etc.\n\n')
stats
sink()


# printing clusters to file
sink(file = "out/hac/clusters.txt")
for(i in c(1:numClusters)) {
  cat('Cluster ');
  cat(i);
  cat(':\n\n\n');
  m = subset(moviesClustered, clusterId == i)$Movie;
  cat(as.character(m), sep = '\n');
  cat('\n\n\n\n');
}
sink()





