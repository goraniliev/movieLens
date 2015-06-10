library(cluster)
library(data.table) # storing data in data.table
library(fpc) # for cluster.stats method used for evaluation of clustering

# loading data
itemsData = read.table('data/itemsData.txt', header = T, sep = '\t', quote = "")


# choosing the number of clusters
# in such a way that I am trying to minimize the within/between ration
# and because that ratio monotonically decreases, I decide to break when the
# decrease of that ratio is not significant 
# I am using the so called "Ellbow method" for choosing the number of clusters
# The elbow occurs at k = 10, so I am choosing later k = 10 to be the number of clusters
wb = rep(0, 20) # used to store within/between ratios

d = dist(itemsData[,-1], method = 'binary');# because the data which describes the movies is
# binary - belongs or not to a specific genr 1/0

for(i in c(2:20)) {
  numClusters = i; # trying every value from 2 to 20 for number clusters k
  
  clustMovies = kmeans(itemsData[,-1], centers = numClusters);# doing the clustering
  # storing the withing/between ratios
  wb[i] = clustMovies$tot.withinss / clustMovies$betweenss
}

numClusters = c(2:20) # all numbers of cluster tried
withinBetweenRatio = wb[2:20] # all within/between ratios obtained

#the graphic which shows the decrease of within/between ratio as the number of clusters grows
lines(numClusters, withinBetweenRatio) 

k = 5 # I chose it after analysing the graphic, that's where the elbow occurs

clustMovies = kmeans(itemsData[,-1], centers = k)

groups = clustMovies$cluster# cut the clustering tree, creating 10 clusters
moviesClustered = itemsData;
moviesClustered$clusterId = groups;# appended to moviesData the clusterId

# shows many properties of the clustering, such as between distance, within distance, etc.
cluster.stats(d, groups)

# getting some movies from the 1st cluster
# I checked on IMDB and this is a cluster of the comedies
head(subset(moviesClustered, clusterId == 2))$Movie
