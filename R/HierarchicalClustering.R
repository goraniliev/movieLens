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
graphic = lines(numClusters, withinBetweenRatio) 

numClusters = 10 # I chose it after analysing the graphic, that's where the elbow occurs

groups = cutree(clustMovies, k = numClusters) # cut the clustering tree, creating 10 clusters
moviesClustered = itemsData;
moviesClustered$clusterId = groups;# appended to moviesData the clusterId

# shows many properties of the clustering, such as between distance, within distance, etc.
cluster.stats(d, groups)

# checking some cluster
# after checking on IMDB I realized that the 2nd cluster contains movies
# with gres mistery, thriler or crime (mostly)
head(subset(moviesClustered, clusterId == 2))$Movie



