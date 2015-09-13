library(cluster)
library(data.table) # storing data in data.table
library(fpc) # for cluster.stats method used for evaluation of clustering

# needs to be changed to the corresponding folder on the machine on which will be run
setwd('/home/goran/git/movieLens/movieLens/R')

# loading data
# itemsData.txt was not originally present in the data I downloaded
#I produced this file, because u.item contained several unimportant features for the data mining techniques
itemsData = read.table('data/itemsData.txt', header = T, sep = '\t', quote = "")


# choosing the number of clusters
# in such a way that I am trying to minimize the within/between ratio
# and because that ratio monotonically decreases (mostly), I decide to break when the
# decrease of that ratio is not significant 
# I am using the so called "Ellbow method" for choosing the number of clusters
# The elbow occurs at k = 10 (after that even an increase of the ratio occurs, 
# probably because of randomness), 
# so I am choosing later k = 10 to be the number of clusters
# I could choose k=6, but it has higher values arount itself, 
# so it maybe gives low values because of randomness and in another scenario could give higher ratio

wb = rep(0, 20) # used to store within/between ratios

d = dist(itemsData[,-1], method = 'binary');# because the data which describes the movies is
                                          # binary - belongs or not to a specific genre 1/0

for(i in c(2:20)) {
  numClusters = i; # trying every value from 2 to 20 for number of clusters k
  
  clustMovies = kmeans(itemsData[,-1], centers = numClusters);# doing the clustering
  # storing the withing/between ratios
  wb[i] = clustMovies$tot.withinss / clustMovies$betweenss
}

numClusters = c(2:20) # all numbers of cluster tried
withinBetweenRatio = wb[2:20] # all within/between ratios obtained

#the graphic which shows the decrease/increase of within/between ratio as the number of clusters grows
jpeg('out/kMeans/plots/elbowChooseNumOfClusters.jpg')
plot(numClusters, withinBetweenRatio, xlab = "K", ylab = "Within/Between Ratio for this K")
lines(numClusters, withinBetweenRatio)
dev.off()

k = 10 # I chose it after analysing the graphic, that's where the elbow occurs

# clustering with the chosen value of k
clustMovies = kmeans(itemsData[,-1], centers = k)

groups = clustMovies$cluster# cut the clustering tree, creating 10 clusters
moviesClustered = itemsData;
moviesClustered$clusterId = groups;# appended to moviesData the clusterId

# shows many properties of the clustering, such as between distance, within distance, etc.
stats = cluster.stats(d, groups)

# printing statistics for the clustering
sink(file = "out/kMeans/stats.txt", type = "output")
cat('Statistics for the k-means clustering, like betweeness, number of items in cluster etc.\n\n')
stats
sink()

# getting some movies from the 1st cluster
# I checked on IMDB and this is a cluster of the comedies

# printing clusters to file
sink(file = "out/kMeans/clusters.txt")
for(i in c(1:k)) {
  cat('Cluster ');
  cat(i);
  cat(':\n\n\n');
  m = subset(moviesClustered, clusterId == i)$Movie;
  cat(as.character(m), sep = '\n');
  cat('\n\n\n\n');
}
sink()

