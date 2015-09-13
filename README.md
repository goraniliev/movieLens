In the .R files in the project R I tried different Data Mining algorithms to create some models which could be used for classification, clustering, etc. The data is downloaded from the popular movie lens data set: http://grouplens.org/datasets/movielens/. There is one Python script in the Python folder which produces one additional file from the given files. All the other files are taken from the above mentioned link. 

In R/preprocessing.R I am loading the data which will be used in most of the other scripts. There I produce two groups of tables: ones in which I load the raw data read from the files and others which contain normalized data.

In bayes.R I create and evaluate a Naive Bayes Classificator to predict movie ratings. All data produced by this script is stored in out/bayes folder.

In KNN.R I create and evaluate a K Nearest Neighbors Classificator to predict ratings for movies. I used normalized data. The data produced is stored in out/KNN folder.

In decisionTree.R Decision Tree is built to predict the rating for movies. Output data is stored in out/decisionTree.

In randomForest.R a Random Forest is created instead of Decision Tree, thus the overfitting is lower.

In logitRegression.R I use Logistic Regression (using Neural Network) to predict the ratings.

In Kmeans.R k-means algorithm is used to cluster movies based on the genres they belong to. Firstly I tried clustering for several k values ( in the interval [2, 20] ) and then using the Elbow rule I chose a value for the number of clusters k.

In HierarchicalClustering.R I used Hierarchical Clustering to cluster the movies by the genres they belong to.
