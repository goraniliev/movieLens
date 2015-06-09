# loading preprocessed data
setwd('/home/goran/git/movieLens/movieLens/R')
source('preprocessing.R')

# important features for classifications (ids, names are dropped)
# also I dropped unknown genre because it has 0 variacne and doesn't effect the results
importantFeatures = c('gender', 'occupation', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')


# modelData contains only the data relevant for classification
modelData = norm.userMovieRating[, importantFeatures, with = F]

modelDataGenres = norm.userMovieRating[, c(as.character(data.genres[2:nrow(data.genres)]$nameGenre)), with = F]
modelDataAfterPCA = prcomp(modelDataGenres)

# required for the data partitioning and building the models
library(caret)
library(e1071)
library(klaR)
#library(FSelector)
# define an 80%/20% train/test split of the dataset
splitIndex = createDataPartition(modelData$rating, p=0.8, list=FALSE)
data_train = modelData[splitIndex[,1],]
data_test = modelData[-splitIndex[,1],]
# train a naive bayes model
#model <- NaiveBayes(as.factor(rating) ~., data=data_train)

model = NaiveBayes(as.factor(rating) ~., data=data_train)
# make predictions
predictions <- predict(model, data_test[,-length(data_test), with = F])
# summarize results
summarization = confusionMatrix(predictions$class, data_test$rating)
