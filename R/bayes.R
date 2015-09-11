# loading preprocessed data
setwd('/home/goran/git/movieLens/movieLens/R')
source('preprocessing.R')

# important features for classifications (ids, names are dropped)
# also I dropped unknown genre because it has 0 variance and doesn't effect the results
importantFeatures = c('gender', 'occupation', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')


# model.data contains only the data relevant for classification
model.data = data.userMovieRating[, importantFeatures, with = F]

model.data.genres = data.userMovieRating[, c(as.character(data.genres[2:nrow(data.genres)]$nameGenre)), with = F]

# required for the data partitioning and building the models
library(lattice)
library(caret)
library(e1071)
library(klaR)

# define an 80%/20% train/test split of the dataset
splitIndex = createDataPartition(model.data$rating, p=0.8, list=FALSE)
data_train = model.data[splitIndex[,1],]
data_test = model.data[-splitIndex[,1],]

# train a naive bayes model
model = NaiveBayes(as.factor(rating) ~., data=data_train)
# make predictions
predictions <- predict(model, data_test[,-length(data_test), with = F])
# summarize results
summarization = confusionMatrix(predictions$class, data_test$rating)
sink(file="out/bayes/summarization.txt", type="output") 
summarization
sink() 

