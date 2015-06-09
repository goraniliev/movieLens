library(caret)
setwd('/home/goran/git/movieLens/movieLens/R')
source('preprocessing.R')

head(data.userMovieRating)

head(norm.userMovieRating)

head(norm.userMovieRating)
importantFeatures = c('age', 'gender', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')

modelData = norm.userMovieRating[, importantFeatures, with = F]
# converting male and female to 1 and 2 before applyin KNN
modelData$gender = as.numeric(modelData$gender)

# define an 80%/20% train/test split of the dataset
trainIndex <- createDataPartition(modelData$rating, p=0.80, list=FALSE)
data_train <- modelData[trainIndex[,1],]
data_test <- modelData[-trainIndex[,1],]
# train a naive bayes model
#model <- NaiveBayes(as.factor(rating) ~., data=data_train)
allClasses = c(1, 2, 3, 4, 5)

# split data into train and test sets
dTrain = data_train[, -length(data_train), with = F]
dTest = data_test[, -length(data_test), with = F]
# actual results of the train set
cl = as.factor(as.matrix(data_train[, length(data_train), with = F]))
cl.test = cl

# true results for the test set
trueResults = data_test[, length(data_test), with = F]

library('e1071')
library(klaR)
library(class)
# create the model
bestK = 1
acc = rep(15, 0)
bestAcc = 0
kValues = seq(1, 15, by = 1)
for(kt in kValues) {
  print(kt)
  model <- knn(train = dTrain, test = dTest, cl = cl, k = kt)
  # summarization of the results
  summarization = confusionMatrix(model, as.factor(as.matrix(trueResults)))
  acc[kt] = summarization$overall['Accuracy']
  if(acc[kt] > bestAcc) {
    bestAcc = acc[kt]
    bestK = kt
  }
}

plot(acc, kValues)
lines(acc, kValues)


model <- knn(train = dTrain, test = dTest, cl = cl, k = bestK)# 6 chosen
# summarization of the results
summarization = confusionMatrix(model, as.factor(as.matrix(trueResults)))
