library(caret)
setwd('/home/goran/git/movieLens/movieLens/R')
source('preprocessing.R')

head(data.userMovieRating)

head(norm.userMovieRating)

head(norm.userMovieRating)

# age, gender, genres are the important features for predicting rating
# ids, names aren't important for the KNN classifier
important.features = c('age', 'gender', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')

# using normalized data because this KNN works better with scaled data
# that way, every attribute has same impact, which may not always what we actually want
# for future impovement one could use weighted KNN and some optimization techniques to determine the 
# best weights
model.data = norm.userMovieRating[, important.features, with = F]
# converting male and female to 1 and 2 before applying KNN (because KNN runs on numeric data)
model.data$gender = as.numeric(model.data$gender)

# define an 80%/20% train/test split of the dataset
model.data$rating = as.numeric(data.userMovieRating$rating)
train.index <- createDataPartition(model.data$rating, p=0.80, list=FALSE)
data_train <- model.data[train.index[,1],]
data_test <- model.data[-train.index[,1],]
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
bestAcc = 0
kValues = seq(1, 75, by = 5)
acc = vector(mode = 'numeric', length(kValues))

# choosing best value of k
for(kt in kValues) {
  model <- knn(train = dTrain, test = dTest, cl = cl, k = kt)
  # summarization of the results
  summarization = confusionMatrix(model, as.factor(as.matrix(trueResults)))
  accId = (kt - 1) / 5 + 1
  acc[accId] = summarization$overall['Accuracy']
  if(acc[accId] > bestAcc) {
    bestAcc = acc[accId]
    bestK = kt
  }
  cat(sprintf('%s %s\n', kt, accId))
}

plot(kValues, acc)
lines(kValues, acc)

acc

# after printing the acc values which indicate the accuracy of different values for k
# and seeing the plot, from the tested value for k = 71 I achieve best accuracy
# However the value for k = 46 the accuracy is also preetty good 
# So I choose value of k = 46, because in that way fewer movies will be taken into account to predict
# the rating

model <- knn(train = dTrain, test = dTest, cl = cl, k = bestK)# 46 chosen

# summarization of the results
summarization = confusionMatrix(model, as.factor(as.matrix(trueResults)))

# writing the summary to file
sink(file = "out/knn/summarization.txt", type = "output")
summarization
sink()
