library(randomForest)

# loading data
source('preprocessing.R')
# important features for the classification
importantFeatures = c('age', 'gender', 'occupation', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')

# loading data which will be used for model generation
modelData = data.userMovieRating[, importantFeatures, with = F]

# splitting data into 80% train set and 20% test set
ind = sample(2, nrow(modelData), replace=TRUE, prob=c(0.8, 0.2))
trainData = modelData[ind==1, ]
testData = modelData[ind==2, ]

# use all important data to predict the rating
formula = as.factor(rating) ~ age + gender + occupation + Action + Adventure + Animation + Children +   
  + Comedy + Crime + Documentary + Drama + Fantasy + FilmNoir + Horror +     
  + Musical + Mystery + Romance + SciFi + Thriller + War + Western

# build the random forest
rf = randomForest(formula, data = trainData, ntree = 100)

# stores true results for the test data
trueResults = as.factor(testData$rating)

# stores the predicted ratings by the previously created random forest rf
predicted = predict(rf, newdata = testData) 

# summarization of the classification
summarization = confusionMatrix(predicted, trueResults)

# printing results to file
sink(file = "out/randomForest/summarization.txt", type = "output")
cat("Random Forest summarization details:\n\n\n")
summarization
cat()
