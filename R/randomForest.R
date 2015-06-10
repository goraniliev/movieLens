library(randomForest)

source('preprocessing.R')
importantFeatures = c('age', 'gender', 'occupation', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')
modelData = data.userMovieRating[, importantFeatures, with = F]
head(modelData)

ind = sample(2, nrow(modelData), replace=TRUE, prob=c(0.7, 0.3))
trainData = modelData[ind==1, ]
testData = modelData[ind==2, ]

dim(trainData)
dim(testData)

formula = as.factor(rating) ~ age + gender + occupation + Action + Adventure + Animation + Children +   
  + Comedy + Crime + Documentary + Drama + Fantasy + FilmNoir + Horror +     
  + Musical + Mystery + Romance + SciFi + Thriller + War + Western

rf = randomForest(formula, data = trainData, ntree = 100)

#resp = treeresponse(moviesCtree, newdata = testData)

trueResults = as.factor(testData$rating)

predicted = predict(rf, newdata = testData) 
table(predicted, trueResults)

summarization = confusionMatrix(predicted, trueResults)
