source('preprocessing.R')

head(norm.userMovieRating)
importantFeatures = c('age', 'gender', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')

# choose important feature to create the model data
modelData = norm.userMovieRating[, importantFeatures, with = F]
# converting male and female to 1 and 2 before applyin KNN
modelData$gender = as.numeric(modelData$gender)

# ratings should not be normalized
modelData$rating = data.userMovieRating$rating

# splitting data into 80% train and 20% test set
ind = sample(2, nrow(modelData), replace=TRUE, prob=c(0.8, 0.2))
trainData = modelData[ind==1, ]
testData = modelData[ind==2, ]

# use the different features to predict the rating
formula = rating ~ age + gender +  Action + Adventure + Animation + Children +
  + Comedy + Crime + Documentary + Drama + Fantasy + FilmNoir + Horror +     
  + Musical + Mystery + Romance + SciFi + Thriller + War + Western


library(nnet)
# create multinom regression model
# Fits multinomial log-linear models via neural networks.
ml = multinom(formula, data = trainData)

# make predictions on the test data
predicted = predict(ml, newdata = testData)
# store true results
trueResults = testData$rating

# store summary for the created model
summarization = confusionMatrix(as.factor(predicted), as.factor(trueResults))
# writing summarization to file
sink(file = "out/logitRegression/summarization.txt", type = "output")
cat("Logistic Regression Summarization:\n\n\n")
summarization
sink()