source('preprocessing.R')

head(norm.userMovieRating)
importantFeatures = c('age', 'gender', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')

modelData = norm.userMovieRating[, importantFeatures, with = F]
# converting male and female to 1 and 2 before applyin KNN
modelData$gender = as.numeric(modelData$gender)

ind = sample(2, nrow(modelData), replace=TRUE, prob=c(0.7, 0.3))
trainData = modelData[ind==1, ]
testData = modelData[ind==2, ]

formula = as.factor(rating) ~ age + gender +  Action + Adventure + Animation + Children +
  + Comedy + Crime + Documentary + Drama + Fantasy + FilmNoir + Horror +     
  + Musical + Mystery + Romance + SciFi + Thriller + War + Western

logit = glm(formula, data = trainData, family = "binomial")
summary(logit)

predicted = predict(logit, newdata = testData)
trueResults = testData$rating
summarization = confusionMatrix(as.factor(round(predicted)), as.factor(trueResults))







# new

install.packages('mlogit')
library(mlogit)

?mlogit
multiLogit = mlogit(formula, data = trainData)
formula
head(trainData)

