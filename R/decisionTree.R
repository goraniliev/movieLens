# loading data
source('preprocessing.R')
library(party)

importantFeatures = c('age', 'gender', 'occupation', as.character(data.genres[2:nrow(data.genres)]$nameGenre), 'rating')

modelData = data.userMovieRating[, importantFeatures, with = F]
head(modelData)

# splitting data into 80% for train and 20% for test
ind = sample(2, nrow(modelData), replace=TRUE, prob=c(0.8, 0.2))
trainData = modelData[ind==1, ]
testData = modelData[ind==2, ]

dim(trainData)
dim(testData)

# model will be created to predict rating, and all other features will be used to predict the rating
formula = as.factor(rating) ~ age + gender + occupation + Action + Adventure + Animation + Children +   
  + Comedy + Crime + Documentary + Drama + Fantasy + FilmNoir + Horror +     
  + Musical + Mystery + Romance + SciFi + Thriller + War + Western

# build the decision tree
moviesCtree = ctree(formula, data = trainData)

# the real results taken from the test data labels
trueResults = as.factor(testData$rating)

# valeus predicted by the decision tree
predicted = predict(moviesCtree, newdata = testData) 

# shows contingency table, for each row R and column C the value contingencyTable[R, C] 
# shows how many ratins with value R have been classified as rating C
contingencyTable = table(predicted, trueResults)

summarization = confusionMatrix(predicted, trueResults)

# writing summaries to out/decisionTree/summarization.txt
sink(file = "out/decisionTree/summarization.txt", type = "output")
cat('Contingency table shows how many movies with rating C (the column name) have been classified as R (row name) rating')
cat('\n\n')

cat('Summarization shows how accurate the decision tree is (measured on the test set) and other details:')
cat('\n')
summarization
sink()