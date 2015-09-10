# data is being stored in data.table
library(data.table)

# needs to be changed to the corresponding folder on the machine on which will be run
setwd('/home/goran/git/movieLens/movieLens/R')

# reading all genres
data.genres = as.data.table(read.table('data/u.genre', sep = '|'))
setnames(data.genres, c('nameGenre', 'idGenre'))

# contains information about each movie, from which genres binary data is most important for
# the manipulations which will be done
data.movies = as.data.table(read.table('data/u.item', sep = '|', quote = ""))
# removing the columns in which all values are NA (in my case 4th column)
data.movies = data.movies[, colSums(is.na(data.movies))<nrow(data.movies), with = F]

# setting names of the columns in data.movies
setnames(data.movies, c('idMovie', 'nameMovie', 'dateRelease', 'imdbUrl', 
                        as.character(data.genres$nameGenre)))

# stores data about each user
data.users = as.data.table(read.table('data/u.user', sep = '|'))
# removing 5th row, because I had no idea how to use ZIP code for the future algorithms
data.users = data.users[, -5, with = F]
setnames(data.users, c('idUser', 'age', 'gender', 'occupation'))

#loading ratings given by users
data.ratings = as.data.table(read.table('data/u.data', sep = '\t'))
# deleting 4th column which represents the time when the movie was rated by that user
data.ratings = data.ratings[, -4, with = F]
#setting column names
setnames(data.ratings, c('idUser', 'idMovie', 'rating'))

# make ratings floats
data.ratings$rating = as.numeric(data.ratings$rating)


# mean, median, variance, ...

sink(file="out/preprocessing/preprocessing.txt", type="output") 
cat('\tBasic summaries for all read data tables before normalization\n\n\n')
cat('Users data summary:\n\n')
summary(data.users)
cat('\n\n')
cat('Movies data summary:\n\n')
summary(data.movies)
cat('\n\n')
cat('Ratings summary:\n\n')
summary(data.ratings)
cat('\n\n#############################################################################################')
sink() 

# Creating images of important histograms

# shows histogram of users age
jpeg('out/preprocessing/plots/usersAgeHist.jpg')
hist(data.users$age, main=paste('Histogram of users \' age'), xlab='age', ylab='num. users')
dev.off()

# shows histogram of ratings
jpeg('out/preprocessing/plots/ratingsHist.jpg')
hist(data.ratings$rating, main=paste('Histogram of rating distribution'), xlab='rating value', ylab='num. of users')
dev.off()

# show age box plot
jpeg('out/preprocessing/plots/ageBoxplot.jpg')
summary(data.users$age)
boxplot(data.users$age, main='Age boxplot', ylab='Age')
dev.off()

# show rating box-plot
jpeg('out/preprocessing/plots/ratingBoxplot.jpg')
summary(data.ratings$rating)
boxplot(data.ratings$rating, main='Rating box-plot', ylab='rating')
dev.off()

# merge users and ratings
data.userRating = merge(data.users, data.ratings, by = "idUser")

# merge userRating with movie to have actual movie titles instead of ids
data.userMovieRating = merge(data.userRating, data.movies, by = "idMovie")


# removing outliers from users, based on the users age
age = data.users$age
age = sort(age)
lowerBound = quantile(age, 0.25)# 1st quartile
upperBound = quantile(age, 0.75)# 3rd quartile
ICR = upperBound - lowerBound # Inter Quartile Range
lowerBound = lowerBound - 1.5 * ICR # a user not to considered an outlier needs to have age
upperBound = upperBound + 1.5 * ICR # between the lowerBound and upperBound
# Remove all users whose age doesn't belong in the interval 
# [lowerBound - 1.5 * ICR, upperBound + 1.5 * ICR]
withoutOutliers = subset(data.users, age > lowerBound && age < upperBound)

# Instead of working with raw data, sometimes it is useful to convert all different
# attributes to be on same scale
# I will scale all attribute values to fall in the interval [0, 1]

# norm.users stores the users with normalized age between [0, 1]
norm.users = data.users

# normalizing users age
norm.users$age = (norm.users$age - min(norm.users$age)) / (max(norm.users$age) - min(norm.users$age))
norm.userRating = data.userRating
norm.userRating$age = (data.userRating$age - min(data.userRating$age)) / (max(data.userRating$age) - min(data.userRating$age))
norm.userRating$rating = (data.userRating$rating - min(data.userRating$rating)) / (max(data.userRating$rating) - min(data.userRating$rating))

norm.userMovieRating = data.userMovieRating
norm.userMovieRating$age = (data.userMovieRating$age - min(data.userMovieRating$age)) / (max(data.userMovieRating$age) - min(data.userMovieRating$age))
norm.userMovieRating$rating = (data.userMovieRating$rating - min(data.userMovieRating$rating)) / (max(data.userMovieRating$rating) - min(data.userMovieRating$rating))

# Normalized data summary and plots

sink(file="out/preprocessing/preprocessingNormalized.txt", type="output") 
cat('\n\n\tBasic summaries for all read norm tables after normalization\n\n\n')
cat('Normalized users norm summary:\n\n')
summary(norm.userRating$age)
cat('\n\n')

cat('Normalized ratings summary:\n\n')
summary(norm.userRating$rating)
cat('\n\n#############################################################################################')
sink() 

# shows histogram of users age normalized
jpeg('out/preprocessing/plots/usersAgeNormHist.jpg')
hist(norm.userRating$age, main=paste('Histogram of normalized users \' age'), xlab='age', ylab='num. users')
dev.off()

# shows histogram of ratings
jpeg('out/preprocessing/plots/ratingsNormHist.jpg')
hist(norm.userRating$rating, main=paste('Histogram of normalized rating distribution'), xlab='rating value', ylab='num. of users')
dev.off()

# show age box plot
jpeg('out/preprocessing/plots/ageNormBoxplot.jpg')
summary(norm.userRating$age)
boxplot(norm.userRating$age, main='Normalized age boxplot', ylab='Age')
dev.off()

# show rating box-plot
jpeg('out/preprocessing/plots/ratingNormBoxplot.jpg')
summary(norm.userRating$rating)
boxplot(norm.userRating$rating, main='Normalized rating box-plot', ylab='Rating')
dev.off()

jpeg('out/preprocessing/plots/ratingAgeScatterPlot.jpg')
plot(data.userRating$age, data.userRating$rating)
dev.off()

