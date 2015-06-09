library(data.table)
setwd('/home/goran/git/movieLens/movieLens/R')

data.movies = as.data.table(read.table('data/itemsData.txt', sep = '\t', header = T))

# reading all genres
data.genres = as.data.table(read.table('data/u.genre', sep = '|'))
setnames(data.genres, c('nameGenre', 'idGenre'))

data.movies = as.data.table(read.table('data/u.item', sep = '|', quote = ""))
# removing the columns in which all values are NA (in my case 4th column)
data.movies = data.movies[, colSums(is.na(data.movies))<nrow(data.movies), with = F]

# setting names of the columns in data.movies
setnames(data.movies, c('idMovie', 'nameMovie', 'dateRelease', 'imdbUrl', 
                        as.character(data.genres$nameGenre)))

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
# mean, median, variance, ...
summary(data.users)
summary(data.movies)
summary(data.ratings)

# shows histogram of users age
hist(data.users$age)

# shows histogram of ratings
hist(data.ratings$rating)


summary(data.users$age)
boxplot(data.users$age)

summary(data.ratings$rating)
boxplot(data.ratings$rating)

# merge users and ratings
data.userRating = merge(data.users, data.ratings, by = "idUser")

data.userMovieRating = merge(data.userRating, data.movies, by = "idMovie")


# removing outliers from users, based on the users age
age = data.users$age
age = sort(age)
lowerBound = quantile(age, 0.25)# 1st quartile
upperBound = quantile(age, 0.75)# 3rd quartile
ICR = upperBound - lowerBound # Inter Quartile Range
lowerBound = lowerBound - 1.5 * ICR # a user not to considered an outlier needs to have age
upperBound = upperBound + 1.5 * ICR # between the lowerBound and upperBound
withoutOutliers = subset(data.users, age > lowerBound && age < upperBound)

# norm.users stores the users with normalized age between [0, 1]
norm.users = data.users
norm.users$age = scale(data.users$age)

# normalizing users age
norm.users$age = (norm.users$age - min(norm.users$age)) / (max(norm.users$age) - min(norm.users$age))
norm.userRating = data.userRating
norm.userRating$age = (data.userRating$age - min(data.userRating$age)) / (max(data.userRating$age) - min(data.userRating$age))

norm.userMovieRating = data.userMovieRating
norm.userMovieRating$age = (data.userRating$age - min(data.userRating$age)) / (max(data.userRating$age) - min(data.userRating$age))
