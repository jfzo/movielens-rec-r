# https://blog.rstudio.com/2019/11/06/renv-project-environments-for-r/
#
# Use renv::init() to initialize a project. 
# renv will discover the R packages used in your project, and install those packages into a private project library.
# Work in your project as usual, installing and upgrading R packages as required as your project evolves.

# Use renv::snapshot() to save the state of your project library. 
# The project state will be serialized into a file called renv.lock.

# Use renv::restore() to restore your project library from the state of your previously-created lockfile renv.lock.

# http://files.grouplens.org/datasets/movielens/ml-latest-small.zip

library(tidyr)
library(scales)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(caret)
library(lubridate)


# download movie data and extrat into the working dir.
dl <- paste(tempfile(),".zip",sep = "")
download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", dl)
unzip(dl, list = TRUE) # shows the content
unzip(dl, exdir = getwd()) # extracts into the working dir.
rm(dl)

########################
rm(list = ls())

## Reading the data
ratings <- read.csv("ml-latest-small/ratings.csv")
movies <- read.csv("ml-latest-small/movies.csv")
tags <- read.csv("ml-latest-small/tags.csv")


# joining all the data
movielens <- left_join(ratings, movies, by = "movieId")

# 'Validation' set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in 'validation' set are also in 'edx' set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from 'validation' set back into 'edx' set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(test_index, temp, movielens, removed)


# The edx set is used for training and testing, and the validation set is used 
# for final validation to simulate the new data.
# we split the edx set in 2 parts: the training set and the test set.

#
# The model building is done in the training set, and the test set is used to 
# test the model. When the model is complete,
# we use the validation set to calculate the final RMSE. 
# We use the same procedure used to create edx and validation sets.


# The training set will be 90% of edx data and the test set will be the remaining 10%.

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)


rm(test_index, temp, removed)


##################
# Some data exploration
# getting initial, final dates and the total period in years.
tibble(`Initial Date` = date(as_datetime(min(edx$timestamp), origin="1970-01-01")),
       `Final Date` = date(as_datetime(max(edx$timestamp), origin="1970-01-01"))) %>%
  mutate(Period = duration(max(edx$timestamp)-min(edx$timestamp)))


# number of ratings per year
edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "white") + 
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma) + 
  theme_fivethirtyeight()

# scale goes from 0 to 5 in steps of 0.5
edx %>% group_by(rating) %>% summarize(n=n())

edx %>% group_by(rating) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_line() +
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Rating Distribution", subtitle = "Higher ratings are prevalent.") + 
  xlab("Rating") +
  ylab("Count") +
  theme_fivethirtyeight()

# movies: 9724
# nr of ratings per movie. 
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies", 
          subtitle = "Most movies have less than 1k ratings.") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") + 
  theme_fivethirtyeight()


# users: 610
# ratings per user
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  head()


# nr of ratings made per user 
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Users", 
          subtitle="The distribution is right skewed.") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  scale_y_continuous(labels = comma) + 
  theme_fivethirtyeight()


# To visualize this sparse association between users and rated movies
users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")
title("User x Movie Matrix")

#
# The random prediction model
#

# First, we calculate the probability of each rating in the training set, 
# then we predict the rating for the test set and compare with actual rating.

# Since the training set is a sample of the entire population and we don’t know 
# the real distribution of ratings, the Monte Carlo simulation with replacement 
# provides a good approximation of the rating distribution.

# Define Mean Absolute Error (MAE)
MAE <- function(true_ratings, predicted_ratings){
  mean(abs(true_ratings - predicted_ratings))
}

# Define Mean Squared Error (MSE)
MSE <- function(true_ratings, predicted_ratings){
  mean((true_ratings - predicted_ratings)^2)
}

# Define Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

set.seed(4321, sample.kind = "Rounding")

# Create the probability of each rating
p <- function(x, y) mean(y == x)

rating <- seq(0.5,5,0.5)

# Estimate the probability of each rating with Monte Carlo simulation
B <- 10^3
M <- replicate(B, {
  s <- sample(train_set$rating, 100, replace = TRUE)
  sapply(rating, p, y= s)
})
prob <- sapply(1:nrow(M), function(x) mean(M[x,]))

# Predict random ratings
y_hat_random <- sample(rating, size = nrow(test_set), 
                       replace = TRUE, prob = prob)

# Create a table with the error results
#result <- tibble(Method = "Project Goal", RMSE = 0.8649, MSE = NA, MAE = NA)
result <- bind_rows(#result, 
                    tibble(Method = "Random prediction", 
                           RMSE = RMSE(test_set$rating, y_hat_random),
                           MSE  = MSE(test_set$rating, y_hat_random),
                           MAE  = MAE(test_set$rating, y_hat_random)))


#
# Linear model: $\hat{y}=\mu + b_i + b_u + e_{u,i}$
# 

# 'Mean model'
# model0: $\hat{y}=\mu + e_{u,i}$
# Mean of observed values
mu <- mean(train_set$rating)

# Update the error table  
result <- bind_rows(result, 
                    tibble(Method = "Mean", 
                           RMSE = RMSE(test_set$rating, mu),
                           MSE  = MSE(test_set$rating, mu),
                           MAE  = MAE(test_set$rating, mu)))
# Show the RMSE improvement  
result

edx %>% head %>% separate(genres, NA, sep = "|")



# 'movie effect'. Considering movie rating bias (some are more popular than others).
# model1: $\hat{y}=\mu + b_i + e_{u,i}$
#
# Movie effects (bi)
bi <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

head(bi)

# visualization of the movie effect skewness
bi %>% ggplot(aes(x = b_i)) + 
  geom_histogram(bins=10, col = I("black")) +
  ggtitle("Movie Effect Distribution") +
  xlab("Movie effect") +
  ylab("Count") +
  scale_y_continuous(labels = comma) + 
  theme_fivethirtyeight()


# Predict the rating with mean + bi  
y_hat_bi <- mu + (test_set %>% 
  left_join(bi, by = "movieId") %>% 
  .$b_i)

# Calculate the RMSE  
result <- bind_rows(result, 
                    tibble(Method = "Mean + bi", 
                           RMSE = RMSE(test_set$rating, y_hat_bi),
                           MSE  = MSE(test_set$rating, y_hat_bi),
                           MAE  = MAE(test_set$rating, y_hat_bi)))

# Show the RMSE improvement  
result

#
# Considering user bias.
# model2: User effect. $\hat{y}=\mu + b_i + b_u + e_{u,i}$
#
# predict the rating with mean + bi + bu

# User effect (bu)
bu <- train_set %>% 
  left_join(bi, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Prediction
y_hat_bi_bu <- test_set %>% 
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Update the results table
result <- bind_rows(result, 
                    tibble(Method = "Mean + bi + bu", 
                           RMSE = RMSE(test_set$rating, y_hat_bi_bu),
                           MSE  = MSE(test_set$rating, y_hat_bi_bu),
                           MAE  = MAE(test_set$rating, y_hat_bi_bu)))

# Show the RMSE improvement  
result

##
# Also the genre bias could be considered.
##

#
# distribution of user effect

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(color = "black") + 
  ggtitle("User Effect Distribution") +
  xlab("User Bias") +
  ylab("Count") +
  scale_y_continuous(labels = comma) + 
  theme_economist()


## Separando columnas
edx  %>% separate_rows(genres, sep = "\\|")