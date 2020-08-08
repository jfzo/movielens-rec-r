# https://blog.rstudio.com/2019/11/06/renv-project-environments-for-r/
#
# Use renv::init() to initialize a project. 
# renv will discover the R packages used in your project, and install those packages into a private project library.
# Work in your project as usual, installing and upgrading R packages as required as your project evolves.

# Use renv::snapshot() to save the state of your project library. 
# The project state will be serialized into a file called renv.lock.

# Use renv::restore() to restore your project library from the state of your previously-created lockfile renv.lock.

# http://files.grouplens.org/datasets/movielens/ml-latest-small.zip

# download movie data and extrat into the working dir.
dl <- paste(tempfile(),".zip",sep = "")
download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", dl)
unzip(dl, list = TRUE) # shows the content
unzip(dl, exdir = getwd()) # extracts into the working dir.

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