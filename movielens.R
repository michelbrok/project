## ------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(forecast)
library(lubridate)
library(purrr)
library(tibble)
library(ggplot2)


## ------------------------------------------------------------------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
dl <- tempfile()
 download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
 colnames(movies) <- c("movieId", "title", "genres")
 movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                            title = as.character(title),
                                            genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


## ------------------------------------------------------------------------------------------------------------------------
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
 edx <- movielens[-test_index,]
 temp <- movielens[test_index,]
 validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")
 removed <- anti_join(temp, validation)
 edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ------------------------------------------------------------------------------------------------------------------------
edx %>% filter(rating == 0)
validation %>% filter(rating == 0)


## ------------------------------------------------------------------------------------------------------------------------
any(is.na(edx))


## ------------------------------------------------------------------------------------------------------------------------
str(edx)
dim(edx)
dim(validation)


## ------------------------------------------------------------------------------------------------------------------------
n_distinct(edx$userId)
n_distinct(edx$movieId)


## ------------------------------------------------------------------------------------------------------------------------
length(edx$rating)
summary(edx$rating)


## ------------------------------------------------------------------------------------------------------------------------
qplot(edx$rating, xlab = "Movie rating",  bins = 100)


## ------------------------------------------------------------------------------------------------------------------------
edx %>% 
count(movieId) %>% 
ggplot(aes(n)) +
geom_histogram(bins = 30, color = "black") +
scale_x_log10() +
ggtitle("Ratings per movie")


## ------------------------------------------------------------------------------------------------------------------------
edx %>% 
count(userId) %>% 
ggplot(aes(n)) +
geom_histogram(bins = 30, color = "black") +
scale_x_log10() +
ggtitle("Users giving ratings")


## ------------------------------------------------------------------------------------------------------------------------
edx %>% group_by(title) %>% summarise(number = n()) %>%
arrange(desc(number))


## ------------------------------------------------------------------------------------------------------------------------
avg <- edx %>% mutate(year = year(as_datetime(timestamp))) %>%
group_by(year) %>%
summarize(avg = mean(rating))
plot(avg)


## ------------------------------------------------------------------------------------------------------------------------
edx %>% group_by(genres) %>% summarize(avg_rating = mean(rating)) %>% arrange(desc(avg_rating))


## ------------------------------------------------------------------------------------------------------------------------
edx %>% group_by(genres) %>% summarize(sum = sum(rating)) %>% arrange(desc(sum))


## ------------------------------------------------------------------------------------------------------------------------
edx %>% mutate(week_of_day = weekdays(as_datetime(timestamp))) %>% group_by(week_of_day) %>% summarize(rating_count = n()) %>% ggplot(aes(week_of_day, rating_count)) + geom_point()


## ------------------------------------------------------------------------------------------------------------------------
edx %>% mutate(week_of_day = weekdays(as_datetime(timestamp))) %>% group_by(week_of_day) %>% summarize(rating_mean = mean(rating)) %>% ggplot(aes(week_of_day, rating_mean)) + geom_point()


## ------------------------------------------------------------------------------------------------------------------------
edx %>% mutate(hr_of_day = hour(as_datetime(timestamp))) %>% group_by(hr_of_day) %>% summarize(rating_count = n()) %>% ggplot(aes(hr_of_day, rating_count)) + geom_line()


## ------------------------------------------------------------------------------------------------------------------------
library(caret)
set.seed(1, sample.kind = "Rounding")
ind <- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx [-ind, ]
test_set <- edx[ind, ]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


## ------------------------------------------------------------------------------------------------------------------------
mu_hat <- mean(edx$rating)
mu_hat


## ------------------------------------------------------------------------------------------------------------------------
library(caret)
naive_rmse <- RMSE(train_set$rating, mu_hat)
naive_rmse
rmse_results <- tibble(method = "Average", RMSE = naive_rmse)
rmse_results


## ------------------------------------------------------------------------------------------------------------------------
RMSE <- function(true_ratings, predicted_ratings){sqrt(mean((true_ratings - predicted_ratings) ^2))
  }


## ------------------------------------------------------------------------------------------------------------------------
mu <- mean(train_set$rating)
movie_avg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
predict_movie1 <- test_set %>%
  left_join(movie_avg, by = 'movieId') %>%
  mutate(pred = (mu + b_i)) %>%
  pull(pred)
movie_rmse <- RMSE(predict_movie1, test_set$rating)
movie_results <- tibble(method = "Movie results", RMSE = movie_rmse)
movie_results


## ------------------------------------------------------------------------------------------------------------------------
user_avg1 <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu))
predict_user1 <- test_set %>%
  left_join(user_avg1, by = 'userId') %>%
  mutate(pred1 = (mu + b_u)) %>%
  pull(pred1)
user_rmse1 <- RMSE(predict_user1, test_set$rating)
movie_results1 <- tibble(method = "Users results", RMSE = user_rmse1)
movie_results1


## ------------------------------------------------------------------------------------------------------------------------
genre_avg <- train_set %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu))
predict_genre <- test_set %>%
  left_join(genre_avg, by = 'genres') %>%
  mutate(pred2 = (mu + b_g)) %>%
  pull(pred2)
genres_rmse <- RMSE(predict_genre, test_set$rating)
genre_results <- tibble(method = "Genre results", RMSE = genres_rmse)
genre_results


## ------------------------------------------------------------------------------------------------------------------------
wkday_avg <- train_set %>%
  mutate(wkday = weekdays(as_datetime(timestamp))) %>%
  group_by(wkday) %>%
  summarize(b_w = mean(rating - mu))
predict_wkday <- test_set %>%
  mutate(wkday = weekdays(as_datetime(timestamp))) %>%
  left_join(wkday_avg, by = 'wkday') %>%
  mutate(pred3 = (mu + b_w)) %>%
  pull(pred3)
wkday_rmse <- RMSE(predict_wkday, test_set$rating)
wkday_results <- tibble(method = "wkday results", RMSE = wkday_rmse)
wkday_results


## ------------------------------------------------------------------------------------------------------------------------
bind_rows(movie_results, movie_results1, genre_results, wkday_results)


## ------------------------------------------------------------------------------------------------------------------------
user_avg <- train_set %>%
  left_join(movie_avg, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
  
  pred_users <- test_set %>%
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
user_movie_rmse <- RMSE(pred_users, test_set$rating)
user_movie_results <- tibble(method = "User_movie results", RMSE = user_movie_rmse)
user_movie_results


## ------------------------------------------------------------------------------------------------------------------------
genre_movie_avg <- train_set %>%
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

 pred_genre_movie <- test_set %>%
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  left_join(genre_avg, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
 genre_movie_rmse <- RMSE(pred_genre_movie, test_set$rating)
  genre_movie_results <- tibble(method = "genre_movies_results", RMSE = genre_movie_rmse)
  genre_movie_results


## ------------------------------------------------------------------------------------------------------------------------
genre_movie_users_time <- train_set %>%
  left_join(movie_avg, by = 'movieId') %>% 
  left_join(user_avg, by = 'userId') %>%
  left_join(genre_avg, by = 'genres') %>%
  mutate(wkday = weekdays(as_datetime(timestamp))) %>%
  group_by(wkday) %>%
  summarize(b_w = mean(rating - b_g - mu - b_i - b_u))

pred_genre_movie_users_time <- test_set %>%
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  left_join(genre_avg, by = 'genres') %>%
  mutate(wkday = weekdays(as_datetime(timestamp))) %>%
  left_join(wkday_avg, by = 'wkday') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_w) %>%
    pull(pred)
 genre_movie_users_time_rmse <- RMSE(pred_genre_movie_users_time, test_set$rating)
  genre_movie_users_time_results <- tibble(method = "genre_movies_results", RMSE = genre_movie_users_time_rmse)
  genre_movie_users_time_results


## ------------------------------------------------------------------------------------------------------------------------
bind_rows(movie_results, user_movie_results, genre_movie_results, genre_movie_users_time_results)


## ------------------------------------------------------------------------------------------------------------------------
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()
movie_avg %>% 
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  slice(1:10) %>%
  pull(title)


## ------------------------------------------------------------------------------------------------------------------------
movie_avg %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>%
  slice(1:10) %>%
  pull(title)


## ------------------------------------------------------------------------------------------------------------------------
train_set %>%
  count(movieId) %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  slice(1:10) %>%
  pull(n)


## ------------------------------------------------------------------------------------------------------------------------
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(lambda){
  
mu <- mean(train_set$rating)
  
# adjust mean by movie effect and penalize for low numbers:
  b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# adjust mean by user and movie effect and penalize for low numbers:
  b_u <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)


## ------------------------------------------------------------------------------------------------------------------------
lambda <- 5

mu <- mean(edx$rating)
  
movie_effect <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

user_effect <- edx %>%
  left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu) / (n()+lambda))

fit <- validation %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

final_rmse <- RMSE(validation$rating, fit)
final_rmse
final_rmse <- tibble(method = "Final_RMSE_score", RMSE = final_rmse)


## ------------------------------------------------------------------------------------------------------------------------
options(pillar.sigfig = 5)
bind_rows(movie_results, user_movie_results, final_rmse)

