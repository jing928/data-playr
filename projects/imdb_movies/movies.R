## @knitr setup
library(tidyverse)
library(stringr)
library(ggplot2)
library(assertr)
library(caret)
library(rpart)
library(rpart.plot)

## @knitr import

movies_raw <- read_csv("../data-hub/data_analysis/imdb_movies/movie_metadata.csv", 
                       col_types = cols(budget = "d", gross = "d"))

## @knitr clean

movies_clean <- movies_raw %>% 
  mutate_if(is.character, str_trim) %>% 
  select(-movie_imdb_link)

## @knitr summary
# USA only
movies_usa <- movies_clean %>% 
  filter(country %in% "USA") %>% 
  mutate_at(vars(gross, budget), function(x) x / 1000000)

movies_usa %>% 
  ggplot(aes(x = imdb_score, y = gross)) +
  geom_point() +
  theme_light()

movies_usa %>% 
  ggplot(aes(imdb_score)) +
  geom_histogram() +
  theme_light()

movies_usa %>% 
  ggplot(aes(gross)) +
  geom_histogram() +
  theme_light()

movies_usa %>% 
  ggplot(aes(duration)) +
  geom_histogram() +
  theme_light()

movies_usa %>% 
  ggplot(aes(x = duration, y = gross)) +
  geom_point() +
  theme_light()

movies_usa %>% 
  ggplot(aes(x = num_voted_users, y = imdb_score)) +
  geom_point() +
  theme_light()

movies_usa %>% 
  ggplot(aes(x = budget, y = gross)) +
  geom_point() +
  theme_light()

movies_usa %>% 
  ggplot(aes(x = budget, y = gross)) +
  geom_point() +
  theme_light()

movies_usa %>% 
  mutate(cast_total_facebook_likes = cast_total_facebook_likes / 1000) %>% 
  ggplot(aes(x = cast_total_facebook_likes, y = gross)) +
  geom_point() +
  theme_light()

movies_usa %>% 
  ggplot(aes(x = facenumber_in_poster, y = gross)) +
  geom_point() +
  theme_light()

movies_usa %>% 
  ggplot(aes(x = facenumber_in_poster, y = imdb_score)) +
  geom_point() +
  theme_light()

## @knitr trend

# average imdb_score over the years
movies_usa %>% 
  group_by(title_year) %>% 
  summarise(avg_imdb_score = mean(imdb_score)) %>% 
  ggplot(aes(title_year, avg_imdb_score)) +
  geom_line() +
  theme_light()
movies_usa %>% 
  mutate(num_voted_users = num_voted_users / 1000) %>% 
  group_by(title_year) %>% 
  summarise(avg_num_of_votes = mean(num_voted_users)) %>% 
  ggplot(aes(title_year, avg_num_of_votes)) +
  geom_line() +
  theme_light()

# facenumber over the years
movies_usa %>% 
  group_by(title_year) %>% 
  summarise(avg_facenumber = mean(facenumber_in_poster)) %>% 
  ggplot(aes(title_year, avg_facenumber)) +
  geom_line() +
  theme_light()


## @knitr decision_tree_score

# Select variables and cut score
movies_usa_tree <- movies_usa %>% 
  select(imdb_score,title_year, director_name, duration, num_critic_for_reviews, gross, genres, movie_title, num_voted_users,
         facenumber_in_poster, content_rating, budget, aspect_ratio) %>% 
  filter(complete.cases(.)) %>% 
  distinct(movie_title, title_year, director_name, .keep_all = TRUE) %>% 
  mutate(movie_grade = cut(imdb_score, breaks = c(0, 4, 6, 8, 10), labels = c("D", "C", "B", "A"))) %>% 
  separate_rows(genres, sep = "\\|") %>% 
  mutate(is_genre = TRUE,
         genres = str_replace(genres, "-", "_")) %>% 
  group_by(movie_title, title_year, director_name) %>% 
  do(spread(., genres, is_genre)) %>% 
  ungroup() %>% 
  mutate_at(vars(Drama:Film_Noir), function(col) coalesce(col, FALSE)) %>% 
  mutate_at(vars(content_rating, aspect_ratio), as.factor) %>% 
  assert(not_na, movie_grade) 

# 80-20 split training and testing
set.seed(6)
sample <- createDataPartition(movies_usa_tree$movie_grade, p = 0.8, list = FALSE)
movies_train <- movies_usa_tree[sample, ] %>% 
  select(-imdb_score, -director_name, -movie_title)
movies_test <- movies_usa_tree[-sample, ] %>% 
  select(-imdb_score, -director_name, -movie_title)

# run model

tree_param <- rpart.control()
movies_rpart <- rpart(formula = movie_grade ~ ., 
                      data = movies_train, control = tree_param)

rpart.plot(movies_rpart)





