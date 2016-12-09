## @knitr setup
library(tidyverse)
library(stringr)
library(ggplot2)

## @knitr import

movies_raw <- read_csv("../data-hub/data_analysis/imdb_movies/movie_metadata.csv", col_types = cols(budget = "d", gross = "d"))

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


