## @knitr setup
library(tidyverse)
library(stringr)

## @knitr import

movies_raw <- read_csv("../data-hub/data_analysis/imdb_movies/movie_metadata.csv")

## @knitr clean

movies_clean <- movies_raw %>% 
  mutate_if(is.character, str_trim) %>% 
  select(-movie_imdb_link)

## @knitr summary
