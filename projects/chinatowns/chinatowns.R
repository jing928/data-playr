library(ggmap)
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(tibble)

url <- "http://www.usatoday.com/story/travel/destinations/2014/03/08/chinatown-chinese-asian-food/6173601/"

usa_news <- read_html(url)
city <- usa_news %>% 
  html_nodes("b") %>% 
  html_text() %>% 
  tbl_df() %>% 
  filter(str_detect(value, "^\\d"), !str_detect(value, "Honolulu")) %>% 
  rename(city = value) %>% 
  mutate(city = str_replace(city, "^\\d{1,2}\\. ", ""))

ct_ft <- city %>% 
  bind_cols(
    tribble(
      ~zip_chinatown, ~zip_financial,
      "94108", "94111",
      "10013", "10038",
      "60616", "60605",
      "98104", "98104",
      "19107", "19103",
      "02111", "02110",
      "90012", "90071",
      "77036", "77002",
      "20001", "20433"
    )
  )

ct_ft_dist <- ct_ft %>% 
  bind_cols(
    ., 
    mapdist(.$zip_chinatown, .$zip_financial)
  )




# ct_fd <- city %>%
#   mutate(city = str_replace(city, "^\\d{1,2}\\. ", ""),
#          chinatown = str_c(city, " Chinatown"),
#          financial_district = str_c(city, " Financial District")) %>% 
#   gather(area, query_name, -city) %>% 
#   arrange(city)
#   
# 
# ct_fd_geo <- ct_fd %>% 
#   invoke_rows(function(query_name, ...) geocode(query_name, output = "more"), ., .to = "geo_code")
