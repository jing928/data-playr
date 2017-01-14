library(ggmap)
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(tibble)
library(zipcode)

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
      ~Chinatown, ~Financial,
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
    mapdist(.$Chinatown, .$Financial, mode = "walking", output = "simple")
  ) %>% 
  arrange(km)

data("zipcode")

ct_ft_zips <- ct_ft %>% 
  gather(Area, zip, -city) %>% 
  left_join(zipcode %>% 
              select(-city),
            by = "zip") %>% 
  arrange(city)


map_area <- function(city, state, zoom, size) {
  ggmap(get_map(str_c(city, ", ", state), zoom = zoom, maptype = "toner-lite", source = "google")) +
    geom_point(data = ct_ft_zips %>% filter(state == state), 
               aes(x = longitude, y = latitude, color = Area), size = size) +
    labs(x = "Longitude", y = "Latitude")
}

# Boston
map_area("Boston", "MA", 14, 5)

# Chicago
map_area("Chicago", "IL", 13, 5)

# Houston
map_area("Houston", "TX", 11, 5)

# Los Angeles
map_area("Los Angeles", "CA", 13, 5)

# New York City
map_area("Lower Manhattan", "NY", 14, 5)

# Philadelphia
map_area("Philadelphia", "PA", 14, 5)

# San Francisco
map_area("San Francisco", "CA", 13, 5)

# Seattle
map_area("Seattle", "WA", 13, 5)

# Washington, D.C.
map_area("Washington", "DC", 14, 5)


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
