install.packages('tidygeocoder')

library(tibble)
library(dplyr)
library(tidygeocoder)

setwd('D:/GBD_2020/ghs-smod/')

df <- read.csv("cities_latlon.csv")
rev1 <- df %>%
  reverse_geocode(lat = latitude, long = longitude, address = addr, method = "osm")

rev1 <- as.data.frame(rev1)
write.csv(rev1, 'rev.geocode.cities.csv')

rev2 <- df %>%
  reverse_geocode(lat = latitude, long = longitude, address = addr, method = "arcgis")

write.csv(rev2, 'rev.geocode.cities.arc.csv')


rev3 <- address_components %>%
  reverse_geocode(street = street, city = cty, state = st, method = "census")