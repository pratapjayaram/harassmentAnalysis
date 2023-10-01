library(tidyverse)
library(sf)
library(RSocrata)
library(tigris)

## PURPOSE: Isolate DOHMH indoor environmental complaints for all properties in Brooklyn, associated to a BBL

## Generate query for Brooklyn residential DOHMH between chosen dates
## DOHMH API docs: https://dev.socrata.com/foundry/data.cityofnewyork.us/9jgj-bmct
dohmh_url <- "https://data.cityofnewyork.us/resource/9jgj-bmct.json"
dohmh_query <- paste("(incident_address_4 = 'Brooklyn'",
                     "date_received between '2019-06-14T00:00:00' and '2022-12-31T23:59:59')",
                     sep = " AND ")
dohmh_request <- paste(dohmh_url, dohmh_query, sep = "?$where=")
dohmh_response <- read.socrata(dohmh_request)

## Create summary dataframe
dohmh_summary <- dohmh_response |> 
  mutate(bbl = as.numeric(bbl)) |> 
  group_by(bbl) |> 
  count() |> 
  filter(!is.na(bbl)) |> 
  rename(dohmh = n)

## Create sf with long/lat fields
dohmh_xy <- dohmh_summary |> 
  left_join(dohmh_response, by = "bbl", multiple = "any") |> 
  select(bbl, n, longitude, latitude) |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

## Trim outliers with Brooklyn shapefile
bk <- county_subdivisions(state = "New York",
                          county = "Kings",
                          class = "sf",
                          progress_bar = FALSE) |> 
  st_transform(4326)
dohmh_xy <- dohmh_xy |> 
  st_intersection(bk)
