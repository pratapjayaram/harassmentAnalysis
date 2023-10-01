library(tidyverse)
library(RSocrata)

## PURPOSE: Isolate evictions for all properties in Brooklyn, associated to a BBL

## Generate query for Brooklyn residential evictions between chosen dates
## Evictions API docs: https://dev.socrata.com/foundry/data.cityofnewyork.us/6z8x-wfk4
evictions_url <- "https://data.cityofnewyork.us/resource/6z8x-wfk4.csv"
evictions_query <- paste("(borough = 'BROOKLYN'",
                        "residential_commercial_ind = 'Residential'",
                        "executed_date between '2019-06-14T00:00:00' and '2022-12-31T23:59:59')",
                        sep = " AND ")
evictions_request <- paste(evictions_url, evictions_query, sep = "?$where=")
evictions_response <- read.socrata(evictions_request)

## Create summary dataframe
evictions_summary <- evictions_response |> 
  mutate(bbl = as.numeric(bbl)) |> 
  group_by(bbl) |> 
  count() |> 
  rename(evictions = `n`)

## Create sf with long/lat fields
evictions_xy <- evictions_summary |> 
  left_join(evictions_response, by = "bbl", multiple = "any") |> 
  select(bbl, n, longitude, latitude) |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)