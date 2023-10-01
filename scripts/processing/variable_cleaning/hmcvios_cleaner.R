library(tidyverse)
library(RSocrata)

## PURPOSE: Isolate hmcomplaints for all properties in Brooklyn, associated to a BBL

## Generate query for Brooklyn Housing Maintenance Code complaints between chosen dates
## hmcomplaints API docs: https://dev.socrata.com/foundry/data.cityofnewyork.us/uwyv-629c
hmcomplaints_url <- "https://data.cityofnewyork.us/resource/uwyv-629c.json"
hmcomplaints_query <- paste("(borough = 'BROOKLYN'",
                            "receiveddate between '2019-06-14T00:00:00' and '2022-12-31T23:59:59')",
                            sep = " AND ")
hmcomplaints_request <- paste(hmcomplaints_url, hmcomplaints_query, sep = "?$where=")
hmcomplaints_response <- read.socrata(hmcomplaints_request)

## Create summary dataframe
hmcomplaints_summary <- hmcomplaints_response |> 
  mutate(bbl = if_else(is.na(lot), NA, 3000000000 + 10000*as.numeric(block) + as.numeric(lot))) |> 
  filter(!is.na(bbl)) |> 
  group_by(bbl) |> 
  count() |> 
  rename(hmcomplaints = n)

## Create sf with long/lat fields
hmcomplaints_xy <- hmcomplaints_summary |> 
  left_join(hmcomplaints_response, by = "bbl", multiple = "any") |> 
  select(bbl, n, longitude, latitude) |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)