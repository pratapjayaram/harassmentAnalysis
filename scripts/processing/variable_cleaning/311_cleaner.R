library(tidyverse)
library(sf)
library(RSocrata)
library(httr)
# library(tigris)

## PURPOSE: Isolate 311 calls directed to NYPD for all blocks in Brooklyn

## Generate query for NYPD-directed 311 calls in Brooklyn between chosen dates
## 311 API docs: https://dev.socrata.com/foundry/data.cityofnewyork.us/erm2-nwe9
nypd311_url <- "https://data.cityofnewyork.us/resource/erm2-nwe9.json"
nypd311_query <- paste("(agency = 'NYPD'",
                       "borough = 'BROOKLYN'",
                       "location is not null",
                       "created_date between '2019-06-14T00:00:00' and '2022-12-31T23:59:59')",
                       sep = " AND ")
nypd311_request <- paste(nypd311_url, nypd311_query, sep = "?$where=")
nypd311_response <- read.socrata(nypd311_request)

## Create sf with long/lat fields
nypd311_xy <- nypd311_response |> 
  select(unique_key, complaint_type, descriptor, longitude, latitude) |>
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

## Spatial join LION street buffer to xy data, adding SegmentID column to xy dataframe
nypd311_streetbuff <- nypd311_xy |> 
  st_join(lion_buffer)

## Summarise calls by SegmentID - resulting df will show # of calls per block
nypd311_summary <- nypd311_streetbuff |> 
  st_drop_geometry() |> 
  group_by(SegmentID) |> 
  count()

## Left join arrest summary to street lines, by SegmentID field
nypd311_blocks <- lion_bk_unique |> 
  select(SegmentID, Street, FromLeft, ToLeft, FromRight, ToRight, StreetWidth_Max, geometry) |> 
  left_join(nypd311_summary, by = "SegmentID") |> 
  mutate(n = if_else(is.na(n), 0, n)) |> 
  st_transform(2263)

## Summarize calls per census tract
nypd311_tracts <- nypd311_xy |> 
  st_join(select(bk_tracts, NAME)) |>
  st_drop_geometry() |> 
  group_by(NAME) |> 
  count() |> 
  right_join(bk_tracts, by = "NAME") |> 
  st_as_sf() |> 
  mutate(n = if_else(is.na(n), 0, n))


