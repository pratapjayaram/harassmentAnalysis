library(tidyverse)
library(sf)
library(RSocrata)
library(httr)
# library(tigris)

## PURPOSE: Isolate order maintenance arrests for all blocks in Brooklyn, associated to a segment of the LION street database

## Generate query for Brooklyn arrests between chosen dates
## Arrests API docs: https://dev.socrata.com/foundry/data.cityofnewyork.us/8h9b-rp9u
arrests_url <- "https://data.cityofnewyork.us/resource/8h9b-rp9u.json"
arrests_query <- paste("(arrest_boro = 'K'",
                       "arrest_date between '2019-06-14T00:00:00' and '2022-12-31T23:59:59')",
                       sep = " AND ")
arrests_request <- paste(arrests_url, arrests_query, sep = "?$where=")
arrests_response <- read.socrata(arrests_request)

## Filter to order maintenance arrests: prostitution, criminal mischief, offenses against the public order and public sensibilities, disorderly conduct, and theft of services
arrests_filtered <- arrests_response |> 
  filter(ofns_desc %in% c("PROSTITUTION & RELATED OFFENSES", "CRIMINAL MISCHIEF & RELATED OF", "OFF. AGNST PUB ORD SENSBLTY &", "DISORDERLY CONDUCT", "THEFT OF SERVICES"))

## Create sf with long/lat fields
arrests_xy <- arrests_filtered |> 
  select(arrest_key, arrest_date, ofns_desc, longitude, latitude) |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

## Spatial join xy data to street buffer, to see many arrests overlap multiple segments
streetbuff_arrests <- lion_buffer |>
  st_join(arrests_xy)
arrests_duped <- streetbuff_arrests |>
  st_drop_geometry() |>
  group_by(arrest_key) |>
  count() |>
  filter(n > 1)
arrests_duped_xy <- arrests_xy |>
  filter(arrest_key %in% arrests_duped$arrest_key) |>
  left_join(arrests_duped, by = "arrest_key")
st_write(arrests_duped_xy, "raw/arrests_duped_xy.shp", append=F)

## Spatial join LION street buffer to xy data, adding SegmentID column to xy dataframe
arrests_streetbuff <- arrests_xy |> 
  st_join(lion_buffer)

## Summarise arrests by SegmentID - resulting df will show # of arrests per block
arrests_summary <- arrests_streetbuff |> 
  st_drop_geometry() |> 
  group_by(SegmentID) |> 
  count()

## Left join arrest summary to street lines, by SegmentID field
arrests_blocks <- lion_bk_unique |> 
  select(SegmentID, Street, FromLeft, ToLeft, FromRight, ToRight, StreetWidth_Max, geometry) |> 
  left_join(arrests_summary, by = "SegmentID") |> 
  mutate(n = if_else(is.na(n), 0, n)) |> 
  st_transform(2263)

## Summarize arrests per census tract
arrests_tracts <- arrests_xy |> 
  st_join(select(bk_tracts, NAME)) |>
  st_drop_geometry() |> 
  group_by(NAME) |> 
  count() |> 
  right_join(bk_tracts, by = "NAME") |> 
  st_as_sf() |> 
  mutate(n = if_else(is.na(n), 0, n))



