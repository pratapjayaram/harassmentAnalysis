library(tidyverse)
library(RSocrata)

## PURPOSE: Isolate HP actions for all properties in Brooklyn, associated to a BBL

## Generate query for Brooklyn cases between chosen dates
## HP Actions API docs: https://dev.socrata.com/foundry/data.cityofnewyork.us/59kj-x8nc
hpaction_url <- "https://data.cityofnewyork.us/resource/59kj-x8nc.csv"
hpaction_query <- paste("(boroid=3",
                        "(caseopendate between '2019-06-14T00:00:00' and '2022-12-31T23:59:59'))",
                        sep = " AND ")
hpaction_request <- paste(hpaction_url, hpaction_query, sep = "?$where=")
hpactions_response <- read.socrata(hpaction_request)

## Apply filters for case type, harassment finding, and case status
hpactions_cleaned <- hpactions_response |> 
  filter(casetype %in% c("Tenant Action/Harrassment", "Heat and Hot Water", "Tenant Action"),
         # findingofharassment != "No Harassment",
         casestatus == "CLOSED")

## Create summary dataframe
hpactions_summary <- hpactions_cleaned |> 
  mutate(bbl = as.numeric(bbl)) |> 
  group_by(bbl) |> 
  count() |> 
  mutate(harassOccurred = if_else(n > 0, T, F)) |> 
  select(bbl, harassOccurred)

## Create sf with long/lat fields
hpactions_xy <- hpactions_summary |> 
  left_join(hpactions_cleaned, by = "bbl", multiple = "any") |> 
  select(bbl, n, longitude, latitude) |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)