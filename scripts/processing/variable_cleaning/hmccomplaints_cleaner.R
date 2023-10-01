library(tidyverse)
library(RSocrata)

## PURPOSE: Isolate Housing Maintenance Code violations for all properties in Brooklyn, associated to a BBL

## Generate query for Brooklyn Housing Maintenance Code violations between chosen dates
## hmvios API docs: https://dev.socrata.com/foundry/data.cityofnewyork.us/wvxf-dwi5
hmvios_url <- "https://data.cityofnewyork.us/resource/wvxf-dwi5.json"
hmvios_query <- paste("(boro = 'BROOKLYN'",
                      "approveddate between '2019-06-14T00:00:00' and '2022-12-31T23:59:59')",
                      sep = " AND ")
hmvios_request <- paste(hmvios_url, hmvios_query, sep = "?$where=")
hmvios_response <- read.socrata(hmvios_request)

## Create summary dataframe, filtering out I-class violations, and separating by the remaining classes
hmvios_summary <- hmvios_response |> 
  filter(class != "I",
         !is.na(bbl)) |>
  mutate(bbl = as.numeric(bbl)) |> 
  count(bbl, class) |> 
  spread(class, n) |> 
  rename(hmviosA = A, hmviosB = B, hmviosC = C) |> 
  replace_na(list(hmviosA = 0, hmviosB = 0, hmviosC = 0)) |> 
  rowwise() |> 
  mutate(hmviosTOTAL = sum(hmviosA, hmviosB, hmviosC)) |> 
  ungroup()

## Create sf with long/lat fields
hmvios_xy <- hmvios_summary |> 
  left_join(hmvios_response, by = "bbl", multiple = "any") |> 
  select(bbl, n, longitude, latitude) |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)