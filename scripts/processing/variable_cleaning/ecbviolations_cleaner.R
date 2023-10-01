library(tidyverse)
library(RSocrata)
library(sf)
library(httr)

## PURPOSE: Isolate DOB Environmental Control Board violations for all properties in Brooklyn, associated to a BBL

## Generate query for ECB violations at Brooklyn properties between chosen dates with a finding of a violation
## API docs: https://dev.socrata.com/foundry/data.cityofnewyork.us/6bgk-3dad
ecbviolations_url <- "https://data.cityofnewyork.us/resource/6bgk-3dad.csv"
ecbviolations_query <- paste("(issue_date>'20190614'",
                             "issue_date<'20230101'",
                             "boro='3'",
                             "hearing_status!='DISMISSED')",
                             sep = " AND ")
ecbviolations_request <- paste(ecbviolations_url, ecbviolations_query, sep = "?$where=")
ecbviolations_response <- read.socrata(ecbviolations_request)

## Create bbl field
ecbviolations_cleaned <- ecbviolations_response |> 
  mutate(bbl = as.integer(boro)*10^9 + as.integer(block)*10^4 + as.integer(lot))

## Create summary dataframe
ecbviolations_summary <- ecbviolations_cleaned |>
  mutate(bbl = as.numeric(bbl)) |> 
  group_by(bbl) |> 
  count() |> 
  rename(ecb = n)

## Get matching PLUTO BBLs
pluto_url <- parse_url("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services")
pluto_url$path <- paste(pluto_url$path, "MAPPLUTO/FeatureServer/0/query", sep = "/")
pluto_url$query <- list(where = "BoroCode = 3 AND UnitsRes > 0",
                        outFields = "*",
                        returnGeometry = "true",
                        f = "geojson")
pluto_request <- build_url(pluto_url)
pluto_response_ecb <- st_read(pluto_request) |>
  filter(BBL %in% ecbviolations_summary$bbl)

## Join longitude and latitude fields to summary dataset
ecbviolations_xy <- pluto_response_ecb |>
  rename(bbl = `BBL`,
         longitude = `Longitude`,
         latitude = `Latitude`) |>
  select(bbl, longitude, latitude) |> 
  st_drop_geometry() |> 
  left_join(ecbviolations_summary, by = "bbl", multiple = "any") |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
