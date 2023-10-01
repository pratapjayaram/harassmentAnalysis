library(tidyverse)
library(sf)
library(httr)

## PURPOSE: Isolate all likely rent stabilized properties in Brooklyn using PLUTO data

## Query the PLUTO API for buildings built before 1974 and with 6 or more residential units
pluto_url_rs <- parse_url("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services")
pluto_url_rs$path <- paste(pluto_url_rs$path, "MAPPLUTO/FeatureServer/0/query", sep = "/")
pluto_url_rs$query <- list(where = "BoroCode = 3 AND UnitsRes > 0",
                        outFields = "*",
                        returnGeometry = "false",
                        f = "json")
pluto_request_rs <- build_url(pluto_url_rs)
pluto_response_rs <- st_read(pluto_request_rs) |> 
  select(BBL, UnitsRes, YearBuilt)

## Isolate fields of interest from PLUTO
rentstabPLUTO_cleaned <- pluto_response_rs |> 
  st_drop_geometry() |> 
  select(BBL, UnitsRes, YearBuilt) |> 
  mutate(rentstab = T)
