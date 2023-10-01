library(tidyverse)
library(sf)
library(httr)

## PURPOSE: Pull all 2023 residential lots from the PLUTO API, as a starting point for variable joining

## Make PLUTO API request
pluto_url <- parse_url("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services")
pluto_url$path <- paste(pluto_url$path, "MAPPLUTO/FeatureServer/0/query", sep = "/")
pluto_url$query <- list(where = "BoroCode = 3 AND UnitsRes > 0",
                        outFields = "*",
                        returnGeometry = "true",
                        datumTransformation = "2263",
                        f = "geojson")
pluto_request <- build_url(pluto_url)
plutolots_2023 <- st_read(pluto_request) |> 
  select(bbl = BBL, address = Address, unitsRes = UnitsRes) |> 
  st_transform(2263)
