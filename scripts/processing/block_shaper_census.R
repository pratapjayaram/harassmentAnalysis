library(tidyverse)
library(sf)
library(httr)

## Pull in Brooklyn block groups
censusbg_url <- parse_url("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services")
censusbg_url$path <- paste(censusbg_url$path, "NYC_Census_Blocks_for_2020_US_Census/FeatureServer/0/query", sep = "/")
censusbg_url$query <- list(where = "BoroCode=3",
                       outFields = "*",
                       returnGeometry = "true",
                       f = "geojson")
censusbg_request <- build_url(censusbg_url)
censusbg_bk <- st_read(censusbg_request)
