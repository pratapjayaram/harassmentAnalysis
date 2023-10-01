library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)

## Get matching PLUTO BBLs
# pluto_url <- parse_url("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services")
# pluto_url$path <- paste(pluto_url$path, "MAPPLUTO/FeatureServer/0/query", sep = "/")
# pluto_url$query <- list(where = "BoroCode = 3 AND UnitsRes > 0",
#                                outFields = "*",
#                                returnGeometry = "true",
#                                f = "geojson")
# pluto_request <- build_url(pluto_url)
# pluto_response <- st_read(pluto_request) |> 
#   filter(BBL %in% hpactions_summary$bbl)

## Join longitude and latitude fields to summary dataset

hpactions_xy |> 
  ggplot() +
    annotation_map_tile("cartolight", zoomin = 1) +
    geom_sf(col = alpha("#484848", 0.5),
            aes(size = 1 + log(n))) +
    theme_void()
