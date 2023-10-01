library(tidyverse)
library(sf)
library(httr)

## Pull in LION surface streets in Brooklyn
lion_url <- parse_url("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services")
lion_url$path <- paste(lion_url$path, "LION/FeatureServer/0/query", sep = "/")
lion_url$query <- list(where = "(LBoro='3' OR RBoro='3') AND RW_TYPE=1",
                       outFields = "*",
                       returnGeometry = "true",
                       f = "geojson")
lion_request <- build_url(lion_url)
lion_bk <- st_read(lion_request)

## Filter streets to eliminate duplicates and separated roadways
lion_bk_filtered <- lion_bk |> 
  filter(SegmentTyp %in% c("B", "U", "G"))

## Remove duplicate segments
lion_bk_unique <- lion_bk_filtered[!duplicated(lion_bk_filtered$SegmentID),]

## Buffer LION streets
lion_buffer <- lion_bk_unique |> 
  st_transform(3857) |> 
  st_buffer(10, endCapStyle = "FLAT") |> 
  st_transform(4326) |> 
  select(SegmentID, SegmentTyp, geometry)

## Write buffer to shapefile
st_write(lion_buffer, "raw/lion_bk_buffer.shp", append=F)
