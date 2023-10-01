library(tidyverse)
library(sf)
library(RSocrata)
library(httr)
library(tigris)
library(scales)
library(ggplot2)
library(ggspatial)

## PURPOSE: Isolate and map order maintenance arrests for all blocks in Brooklyn, associated to a segment of the LION street database

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

## Spatial join xy data to street buffer, to check arrests that overlap multiple segments and identify duplicate street segments
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
  select(SegmentID, Street, FromLeft, ToLeft, FromRight, ToRight, geometry) |> 
  left_join(arrests_summary, by = "SegmentID") |> 
  mutate(n = if_else(is.na(n), 0, n))

## Pull in Brooklyn census tracts
bk_tracts <- tracts(state = "NY", county = "Kings", cb = TRUE) |> 
  st_transform(4326)

## Summarize arrests per census tract
arrests_tracts <- arrests_xy |> 
  st_join(select(bk_tracts, NAME)) |>
  st_drop_geometry() |> 
  group_by(NAME) |> 
  count() |> 
  right_join(bk_tracts, by = "NAME") |> 
  st_as_sf() |> 
  mutate(n = if_else(is.na(n), 0, n))

## Filter out 0 values for mapping
arrests_blocks_nonzero <- arrests_blocks |> 
  filter(n > 0) |> 
  rbind(arrests_blocks[1,])

## Map calls per block
arrests_blocks_breaks <- c(0, 0.5, 0.75, 0.9, 0.95, 0.99, 1)
arrests_blocks_nonzero |> 
  ggplot() +
    annotation_map_tile("cartolight", zoomin = 1) +
    geom_sf(aes(color = n)) +
    binned_scale(aesthetics = "color",
                 scale_name = "stepsn",
                 palette = function(x) brewer_pal(palette = "Reds")(6),
                 breaks = quantile(arrests_blocks_nonzero$n, arrests_blocks_breaks),
                 nice.breaks = T,
                 show.limits = T,
                 guide = "colorsteps") +
    theme_void() +
    labs(title = "Order Maintenance Arrests by Block in Brooklyn, 2019/06/14 - 2022/12/31",
         subtitle = "Source: NYPD",
         color = "Percentile of Arrests\n")

## Map calls per census tract
arrests_tracts_breaks <- c(0, 0.5, 0.75, 0.9, 0.95, 0.99, 1)
ggplot(arrests_tracts) +
  annotation_map_tile("cartolight", zoomin = 1) +
  geom_sf(aes(fill = n)) +
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn",
               palette = function(x) brewer_pal(palette = "Reds")(6),
               breaks = quantile(arrests_tracts$n, arrests_tracts_breaks),
               nice.breaks = T,
               show.limits = T,
               guide = "colorsteps") +
  theme_void() +
  labs(title = "Order Maintenance Arrests by Census Tract in Brooklyn, 2019/06/14 - 2022/12/31",
       subtitle = "Source: NYPD",
       fill = "Percentile of Arrests\n")


