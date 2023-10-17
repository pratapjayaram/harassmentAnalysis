library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)

# hpactions_xy |> 
#   ggplot() +
#     annotation_map_tile("cartolight", zoomin = 1) +
#     geom_sf(col = alpha("#484848", 0.5),
#             aes(size = 1 + log(n))) +
#     theme_void()

## Create an outline of Brooklyn
bk_outline <- bk_tracts |> 
  st_union()

## Create point dataset for lots
lots2023_allvars_point <- lots2023_allvars |>
  st_centroid()

## Create map using final variable dataset
lots2023_allvars_point |> 
  filter(harassOccurred == T) |> 
  ggplot() +
    annotation_map_tile("cartolight", zoomin = 1) +
    geom_sf(data = bk_outline, fill = "transparent", color = alpha("#171717", 0.8), lwd = 2) +
    geom_sf(color = "#A53434", alpha = 0.4, aes(size = unitsRes)) +
    scale_size(range = c(1, 15)) +
    theme_void() +
    labs(title = "Properties with Successful HP Actions, 06/14/2019 - 12/31/2022",
         subtitle = "Source: NYC OpenData, HPD",
         size = "Residential Units")
