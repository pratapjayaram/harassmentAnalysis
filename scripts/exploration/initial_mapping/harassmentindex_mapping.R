library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)

## PURPOSE: Map the harassment index generated in harass_regression.R

## Create a heatmap using opacity to indicate risk
predicted_probabilities_bk |> 
  st_as_sf() |> 
  ggplot(aes(alpha = rescale(harassmentIndex, to = c(0.1, 0.9)))) +
    annotation_map_tile("cartodark", zoomin = 1) +
    geom_sf(fill = "red", color = "transparent") +
    theme_void() +
    labs(alpha = "Risk of Harassment",
         title = "Tenant Harassment Risk Index - Brooklyn, NY")
    
## Create a bubble map of the 10th risk percentile, using opacity to indicate risk, and bubble size to indicate number of units
predicted_probabilities_bk |> 
  filter(harassmentIndex >= quantile(predicted_probabilities_bk$harassmentIndex, c(0, 0.99, 1)[2])) |>
  # filter(harassmentIndex >= 0.8) |> 
  st_as_sf() |> 
  st_centroid() |> 
  ggplot(aes(size = rescale(unitsRes, to = c(4, 50)))) +
    annotation_map_tile("cartodark", zoomin = 1) +
    geom_sf(color = "red", alpha = 0.4) +
    theme_void() +
    labs(size = "Residential Units",
         title = "Highest Risk Properties (Top 1%) - Brooklyn, NY")
