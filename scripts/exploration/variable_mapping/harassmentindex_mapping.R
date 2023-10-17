library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)

## PURPOSE: Map the harassment index generated in harass_regression.R

## Create a heatmap using opacity to indicate risk
predicted_probabilities_bk |> 
  ggplot(aes(alpha = rescale(harassmentIndex, to = c(0.1, 0.9)))) +
    # annotation_map_tile("cartodark", zoomin = 1) +
    geom_sf(fill = "red", color = "transparent") +
    theme_void() +
    labs(alpha = "Risk of Harassment",
         title = "Tenant Harassment Risk Index - Brooklyn, NY")

## Map all properties that the model predicted as experiencing harassment, using the 0.3 cutoff
predicted_probabilities_bk |> 
  filter(harassmentIndex > 0.3) |> 
  nrow()
  ggplot() +
    annotation_map_tile("cartodark", zoomin = 1) +
    geom_sf(fill = "red", color = "transparent") +
    theme_void() +
    labs(title = "Properties Predicted to be Experiencing Harassment")
    
## Create a bubble map of the 99th risk percentile, using bubble size to indicate number of units
predicted_probabilities_bk |> 
  filter(harassmentIndex >= quantile(predicted_probabilities_bk$harassmentIndex, c(0, 0.99, 1)[2])) |>
  st_centroid() |>
  ggplot() +
    annotation_map_tile("cartodark", zoomin = 1) +
    # geom_sf(data = arrests_tracts, aes(fill = n)) +
    # scale_fill_fermenter(palette = "Reds",
    #                      breaks = quantile(arrests_tracts$n, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1)),
    #                      direction = 1) +
    # geom_sf(color = "white", size = 1, alpha = 0.4) +
    geom_sf(color = "red", alpha = 0.4, aes(size = unitsRes)) +
    scale_size(range = c(1, 20)) +
    theme_void() +
    labs(size = "Residential Units",
         title = "Highest Risk Properties (Top 1%) - Brooklyn, NY")
