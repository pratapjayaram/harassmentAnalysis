library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)

## Map rent burden percentage per tract
rentburden_breaks <- c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
rentburden_scale <- col_quantile(palette = "Reds", domain = rentburden_clean$percRentBurden, probs = rentburden_breaks)
rentburden_colmap <- rentburden_clean |> 
  mutate(tractfill = rentburden_scale(percRentBurden))
ggplot(rentburden_colmap) +
  annotation_map_tile("cartolight", zoomin = 1) +
  geom_sf(fill = rentburden_colmap$tractfill) +
  theme_void()
