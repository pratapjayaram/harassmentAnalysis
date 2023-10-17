library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)

## Filter out 0 values for mapping
arrests_blocks_nonzero <- arrests_blocks |> 
  filter(n > 0)
arrests_blocks_nonzero |> 
  ggplot() +
    annotation_map_tile("cartolight", zoomin = 1) +
    geom_sf(aes(color = log(n))) +
    scale_color_gradient(low = "#FFD3D1", high = "#663735") +
    theme_void()

## Map calls per tract
arrests_tracts_breaks <- c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
# arrests_tracts_quants <- quantile(arrests_tracts$n, probs = arrests_tracts_breaks)
arrests_tracts_scale <- col_quantile(palette = "Reds", domain = arrests_tracts$n, probs = arrests_tracts_breaks)
arrests_tracts_colmap <- arrests_tracts |> 
  mutate(tractfill = arrests_tracts_scale(n))
ggplot(arrests_tracts_colmap) +
  annotation_map_tile("cartolight", zoomin = 1) +
  geom_sf(fill = arrests_tracts_colmap$tractfill) +
  # scale_fill_manual(values = arrests_tracts_scale(n)) +
  theme_void()
