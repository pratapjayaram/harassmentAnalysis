library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)

## Filter out 0 values for mapping
nypd311_blocks_nonzero <- nypd311_blocks |> 
  filter(n > 0)

nypd311_blocks_nonzero |> 
  ggplot() +
    annotation_map_tile("cartolight", zoomin = 1) +
    geom_sf(aes(color = n)) +
    scale_color_gradient(low = "#FFD3D1", high = "#663735") +
    theme_void()

## Map calls per tract
nypd311_tracts_breaks <- c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
nypd311_tracts_quants <- quantile(nypd311_tracts$n, probs = nypd311_tracts_breaks)
nypd311_tracts_scale <- col_quantile(palette = "Reds", domain = nypd311_tracts$n, probs = nypd311_tracts_breaks)
nypd311_tracts_colmap <- nypd311_tracts |> 
  mutate(tractfill = nypd311_tracts_scale(n))
ggplot(nypd311_tracts_colmap) +
  annotation_map_tile("cartolight", zoomin = 1) +
  geom_sf(fill = nypd311_tracts_colmap$tractfill) +
  # scale_fill_manual(values = nypd311_tracts_scale(n)) +
  theme_void()

