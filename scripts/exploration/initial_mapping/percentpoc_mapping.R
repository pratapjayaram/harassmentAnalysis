library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)

## Map percentage POC per tract
ggplot(percpoc_cleaned) +
  annotation_map_tile("cartolight", zoomin = 1) +
  geom_sf(fill = "#A50F15", aes(alpha = percPOC)) +
  theme_void()
