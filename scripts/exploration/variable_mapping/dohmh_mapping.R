library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)

dohmh_xy |> 
  ggplot() +
  annotation_map_tile("cartolight", zoomin = 1) +
  geom_sf(col = alpha("#484848", 0.5),
          aes(size = 1 + log(n))) +
  theme_void()
