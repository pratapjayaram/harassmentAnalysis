library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
# library(terra)
# library(spatstat)
library(tigris)
library(scales)

## Pull in Brooklyn census tracts
bk_tracts <- tracts(state = "NY", county = "Kings", cb = TRUE) |> 
  st_transform(4326)

## Merge to create dataframe with violations per tract
ecbviolations_tracts <- bk_tracts |>  
  st_join(ecbviolations_xy, left = T) |> 
  rename(tract = `NAME`) |> 
  group_by(tract) |> 
  count(wt = n)

## Generate palette for choropleth
ecb_breaks <- c(0, 10, 25, 50, 75, 100, 150, 200, 250, 352)
# ecb_pal <- col_numeric(palette = "Reds", domain = range(ecbviolations_tracts$n), alpha = 0.7)

## Create choropleth map
ecbviolations_tracts |> 
  ggplot() +
    annotation_map_tile("cartolight", zoomin = 1) +
    geom_sf(aes(fill = n)) +
    # scale_fill_viridis_b(trans = "log", breaks = ecb_breaks) +
    scale_fill_gradient(low = "#FFD3D1", high = "#663735", na.value = "grey90") +
    theme_void()

## Create bubble plot
ecbviolations_xy |>
  ggplot() +
    annotation_map_tile("cartolight", zoomin = 1) +
    geom_sf(col = alpha("#e34a33", 0.15),
            aes(size = 0.5 + log(n)/8)) +
    theme_void()

# ## create raster 
# ecbviolations_buff <- ecbviolations_xy |> 
#   st_transform(2263) |> 
#   st_buffer(1000)
# r <- rast(ext(ecbviolations_buff), nrow = 1000, ncol = 1000)
# ecbviolations_raster <- rasterize(ecbviolations_buff, r, field = "n", fun = "sum")
# plot(ecbviolations_raster,
#      type = "continuous",
#      smooth = T)
# 
# bk <- county_subdivisions(state = "New York", 
#                           county = "Kings", 
#                           class = "sf", 
#                           progress_bar = FALSE) |>
#   st_transform(6345) |> 
#   as.owin()
# ecbviolations_xy_6345 <- ecbviolations_xy |> 
#   st_transform(6345)
# r_6345 <- rast(ext(ecbviolations_xy_6345), nrow = 1000, ncol = 1000)
# ecbviolations_raster_6345 <- rasterize(ecbviolations_xy_6345, r_6345, field = "n", fun = "sum")
# ecb_pts <- crds(ecbviolations_raster_6345)
# ecb_ppp <- ppp(ecb_pts[,1], ecb_pts[,2], window=bk)
# ecb_density <- density(ecb_ppp) |> 
#   plot()
