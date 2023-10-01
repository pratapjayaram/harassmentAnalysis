library(tidyverse)
library(sf)

## PURPOSE: Join census tract level variables to the PLUTO lot shapes

## Spatial join percent of POC population to PLUTO lots
percpoc_lots <- plutolots_2023 |> 
  st_join(percpoc_cleaned, largest = T) |> 
  select(bbl, percPOC) |> 
  st_drop_geometry()

## Spatial join percent rent burdened to PLUTO lots
rentburden_lots <- plutolots_2023 |> 
  st_join(rentburden_clean, largest = T) |> 
  select(bbl, percRentBurden) |> 
  st_drop_geometry()

## Create lots2023_blockvars dataframe with percent rent burden and percent POC lot data
lots2023_ctvars <- percpoc_lots |> 
  left_join(rentburden_lots, by = "bbl")

## Code all NA values as 0
lots2023_ctvars[is.na(lots2023_ctvars)] <- 0
