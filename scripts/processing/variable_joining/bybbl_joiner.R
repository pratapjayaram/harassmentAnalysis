library(tidyverse)
library(sf)

## PURPOSE: Join variables which are associated to a BBL to the PLUTO lot shapes

## Isolate all Brooklyn 2023 BBLs
bbls2023 <- plutolots_2023 |> 
  select(bbl) |> 
  st_drop_geometry()

## Combine all lot variable dataframes into a single list
lotvars_summary <- list(bbls2023, hpactions_summary, evictions_summary, dohmh_summary, ecbviolations_summary, rentstab21_summary, hmcomplaints_summary, hmvios_summary)

## See how many BBLs from each variable summary dataset are unmatched in the PLUTO lot df
# unmatched_bbls <- lapply(lotvars_summary, anti_join, y = plutolots_2023, by = "bbl")

## Join variable dataframes to PLUTO lots
lots2023_lotvars <- lotvars_summary |> 
  reduce(left_join, by = "bbl")

## Code all NA values as 0
lots2023_lotvars[is.na(lots2023_lotvars)] <- 0

## Run correlations for all lot variables
lots2023_lotvars[,c(2:ncol(lots2023_lotvars))] |> 
  as.matrix() |> 
  cor() |> 
  round(3) |> 
  view()

