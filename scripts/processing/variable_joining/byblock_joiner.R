library(tidyverse)
library(sf)

## PURPOSE: Join block level variables to the PLUTO lot shapes using spatial joins

## Buffer NYPD311 blocks to intersect with adjacent lots
nypd311_blocks_buffer <- nypd311_blocks |> 
  filter(n > 0) |> 
  mutate(buf_wdt = if_else(is.na(StreetWidth_Max), 50, StreetWidth_Max + 10))
for(i in 1:nrow(nypd311_blocks_buffer)){
  nypd311_blocks_buffer[i,] <- st_buffer(nypd311_blocks_buffer[i,], dist = nypd311_blocks_buffer$buf_wdt[i])
}

## Spatial join 311 call data to PLUTO lots
nypd311_lots <- plutolots_2023 |> 
  st_join(nypd311_blocks_buffer) |> 
  select(bbl, n) |> 
  st_drop_geometry() |> 
  group_by(bbl) |> 
  count(wt = n) |> 
  rename(nypd311 = `n`)
length(nypd311_lots$nypd311[nypd311_lots$nypd311 > 0])

## Buffer arrests blocks to intersect with adjacent lots
arrests_blocks_buffer <- arrests_blocks |>
  filter(n > 0) |> 
  mutate(buf_wdt = if_else(is.na(StreetWidth_Max), 50, StreetWidth_Max + 10))
for(i in 1:nrow(arrests_blocks_buffer)){
  arrests_blocks_buffer[i,] <- st_buffer(arrests_blocks_buffer[i,], dist = arrests_blocks_buffer$buf_wdt[i])
}

## Spatial join NYPD arrest data to PLUTO lots
arrests_lots <- plutolots_2023 |> 
  st_join(arrests_blocks_buffer) |> 
  select(bbl, n) |> 
  st_drop_geometry() |> 
  group_by(bbl) |> 
  count(wt = n) |> 
  rename(arrests = `n`)
length(arrests_lots$arrests[arrests_lots$arrests > 0])

## Create dataframe of block level data for all PLUTO lots
lots2023_blockvars <- nypd311_lots |> 
  left_join(arrests_lots, by = "bbl")

## Code all NA values as 0
lots2023_blockvars[is.na(lots2023_blockvars)] <- 0
