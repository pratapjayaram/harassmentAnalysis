library(tidyverse)
library(tidycensus)

## PURPOSE: Isolate percent POC for all census tracts in Brooklyn

## Load 1-year ACS data
acs2021 <- load_variables(2021, "acs1", cache = T)

## Pull the total population and the total non-Hispanic white population for each census tract
nonhispwhite_raw <- get_acs(geography = "tract",
                            variables = c(total = "B03002_001",
                                          nonhispwhite = "B03002_003"),
                            state = "New York",
                            county = "Kings",
                            year = 2021,
                            output = "wide",
                            geometry = T)

## Calculate percent POC as the inverse of the percent non-Hispanic white population
percpoc_cleaned <- nonhispwhite_raw |> 
  mutate(percPOC = 1-(nonhispwhiteE/totalE)) |> 
  select(NAME, percPOC) |> 
  st_transform(2263)
