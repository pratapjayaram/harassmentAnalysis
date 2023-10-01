library(tidyverse)
library(tidycensus)

## PURPOSE: Isolate rent burden for all census tracts in Brooklyn

## Load 1-year ACS data
acs2021 <- load_variables(2021, "acs1", cache = T)

## Pull rent burden for Brooklyn census tracts
rentburden_raw <- get_acs(geography = "tract",
                          variables = c(total = "B25070_001",
                                        count30to35 = "B25070_007",
                                        count35to40 = "B25070_008",
                                        count40to50 = "B25070_009",
                                        count50plus = "B25070_010"),
                          state = "New York",
                          county = "Kings",
                          year = 2021,
                          output = "wide",
                          geometry = T)

## Calculate rent burden percentage as a decimal
rentburden_clean <- rentburden_raw |> 
  mutate(percRentBurden = (count30to35E + count35to40E + count40to50E + count50plusE)/totalE) |> 
  select(NAME, percRentBurden) |> 
  st_transform(2263)
