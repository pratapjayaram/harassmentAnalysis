library(tidyverse)
library(sf)

## Take a sample of all False harassment records equivalent to the number of True records
# set.seed(321)
# set.seed(20230924)
set.seed(20230928)
samples_harassF <- sample(lots2023_allvars$bbl[lots2023_allvars$harassOccurred == F],
                          size = sum(lots2023_allvars$harassOccurred == T))

## Create a dataframe with the sample False records and all True records
lots2023_allvars_equalized <- lots2023_allvars |> 
  filter(bbl %in% samples_harassF) |> 
  rbind(lots2023_allvars[lots2023_allvars$harassOccurred == T,])

