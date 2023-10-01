library(tidyverse)
library(sf)
library(ggplot2)

## Generate scatterplots of all independent variables
pairs(sigvars_model[[2]][,c(2:10, 12)], main = "Scatter Plot of Regression Variables")

correlations <- sigvars_model[[2]][,c(2:10, 12)] %>%
  as.matrix() %>%
  cor() %>%
  round(3)
