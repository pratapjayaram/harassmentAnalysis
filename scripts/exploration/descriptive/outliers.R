library(tidyverse)
library(sf)
library(ggplot2)

## PURPOSE: Capture outliers of explanatory variables for both the entire and equalized lot datasets

## Number of bbls from the entire lot dataset with nonzero value for each variable
variables_nonzero <- data.frame(variable = c("evictions", "dohmh", "ecb", "nypd311", "arrests", "unitsRes", "hmcomplaints"))
for (i in 1:length(variables_nonzero$variable)){
  variables_nonzero$cases[i] <- nrow(lots2023_allvars[lots2023_allvars[[variables_nonzero$variable[i]]] > 0,])
}

## Identify outliers for each explanatory variable column in the entire lot dataset
outliers_all <- data.frame(bbl = integer(), variable = factor(), value = double(), harass = logical())
for (col in c("evictions", "dohmh", "ecb", "nypd311", "arrests", "unitsRes", "hmcomplaints")){
  mean <- mean(lots2023_allvars[[col]][lots2023_allvars[[col]] > 0])
  stddev <- sd(lots2023_allvars[[col]][lots2023_allvars[[col]] > 0])
  upper_bound <- mean + 3*stddev
  for (row in 1:nrow(lots2023_allvars)){
    if (lots2023_allvars[[col]][row] > upper_bound){
      outliers_all <- outliers_all |>
        add_row(data.frame(bbl = lots2023_allvars$bbl[row],
                           variable = factor(col),
                           value = lots2023_allvars[[col]][row],
                           harass = lots2023_allvars$harassOccurred[row]))
    }
  }
}

## Create summary table for outliers from entire lot dataset
outliers_all_summary <- outliers_all |> 
  group_by(variable) |> 
  summarize(outliers = n(),
            percLots = outliers/nrow(lots2023_allvars),
            harassPerc = sum(harass)/outliers) |> 
  left_join(variables_nonzero, by = "variable") |> 
  mutate(percVariable = outliers/cases) |> 
  relocate(c(percLots, harassPerc), .after = percVariable)

## Number of bbls from the equalized lot dataset with nonzero value for each variable
variables_nonzero_eq <- data.frame(variable = c("evictions", "dohmh", "ecb", "nypd311", "arrests", "unitsRes", "hmcomplaints"))
for (i in 1:length(variables_nonzero_eq$variable)){
  variables_nonzero_eq$cases[i] <- nrow(lots2023_allvars_equalized[lots2023_allvars_equalized[[variables_nonzero_eq$variable[i]]] > 0,])
}

## Identify outliers for each explanatory variable column in the equalized lot dataset
## Calculate upper bound using full lot dataset mean and sd
outliers_equalized <- data.frame(bbl = integer(), variable = factor(), value = double(), harass = logical())
for (col in c("evictions", "dohmh", "ecb", "nypd311", "arrests", "unitsRes", "hmcomplaints")){
  mean <- mean(lots2023_allvars[[col]][lots2023_allvars[[col]] > 0])
  stddev <- sd(lots2023_allvars[[col]][lots2023_allvars[[col]] > 0])
  upper_bound <- mean + 3*stddev
  for (row in 1:nrow(lots2023_allvars_equalized)){
    if (lots2023_allvars_equalized[[col]][row] > upper_bound){
      outliers_equalized <- outliers_equalized |>
        add_row(data.frame(bbl = lots2023_allvars_equalized$bbl[row],
                           variable = factor(col),
                           value = lots2023_allvars_equalized[[col]][row],
                           harass = lots2023_allvars_equalized$harassOccurred[row]))
    }
  }
}

## Create summary table for outliers from equalized lot dataset
outliers_equalized_summary <- outliers_equalized |> 
  group_by(variable) |> 
  summarize(outliers = n(),
            percLots = outliers/nrow(lots2023_allvars_equalized),
            harassPerc = sum(harass)/outliers) |> 
  left_join(variables_nonzero_eq, by = "variable") |> 
  mutate(percVariable = outliers/cases) |> 
  relocate(c(percLots, harassPerc), .after = percVariable)

## Identify outliers for each log-transformed explanatory variable column in the entire lot dataset
outliers_log <- data.frame(bbl = integer(), variable = factor(), value = double(), harass = logical())
for (col in varnames_log){
  mean <- mean(lots2023_allvars[[col]])
  stddev <- sd(lots2023_allvars[[col]])
  upper_bound <- mean + 3*stddev
  for (row in 1:nrow(lots2023_allvars)){
    if (lots2023_allvars[[col]][row] > upper_bound){
      outliers_log <- outliers_log |>
        add_row(data.frame(bbl = lots2023_allvars$bbl[row],
                           variable = factor(col),
                           value = lots2023_allvars[[col]][row],
                           harass = lots2023_allvars$harassOccurred[row]))
    }
  }
}

## Create summary table for outliers from entire lot dataset
outliers_log_summary <- outliers_log |> 
  group_by(variable) |> 
  summarize(outliers = n(),
            percLots = outliers/nrow(lots2023_allvars),
            harassPerc = sum(harass)/outliers)

## Identify outliers for each log-transformed explanatory variable column in the entire lot dataset
outliers_sqrt <- data.frame(bbl = integer(), variable = factor(), value = double(), harass = logical())
for (col in varnames_sqrt){
  mean <- mean(lots2023_allvars_equalized[[col]])
  stddev <- sd(lots2023_allvars_equalized[[col]])
  upper_bound <- mean + 3*stddev
  for (row in 1:nrow(lots2023_allvars_equalized)){
    if (lots2023_allvars_equalized[[col]][row] > upper_bound){
      outliers_sqrt <- outliers_sqrt |>
        add_row(data.frame(bbl = lots2023_allvars_equalized$bbl[row],
                           variable = factor(col),
                           value = lots2023_allvars_equalized[[col]][row],
                           harass = lots2023_allvars_equalized$harassOccurred[row]))
    }
  }
}

## Create summary table for outliers from entire lot dataset
outliers_sqrt_summary <- outliers_sqrt |> 
  group_by(variable) |> 
  summarize(outliers = n(),
            percLots = outliers/nrow(lots2023_allvars_equalized),
            harassPerc = sum(harass)/outliers)

