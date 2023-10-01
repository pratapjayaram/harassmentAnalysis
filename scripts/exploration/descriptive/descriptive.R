library(tidyverse)
library(sf)
library(ggplot2)

## PURPOSE: Generate basic summary statistics for all explanatory variables

## Create a list of all explanatory variable column names
varnames <- c("unitsRes", "evictions", "dohmh", "ecb", "hmcomplaints", "shareRS", "nypd311", "arrests", "percPOC", "percRentBurden")

## Generate a dataframe of means and sds of all explanatory variables from the full dataset
allvars_descriptive <- data.frame(variable = varnames,
                                  nonzero_properties = rep(0, length(varnames)),
                                  mean = rep(0, length(varnames)),
                                  median = rep(0, length(varnames)),
                                  sd = rep(0, length(varnames)),
                                  max_value = rep(0, length(varnames)))
for (i in 1:nrow(allvars_descriptive)){
  nozeros <- lots2023_allvars[[allvars_descriptive$variable[i]]] > 0
  allvars_descriptive$nonzero_properties[i] <- sum(nozeros)
  allvars_descriptive$mean[i] <- lots2023_allvars[[allvars_descriptive$variable[i]]][nozeros] |> 
    mean() |> round(3)
  allvars_descriptive$median[i] <- lots2023_allvars[[allvars_descriptive$variable[i]]][nozeros] |> 
    median() |> round(3)
  allvars_descriptive$sd[i] <- lots2023_allvars[[allvars_descriptive$variable[i]]][nozeros] |> 
    sd() |> round(3)
  allvars_descriptive$max_value[i] <- lots2023_allvars[[allvars_descriptive$variable[i]]][nozeros] |> 
    max()
}

## Generate a dataframe of means and sds of all explanatory variables from the equalized dataset
allvars_descriptive_eq <- data.frame(variable = varnames,
                                  nonzero_properties = rep(0, length(varnames)),
                                  mean = rep(0, length(varnames)),
                                  sd = rep(0, length(varnames)))
for (i in 1:nrow(allvars_descriptive_eq)){
  nozeros <- lots2023_allvars_equalized[[allvars_descriptive_eq$variable[i]]] > 0
  allvars_descriptive_eq$nonzero_properties[i] <- sum(nozeros)
  allvars_descriptive_eq$mean[i] <- lots2023_allvars_equalized[[allvars_descriptive_eq$variable[i]]] |> 
    mean() |> 
    round(3)
}
for (i in 1:nrow(allvars_descriptive_eq)){
  allvars_descriptive_eq$sd[i] <- lots2023_allvars_equalized[[allvars_descriptive_eq$variable[i]]] |> 
    sd() |> 
    round(3)
}

## Generate a table of correlations for all explanatory variables from the full lot dataset
correlations_all <- lots2023_allvars[,names(lots2023_allvars) %in% c(varnames, "harassOccurred")] |> 
  st_drop_geometry() |> 
  as.matrix() |> 
  cor() |> 
  round(3)

## Generate a table of correlations for all explanatory variables from the equalized lot dataset
correlations_equalized <- lots2023_allvars_equalized[,names(lots2023_allvars_equalized) %in% c(varnames, "harassOccurred")] |> 
  st_drop_geometry() |> 
  as.matrix() |> 
  cor() |> 
  round(3)

## Generate a table of correlations for all log-transformed explanatory variables from the equalized lot dataset
varnames_log <- paste(varnames, "_log", sep = "")
logvars_correlations_eq <- lots2023_allvars_equalized[,names(lots2023_allvars_equalized) %in% c(varnames_log, "harassOccurred")] |> 
  st_drop_geometry() |> 
  as.matrix() |> 
  cor() |> 
  round(3)

## Generate a table of correlations for all sqrt-transformed explanatory variables
varnames_sqrt <- paste(varnames, "_sqrt", sep = "")
sqrtvars_correlations_eq <- lots2023_allvars_equalized[,names(lots2023_allvars_equalized) %in% c(varnames_sqrt, "harassOccurred")] |> 
  st_drop_geometry() |> 
  as.matrix() |> 
  cor() |> 
  round(3)

## Generate scatterplots for the bounded explanatory variables from the full dataset
varnames_bounded <- paste(varnames, "_bounded", sep = "")
boundedvars_correlations <- lots2023_allvars[,names(lots2023_allvars) %in% c(varnames_bounded, "harassOccurred")] |> 
  st_drop_geometry() |> 
  as.matrix() |> 
  cor() |> 
  round(3)

## Generate a dataframe of means and sds of all bounded explanatory variables from the full dataset
boundedvars_descriptive <- data.frame(variable = varnames_bounded,
                                  nonzero_properties = rep(0, length(varnames_bounded)),
                                  mean = rep(0, length(varnames_bounded)),
                                  median = rep(0, length(varnames_bounded)),
                                  sd = rep(0, length(varnames_bounded)),
                                  max_value = rep(0, length(varnames_bounded)))
for (i in 1:nrow(boundedvars_descriptive)){
  nozeros <- lots2023_allvars[[boundedvars_descriptive$variable[i]]] > 0
  boundedvars_descriptive$nonzero_properties[i] <- sum(nozeros)
  boundedvars_descriptive$mean[i] <- lots2023_allvars[[boundedvars_descriptive$variable[i]]][nozeros] |> 
    mean() |> 
    round(3)
  boundedvars_descriptive$median[i] <- lots2023_allvars[[boundedvars_descriptive$variable[i]]][nozeros] |> 
    median() |> round(3)
  boundedvars_descriptive$sd[i] <- lots2023_allvars[[boundedvars_descriptive$variable[i]]][nozeros] |> 
    sd() |> 
    round(3)
  boundedvars_descriptive$max_value[i] <- lots2023_allvars[[boundedvars_descriptive$variable[i]]][nozeros] |> 
    max()
}

