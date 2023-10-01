library(tidyverse)
library(sf)

## PURPOSE: Join together all lot, block, and census tract data into a single dataframe

## Join dataframes together
lots2023_allvars <- plutolots_2023 |> 
  left_join(lots2023_lotvars, by = "bbl") |>
  relocate(unitsRes, .after = harassOccurred) |> 
  left_join(lots2023_blockvars, by = "bbl") |> 
  left_join(lots2023_ctvars, by = "bbl") |> 
  mutate(shareRS = shareRS*100,
         percPOC = percPOC*100,
         percRentBurden = percRentBurden*100)

## Normalize lot variables by unit count
lots2023_allvars <- lots2023_allvars |> 
  mutate(evictions_PU = evictions / unitsRes,
         dohmh_PU = dohmh / unitsRes,
         ecb_PU = ecb / unitsRes,
         hmcomplaints_PU = hmcomplaints / unitsRes)

## Transform all explanatory variables by a log_2 function
log_na_fixer <- 1
for (i in 1:length(varnames)){
  logvar_name <- paste(varnames[i], "_log", sep = "")
  lots2023_allvars[[logvar_name]] <- log(lots2023_allvars[[varnames[i]]]+log_na_fixer, 2)
}
# lots2023_allvars <- lots2023_allvars |> 
#   mutate(unitsRes_log = log(unitsRes+log_na_fixer, 2),
#          evictions_log = log(evictions+log_na_fixer, 2),
#          dohmh_log = log(dohmh+log_na_fixer, 2),
#          ecb_log = log(ecb+log_na_fixer, 2),
#          shareRS_log = log(shareRS+log_na_fixer, 2),
#          hmcomplaints_log = log(hmcomplaints+log_na_fixer, 2),
#          nypd311_log = log(nypd311+log_na_fixer, 2),
#          arrests_log = log(arrests+log_na_fixer, 2),
#          percPOC_log = log(percPOC+log_na_fixer, 2),
#          percRentBurden_log = log(percRentBurden+log_na_fixer, 2))

## Transform all explanatory variables by a sqrt function
for (i in 1:length(varnames)){
  sqrtvar_name <- paste(varnames[i], "_sqrt", sep = "")
  lots2023_allvars[[sqrtvar_name]] <- sqrt(lots2023_allvars[[varnames[i]]])
}
# lots2023_allvars <- lots2023_allvars |> 
#   mutate(unitsRes_sqrt = sqrt(unitsRes),
#          evictions_sqrt = sqrt(evictions),
#          dohmh_sqrt = sqrt(dohmh),
#          ecb_sqrt = sqrt(ecb),
#          shareRS_sqrt = sqrt(shareRS),
#          hmcomplaints_sqrt = sqrt(hmcomplaints),
#          nypd311_sqrt = sqrt(nypd311),
#          arrests_sqrt = sqrt(arrests),
#          percPOC_sqrt = sqrt(percPOC),
#          percRentBurden_sqrt = sqrt(percRentBurden))

## Assign upper bound value to all outliers
bounds <- allvars_descriptive |> 
  mutate(upper_bound = mean + 3*sd) |> 
  dplyr::select(variable, upper_bound) |> 
  pivot_wider(names_from = variable, values_from = upper_bound)
for (i in 1:length(varnames)){
  boundvar_name <- paste(varnames[i], "_bounded", sep = "")
  lots2023_allvars[[boundvar_name]] <- if_else(lots2023_allvars[[varnames[i]]] > bounds[[varnames[i]]],
                                               bounds[[varnames[i]]],
                                               lots2023_allvars[[varnames[i]]])
}

