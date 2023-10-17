library(tidyverse)
library(sf)
library(caret)
library(fmsb)
library(generalhoslem)

## Create final equalized variables dataframe
final_explanatory_varnames <- varnames_bounded
lots2023_finalvars <- lots2023_allvars |> 
  dplyr::select(all_of(c("bbl", "address", "harassOccurred", final_explanatory_varnames)))
lots2023_finalvars_equalized <- lots2023_allvars_equalized |> 
  dplyr::select(all_of(c("bbl", "address", "harassOccurred", final_explanatory_varnames)))

## Run regression using bounded variables
hpaction_logit_runner(equalized_data = lots2023_finalvars_equalized,
                      outcome = "harassOccurred",
                      explanatory = final_explanatory_varnames)

## Generate predictions from final model
final_model <- hpaction_logit_runner(equalized_data = lots2023_finalvars_equalized,
                                     outcome = "harassOccurred",
                                     explanatory = final_explanatory_varnames,
                                     return_model = T)[["model"]]
final_predictions <- harass_predictor(all_data = lots2023_finalvars,
                                      logit = final_model[[1]],
                                      equalized_data = lots2023_finalvars_equalized)

## Calculate VIF
library(car)
vif(final_model[[1]]) ## No multicollinearity found

## Calculate likelihood ratio
library(lmtest)
lrtest(final_model[[1]]) ## Chisq = 4494.9, null model is inferior
