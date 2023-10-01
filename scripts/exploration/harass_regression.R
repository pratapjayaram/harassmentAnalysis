library(tidyverse)
library(sf)
library(caret)
library(fmsb)
library(generalhoslem)

## PURPOSE: Build and test a logit regression model to estimate harassment probability

## Run regression using all variables
hpaction_logit_runner(equalized_data = lots2023_allvars_equalized,
                      outcome = "harassOccurred",
                      explanatory = c("evictions", "shareRS", "dohmh", "ecb", "nypd311", "arrests", "percPOC", "percRentBurden", "unitsRes", "hmcomplaints")) ## R-squared = .706, Accuracy = 91.0%

## Run regression using significant variables returned in initial results
hpaction_logit_runner(equalized_data = lots2023_allvars_equalized,
                      outcome = "harassOccurred",
                      explanatory = c("evictions", "dohmh", "ecb", "nypd311", "percPOC", "unitsRes", "hmcomplaints")) ## R-squared = .706, Accuracy = 91.2%

## Run regression using unit-normalized variables
hpaction_logit_runner(equalized_data = lots2023_allvars_equalized,
                      outcome = "harassOccurred",
                      explanatory = c("evictions_PU", "shareRS", "dohmh_PU", "ecb_PU", "hmcomplaints_PU", "nypd311", "arrests", "percPOC", "percRentBurden")) ## R-squared = .709, Accuracy = 89.4%

## Run regression using logged variables
hpaction_logit_runner(equalized_data = lots2023_allvars_equalized,
                      outcome = "harassOccurred",
                      explanatory = c("evictions_log", "shareRS_log", "dohmh_log", "ecb_log", "nypd311_log", "arrests_log", "percPOC_log", "percRentBurden_log", "unitsRes_log", "hmcomplaints_log")) ## R-squared = .769, Accuracy = 91.6%

## Run regression using only significant logged variables
hpaction_logit_runner(equalized_data = lots2023_allvars_equalized,
                      outcome = "harassOccurred",
                      explanatory = c("evictions_log", "shareRS_log", "ecb_log", "percPOC_log", "unitsRes_log", "hmcomplaints_log")) ## R-squared = .769, Accuracy = 91.6%

## Run regression using bounded variables
hpaction_logit_runner(equalized_data = lots2023_allvars_equalized,
                      outcome = "harassOccurred",
                      explanatory = c("evictions_bounded", "shareRS_bounded", "dohmh_bounded", "ecb_bounded", "nypd311_bounded", "arrests_bounded", "percPOC_bounded", "percRentBurden_bounded", "unitsRes_bounded", "hmcomplaints_bounded")) ## R-squared = .706, Accuracy = 91.0%

## Generate predictions from lots2023_allvars_equalized model
allvars_model <- hpaction_logit_runner(equalized_data = lots2023_allvars_equalized,
                                       outcome = "harassOccurred",
                                       explanatory = c("evictions_bounded", "shareRS_bounded", "dohmh_bounded", "ecb_bounded", "nypd311_bounded", "arrests_bounded", "percPOC_bounded", "percRentBurden_bounded", "unitsRes_bounded", "hmcomplaints_bounded"),
                                       return_model = T)[["model"]]
predicted_probabilities_bk <- harass_predictor(all_data = lots2023_allvars_equalized,
                                               logit = logsigvars_model[[1]],
                                               equalized_data = lots2023_allvars_equalized)

## Conduct Pearson chi-squared test
chisq.test(logsigvars_model[[3]]$harassOccurred, logsigvars_model[[1]]$fitted.values)

## Calculate VIF
library(car)
vif(logsigvars_model[[1]]) ## No multicollinearity found

## Calculate likelihood ratio
library(lmtest)
lrtest(logsigvars_model[[1]]) ## Chisq = 5148, null model is inferior

## Find residual outliers
# residuals_lower <- mean(sigvars_model[[1]]$residuals) - 3*sd(sigvars_model[[1]]$residuals)
# residuals_upper <- mean(sigvars_model[[1]]$residuals) + 3*sd(sigvars_model[[1]]$residuals)
# residual_outliers <- sigvars_model[[3]][sigvars_model[[1]]$residuals < residuals_lower,] |> 
#   add_row(sigvars_model[[3]][sigvars_model[[1]]$residuals > residuals_upper,])

## Run stepwise regression on training data
# stepwise_lotvars_sampled <- logit_lotvars_sampled |> 
#   stepAIC(trace = F)
# summary(stepwise_lotvars_sampled) ## Included: evictions, shareRS, dohmh, ecb, nypd311, percPOC
# NagelkerkeR2(stepwise_lotvars_sampled) ## 0.430

## Test stepwise model against test data
# probabilities_hpactions_stepwise <- stepwise_lotvars_sampled |> 
#   predict(testdata_hpactions, type = "response")
# predicted_classes_stepwise <- ifelse(probabilities_hpactions_stepwise > 0.5, 1, 0)
# mean(predicted_classes_stepwise == testdata_hpactions$harassOccurred) ## 0.754

## Generate accuracy to training values
# probabilities_stepwise_training <- stepwise_lotvars_sampled |> 
#   predict(traindata_hpactions, type = "response")
# predicted_classes_training <- ifelse(probabilities_stepwise_training > 0.5, 1, 0)
# mean(predicted_classes_training == traindata_hpactions$harassOccurred) ## 0.759

