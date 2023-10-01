library(tidyverse)
library(sf)
library(caret)
library(fmsb)
library(generalhoslem)

## PURPOSE: Build and test a logit regression model to estimate harassment probability

## Take a sample of all False harassment records equivalent to the number of True records
set.seed(321)
fraction_harassT_PU <- sum(lots2023_allvars_perunit$harassOccurred == T)/sum(lots2023_allvars_perunit$harassOccurred == F)
samples_harassF_PU <- sample(lots2023_allvars_perunit$bbl[lots2023_allvars_perunit$harassOccurred == F],
                          size = sum(lots2023_allvars_perunit$harassOccurred == T))

## Create a dataframe with the sample False records and all True records
harass_equalized_lots_PU <- lots2023_allvars_perunit |> 
  filter(bbl %in% samples_harassF_PU) |> 
  rbind(lots2023_allvars_perunit[lots2023_allvars_perunit$harassOccurred == T,])
  
## Split into training and test sets
set.seed(123)
training_samples_hpactions_PU <- harass_equalized_lots_PU$harassOccurred |> 
  createDataPartition(p = 0.8, list = FALSE)
traindata_hpactions_PU <- harass_equalized_lots_PU[training_samples_hpactions_PU,]
testdata_hpactions_PU <- harass_equalized_lots_PU[-training_samples_hpactions_PU,]

## Create basic regression model
logit_lotvars_sampled_PU <- glm(harassOccurred ~ evictionsPU + shareRS + dohmhPU + ecbPU + nypd311 + arrests + percPOC + percRentBurden, data = traindata_hpactions_PU, family = "binomial")
summary(logit_lotvars_sampled_PU) ## Significant: evictionsPU, shareRS, dohmhPU, ecbPU, nypd311, percPOC
NagelkerkeR2(logit_lotvars_sampled_PU) ## 0.371, lower than model using raw lot data rather than normalized lot data

## Test model against test data
probabilities_hpactions_PU <- logit_lotvars_sampled_PU |> 
  predict(testdata_hpactions_PU, type = "response")
predicted_classes_PU <- ifelse(probabilities_hpactions_PU > 0.5, 1, 0)
mean(predicted_classes_PU == testdata_hpactions_PU$harassOccurred) ## 0.733

## Predict values of remaining properties with F harassOccurred values
remaining_harassF_PU <- lots2023_allvars_perunit |> 
  filter(harassOccurred != T,
         !(bbl %in% samples_harassF_PU))
probabilities_remaining_harassF_PU <- logit_lotvars_sampled_PU |> 
  predict(remaining_harassF_PU, type = "response", se.fit = T) |> 
  data.frame() |> 
  dplyr::select(harassmentIndex = fit, error = se.fit)
hist(probabilities_remaining_harassF_PU$harassmentIndex)

## Join probabilities back to remaining properties dataset
predicted_probabilities_bk_PU <- remaining_harassF_PU
predicted_probabilities_bk_PU$harassmentIndex <- probabilities_remaining_harassF_PU$harassmentIndex
predicted_probabilities_bk_PU$indexError <- probabilities_remaining_harassF_PU$error

## Run Hosmer-Lemeshow test
# fitted_lotvars_sampled_PU <- fitted(logit_lotvars_sampled_PU)
logitgof(traindata_hpactions_PU$harassOccurred, fitted(logit_lotvars_sampled_PU), g = 7)
