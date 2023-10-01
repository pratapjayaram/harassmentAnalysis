library(tidyverse)
library(sf)
library(caret)
library(fmsb)
library(generalhoslem)

## Create a function to run and return results of a logit regression for harassment
hpaction_logit_runner <- function(equalized_data, outcome, explanatory, return_model = F){
  
  ## Split into training and test sets
  set.seed(123)
  training_samples <- equalized_data$harassOccurred |> 
    createDataPartition(p = 0.8, list = FALSE)
  traindata <- equalized_data[training_samples,]
  testdata <- equalized_data[-training_samples,]
  
  ## Run regression on training data
  frm = as.formula(paste(outcome, "~", paste(explanatory, collapse = "+"), sep = ""))
  logit <- glm(frm, data = traindata, family = "binomial")
  
  ## Test model against test data
  probabilities <- logit |>
    predict(testdata, type = "response")
  predicted_classes <- ifelse(probabilities > 0.3, 1, 0)
  model_accuracy <- mean(predicted_classes == testdata$harassOccurred)
  model_mismatches <- summary(factor(predicted_classes[predicted_classes != testdata$harassOccurred]))
  
  ## Run Hosmer_Lemeshow test
  hl_results <- logitgof(traindata$harassOccurred, fitted(logit), g = 6)
  
  ## Return
  result_summary <- list(summary = summary(logit),
                         r_squared = NagelkerkeR2(logit)[2],
                         odds_ratios = exp(coef(logit)),
                         hosmer_lemeshow = hl_results,
                         accuracy = list(prediction_accuracy = model_accuracy,
                                         missed_predictions = model_mismatches),
                         model = if(return_model){list(logit, traindata, testdata)})
  return(result_summary)
  
}

## Create a function to predict probabilities of remaining BK properties from logit model
harass_predictor <- function(all_data, logit, equalized_data){
  
  # Predict values of remaining properties with F harassOccurred values
  remaining_harassF <- equalized_data |>
    filter(!(bbl %in% equalized_data$bbl))
  probabilities_remaining_harassF <- logit |>
    predict(remaining_harassF, type = "response", se.fit = T) |>
    data.frame() |>
    dplyr::select(harassmentIndex = fit, error = se.fit)

  ## Join probabilities back to remaining properties dataset
  predicted_probabilities <- remaining_harassF
  predicted_probabilities$harassmentIndex <- probabilities_remaining_harassF$harassmentIndex
  predicted_probabilities$indexError <- probabilities_remaining_harassF$error

  ## Return predictions
  return(predicted_probabilities)
}
