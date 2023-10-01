library(tidyverse)
library(sf)

## Isolate all bbls with outliers for any of the explanatory variables from the equalized lot data

equalized_lots <- allvars_model[[2]]

# Determine the number of unique bbls in the outliers dataframe
length(unique(outliers$bbl))
summary(factor(outliers$col)) ## Vast majority of outliers are lots where harassment occurred

## Log all explanatory variables
lots2023_allvars_log <- lots2023_allvars |> 
  mutate(evictions = if_else(evictions != 0, log(evictions), 0),
         dohmh = if_else(dohmh != 0, log(dohmh), 0),
         ecb = if_else(ecb != 0, log(ecb), 0),
         nypd311 = if_else(nypd311 != 0, log(nypd311), 0),
         arrests = if_else(arrests != 0, log(arrests), 0),
         shareRS = if_else(shareRS != 0, log(shareRS), 0),
         unitsRes = if_else(unitsRes != 0, log(unitsRes), 0),
         percPOC = if_else(percPOC != 0, log(percPOC), 0),
         percRentBurden = if_else(percRentBurden != 0, log(percRentBurden), 0))

## Run regression using log'd variables
hpaction_logit_runner(variables = lots2023_allvars_log,
                      outcome = "harassOccurred",
                      explanatory = c("evictions", "shareRS", "dohmh", "ecb", "nypd311", "arrests", "percPOC", "percRentBurden", "unitsRes")) ## 77.3% accuracy, .496 R-squared
sigvars_log_model <- hpaction_logit_runner(variables = lots2023_allvars_log,
                                           outcome = "harassOccurred",
                                           explanatory = c("evictions", "shareRS", "dohmh", "ecb", "nypd311", "arrests", "percPOC", "percRentBurden", "unitsRes"),
                                           return_model = T)[["model"]]
vif(sigvars_log_model[[1]]) ## No multicollinearity found
chisq.test(sigvars_log_model[[3]]$harassOccurred, sigvars_log_model[[1]]$fitted.values) ## p = 0.2691
lrtest(sigvars_log_model[[1]]) ## Chisq = 2786.8, null model is inferior
sigvars_log_model[[2]][,c(2:10, 12)] %>%
  as.matrix() %>%
  cor() %>%
  round(3) |> 
  view()

## Improved log model with more limited variables
sigvars_log_model <- hpaction_logit_runner(variables = lots2023_allvars_log,
                                           outcome = "harassOccurred",
                                           explanatory = c("evictions", "shareRS", "dohmh", "ecb", "percPOC", "unitsRes"),
                                           return_model = T)[["model"]] ## 77.3% accuracy, .495 R-squared
vif(sigvars_log_model[[1]]) ## No multicollinearity found
chisq.test(sigvars_log_model[[3]]$harassOccurred, sigvars_log_model[[1]]$fitted.values) ## p ~ 0
lrtest(sigvars_log_model[[1]]) ## Chisq = 2776.1, null model is inferior

## Create manual Hosmer Lemeshow test
## Adapted from: http://www.medicine.mcgill.ca/epidemiology/joseph/courses/epib-621/logfit.pdf
index <- sort.list(sigvars_log_model[[1]]$fitted.values)
hosmer <- matrix(c(sigvars_log_model[[3]]$harassOccurred[index], sigvars_log_model[[1]]$fitted.values[index]), byrow=F, nrow=nrow(sigvars_log_model[[3]]))
observed <- rep(NA, 10)
for (i in 1:10) {observed[i] <- sum(hosmer[(598*(i-1)+1):(598*i),1])/598}
predicted <- rep(NA, 10)
for (i in 1:10) {predicted[i] <- sum(hosmer[(598*(i-1)+1):(598*i),2])/598}
plot(predicted, observed, type="b") |> 
  abline(a = 0, b = 1)

sens <- rep(NA, 11)
spec <- rep(NA, 11)
sens[1] <- 1
spec[1] <- 0
deciles <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
for (i in 2:10){
  under_decile <- 5986 - sum(hosmer[,2] >= deciles[i])
  over_decile <- under_decile + 1
  sens[i] <- sum(hosmer[over_decile:5986,1])/(5986/2)
  spec[i] <- sum(1 - hosmer[1:under_decile,1])/(5986/2)
}
sens[11] <- 0
spec[11] <- 1
plot(1-spec, sens, type="b")
fit.pos <- sigvars_log_model[[1]]$fitted.values[sigvars_log_model[[3]]$harassOccurred==1]
fit.neg <- sigvars_log_model[[1]]$fitted.values[sigvars_log_model[[3]]$harassOccurred==0]
wilcox <- wilcox.test(x=fit.pos, y=fit.neg)
AUC <- wilcox$statistic/((5986/2)^2) ## 86.5% POG





