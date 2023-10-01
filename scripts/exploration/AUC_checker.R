library(tidyverse)
library(sf)

## Create Receiver Operating Curve and calculate Area Under Curve for best model
## Adapted from: http://www.medicine.mcgill.ca/epidemiology/joseph/courses/epib-621/logfit.pdf
index <- sort.list(allvars_model[[1]]$fitted.values)
hosmer <- matrix(c(allvars_model[[2]]$harassOccurred[index], allvars_model[[1]]$fitted.values[index]), byrow=F, nrow=nrow(allvars_model[[2]]))
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
fit.pos <- allvars_model[[1]]$fitted.values[allvars_model[[2]]$harassOccurred==1]
fit.neg <- allvars_model[[1]]$fitted.values[allvars_model[[2]]$harassOccurred==0]
wilcox <- wilcox.test(x=fit.pos, y=fit.neg)
AUC <- wilcox$statistic/((5986/2)^2) ## 95.8% POG

## Find ideal cutoff point for prediction, minimizing distance to the point [1, 0]
sensspec_table <- data.frame(sens, spec, fpf = (1-spec)) |> 
  mutate(distance = sqrt((1-sens)^2 + fpf^2))
## Reveals that the cutoff value of 0.3 produces the best predictions, rerunning prediction in hpaction_regression_fx
## reveals a 91.6% accuracy with this value, with 63 missed false and true cases each

