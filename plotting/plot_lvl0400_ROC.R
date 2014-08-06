cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Construct a ROC curve with lvl0400 predictions
##  
##  Version: 2014-07-31
##  
################################################################################
##
##  Copyright (C) 2014 Simon Schlauss (sschlauss@gmail.com)
##
##
##  This file is part of BiFoRe.
##  
##  BiFoRe is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  BiFoRe is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with BiFoRe.  If not, see <http://www.gnu.org/licenses/>.
##  
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ROCR")

lapply(lib, function(...) library(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Daten/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv <- "Dropbox/Code/bifore/src/csv/kili/"
path.testing <- paste0(path.csv, "testing_100_test20_2014-07-28/")
path.image <- paste0("Dropbox/Code/bifore/src/images/")
file.in.prediction <- paste0(path.testing,"lvl_0400_prediction_20test.csv")



### Import data ################################################################

data.raw <- read.csv2(file.in.prediction,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Subset & reconstruct prediction data #######################################

## Subset prediction probabilities
df.predict.prob <- data.raw[grep("prob", data.raw$parameters), ]

## Subset prediction classes
df.predict.class <- data.raw[grep("class", data.raw$parameters), ] ## should be observed classes from test dataset!! not predicted classes....

## Reconstruct prediciton class factor levels
# for(i in 3:ncol(df.predict.class)) {
#   df.predict.class[, i] <- as.factor(ifelse(df.predict.class[, i] >= 1,
#                                             "yes","no"))
#   
#   df.predict.class[, i] <- factor(df.predict.class[, i], 
#                                   levels = c("yes", "no"))
#   
# }




### ROCR predcition ############################################################

pred <- prediction(predictions = df.predict.prob[, 3],
                   labels = df.predict.class[, 3])


### ROCR performance ###########################################################

perf <- performance(pred,"tpr","fpr") ## <-!!!!!
# perf <- performance(pred, "prec", "rec")
# perf <- performance(pred, "tpr")  # True positive rate. P(Yhat = + | Y = +). Estimated as: TP/P.
# perf <- performance(pred, "tnr")  # True negative rate. P(Yhat = - | Y = -).
# perf <- performance(pred, "fpr")  # False positive rate. P(Yhat = + | Y = -). Estimated as: FP/N.
# perf <- performance(pred, "fnr")  # False negative rate. P(Yhat = - | Y = +). Estimated as: FN/P.
# perf <- performance(pred, "acc")  # Accuracy. P(Yhat = Y). Estimated as: (TP+TN)/(P+N).
# perf <- performance(pred, "prec") # Precision. P(Y = + | Yhat = +). Estimated as: TP/(TP+FP).
# perf <- performance(pred, "err")  # Error rate. P(Yhat != Y). Estimated as: (FP+FN)/(P+N).
# perf <- performance(pred, "ppv")  # Positive predictive value. P(Y = + | Yhat = +). Estimated as: TP/(TP+FP).
# perf <- performance(pred, "phi")  # Phi correlation coefficient. (TP*TN - FP*FN)/(sqrt((TP+FN)*(TN+FP)*(TP+FP)*(TN+FN))). Yields a number between -1 and 1, with 1 indicating a perfect prediction, 0 indicating a random prediction. Values below 0 indicate a worse than random prediction.
# perf <- performance(pred, "cal")  # Calibration error. The calibration error is the absolute difference between predicted confidence and actual reliability. This error is estimated at all cutoffs by sliding a window across the range of possible cutoffs. The default window size of 100 can be adjusted by passing the optional parameter window.size=200 to performance. E.g., if for several positive samples the output of the classifier is around 0.75, you might expect from a well-calibrated classifier that the fraction of them which is correctly predicted as positive is also around 0.75. In a well-calibrated classifier, the probabilistic confidence estimates are realistic. Only for use with probabilistic output (i.e. scores between 0 and 1).
# perf <- performance(pred, "auc")  # Area under the ROC curve. This is equal to the value of the Wilcoxon-Mann-Whitney test statistic and also the probability that the classifier will score are randomly drawn positive sample higher than a randomly drawn negative sample. Since the output of auc is cutoff-independent, this measure cannot be combined with other measures into a parametric curve. The partial area under the ROC curve up to a given false positive rate can be calculated by passing the optional parameter fpr.stop=0.5 (or any other value between 0 and 1) to performance.


# perf <- performance(pred, "tnr")
# perf <- performance(pred, "auc")


unlist(perf)@y.values


## Define output image | open image port
# png(paste0(path.image, "lvl0400_ROC.png"), 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)
# 
plot(perf)
# 
# ## Close image port
# graphics.off()