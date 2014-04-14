################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-08                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
cat("\014")
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("csv/kili/lvl0400_rf_number-of-species_10.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)

data <- data.raw


################################################################################
### Combining data for randomForest ############################################
################################################################################

## split incoming dataset
tmp.speciesnr <- data[13]
names(tmp.speciesnr) <- "SpeciesNr"

summary(tmp.speciesnr)

df.greyval <- data[14:43]
df.diff <- data[44:71]
df.sd <- data[72:101]


## Create multiple dataframes with species number as predictor datasets
df.spnr.greyval <- cbind(df.greyval, tmp.speciesnr)
df.spnr.diff <- cbind(df.diff, tmp.speciesnr)
df.spnr.greyval.diff <- cbind(df.greyval, df.diff, tmp.speciesnr)
df.spnr.greyval.sd <- cbind(df.greyval, df.sd, tmp.speciesnr)
df.spnr.diff.sd <- cbind(df.diff, df.sd, tmp.speciesnr)


## Define Random Forest input data #############################################
df.input.rf <- df.spnr.greyval ## Insert input dataset here!


################################################################################
### Random sample ##############################################################
################################################################################

## Split dataset | 3/4 train.data, 1/4 test.data      
set.seed(50)

index <- sample(1:nrow(df.input.rf), nrow(df.input.rf)*.75)
length(index)

train.data <- df.input.rf[index, ]
test.data <- df.input.rf[-index, ]

nrow(train.data)
nrow(test.data)


################################################################################
### Random Forest function #####################################################
### Regression - number of species #############################################
################################################################################

predictor_modisVAL <- train.data[,1:ncol(train.data)-1]
response_speciesNR <- train.data[,ncol(train.data)]

## Function ####################################################################
train.rf <- randomForest(x = predictor_modisVAL,
                         y = response_speciesNR,
                         importance = TRUE,
                         ntree = 500,
                         mtry = 5,
                         nodesize = 2,
                         type="regression",
                         do.trace = 100)
print(train.rf)

## Define output image | open image port
# png("images/rf_speciesno_regression_10.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot(train.rf, main = "Number of species (10)\n RandomForest regression \n Mean squared error")

## Close image port
# graphics.off()


print(train.rf)


################################################################################
### Prediction #################################################################
################################################################################

# ##  predict RInfo for new data set
# test2.predict <- predict(train.rf, test.data,index=2, na.rm=TRUE, progress="window", overwrite=TRUE, filename="ProbPred.png")
test.predict <- data.frame(predict(train.rf, test.data))
predict.compare <- cbind(test.data[,ncol(test.data)], 
                         test.predict,
                         abs(test.predict-test.data[,ncol(test.data)]))

names(predict.compare) <- c("real.values", "predicted.values", "diff.abs")

summary(predict.compare$diff)


################################################################################
### Further interpretation #####################################################
################################################################################

varimp <- importance(train.rf)


## Define output image | open image port
# png("images/rf_speciesno_regression_varImp_10.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

varimp.plot <- varImpPlot(train.rf, sort = TRUE, n.var = 30,
                          main = paste0("Number of species - Variable importance (10)"))

## Close image port
# graphics.off()

## The first measure is computed from permuting OOB data: For each tree, 
## the prediction error on the out-of-bag portion of the data is recorded 
## (error rate for classification, MSE for regression). Then the same is 
## done after permuting each predictor variable. The difference between 
## the two are then averaged over all trees, and normalized by 
## the standard deviation of the differences. If the standard deviation 
## of the differences is equal to 0 for a variable, the division is 
## not done (but the average is almost always equal to 0 in that case).
## 
## The second measure is the total decrease in node impurities from 
## splitting on the variable, averaged over all trees. 
## For classification, the node impurity is measured by the Gini index. 
## For regression, it is measured by residual sum of squares.

