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

## Set species to test
species <- "Pnorisa.squalus"


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("csv/kili/lvl0400_rf_strat_prevalence_all.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Combining data for randomForest ############################################
################################################################################

## Split incoming dataset
df.greyval <- data.raw[178:207]  ## greyvalues
df.diff <- data.raw[208:235]  ## diff
df.sd <- data.raw[236:265]  ## sd 

## Select species data
df.species <- data.frame(data.raw[,names(data.raw) %in% c(species)])
names(df.species) <- species

tmp.species <- df.species

summary(tmp.species)

## Create multiple dataframes with single species as predictor datasets
df.spec.greyval <- cbind(df.greyval, tmp.species)
df.spec.diff <- cbind(df.diff, tmp.species)
df.spec.greyval.diff <- cbind(df.greyval, df.diff, tmp.species)
df.spec.greyval.sd <- cbind(df.greyval, df.sd, tmp.species)
df.spec.diff.sd <- cbind(df.diff, df.sd, tmp.species)


## Define Random Forest input data #############################################
df.input.rf <- df.spec.greyval ## Insert input dataset here!


################################################################################
### Random sample ##############################################################
################################################################################

## Split dataset | 3/4 train.data, 1/4 test.data      
set.seed(50)

index <- sample(1:nrow(df.input.rf), nrow(df.input.rf)*.75)
length(index)
# index

train.data <- df.input.rf[index, ]
test.data <- df.input.rf[-index, ]

nrow(train.data)
nrow(test.data)


################################################################################
### Random Forest function #####################################################
### Classification - single species ############################################
################################################################################

predictor_modisVAL <- train.data[,1:ncol(train.data)-1]
response_speciesCLASS <- as.factor(train.data[,ncol(train.data)])

## Function ####################################################################
train.rf <- randomForest(x = predictor_modisVAL,
                         y = response_speciesCLASS,
                         importance = TRUE,
                         ntree = 500,
                         mtry = 2,
                         nodesize = 2,
                         type="classification",
                         do.trace = 100)

## Define output image | open image port
# png(paste0("images/rf_", species, "_classification_prevalence.png"), 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

rf.plot <- plot(train.rf, main = paste0("Prevalence ", species, 
                                        "\n RandomForest classification \n", 
                                        "Mean squared error"))

rf.plot

## Close image port
graphics.off()

print(train.rf)

print("Observed classes")
table(tmp.species)


################################################################################
### Prediction #################################################################
################################################################################


predict.df <- data.frame(predict(train.rf, test.data))

predict.df.comparison <- cbind(test.data[,ncol(test.data)], 
                               predict.df)

names(predict.df.comparison) <- c("real.classes", "predicted.classes")

predict.df.comparison

## Calculation of identical classes
notidentical <- which(test.data[,ncol(test.data)] != predict.df)
print(paste0((1-length(notidentical)/nrow(test.data))*100, "% of predicted classes are identical to real classification."))

################################################################################
### Further interpretation #####################################################
################################################################################

varimp <- importance(train.rf)


## Define output image | open image port
# png(paste0("images/rf_", species, "_classification_prevalence_varImp.png"), 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

varimp.plot <- varImpPlot(train.rf, sort = TRUE, n.var = 30,
                          main = paste0(species," - Variable importance"))
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

