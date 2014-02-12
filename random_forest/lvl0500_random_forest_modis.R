################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-12                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Import dataset
train.data <- read.csv2("csv/kili/lvl0400_speciesNR.csv",
                    dec = ".",
                    header = TRUE,
                    stringsAsFactors = TRUE)


################################################################################
### Random Forest ##############################################################

# ## Define desired parameters
n.tree <- 500 # Number of trees to grow
m.try <- 7 # Number of variables randomly sampled as candidates at each split
# 
# 
# ## Check colnames to get the right predictor values
names(train.data[,5:ncol(train.data)-1])

## Function 
train.rf <- randomForest(x = train.data[,5:ncol(train.data)-1],
                         y = train.data[,names(train.data) %in% c("SpeciesNr")],
                         importance = TRUE,
#                          na.action = na.omit(train.data),
#                          type="classification",
                         do.trace = 100)

print(train.rf)


################################################################################
### Prediction #################################################################

## Create test-df

testData <- train.data[1:180,]


##  predict RInfo for new data set
test.predict <- predict(train.rf, testData[,1:ncol(testData)],type="prob", index=2, na.rm=TRUE, progress="window", overwrite=TRUE, filename="ProbPred.png")
test.predict <- predict(train.rf, testData[,1:ncol(testData)], type="vote")
test.predict
