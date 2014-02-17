################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-17                                                        ##
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
                    stringsAsFactors = FALSE)

################################################################################
### Subsetting data ############################################################

## Predictor
train.greyval <- train.data[,4:33]
names(train.greyval)

train.diff <- train.data[,34:61]
names(train.diff)

train.diff.sd <- train.data[,35:ncol(train.data)-1]
names(train.diff.sd)

train.greyval.sd <- cbind(train.data[,4:33], train.data[62:91])
names(train.greyval.sd)

## Response
response <- as.factor(train.data[,names(train.data) %in% c("SpeciesNr")])
names(response)



################################################################################
### Random Forest ##############################################################

# ## Define desired parameters
n.tree <- 500 # Number of trees to grow
m.try <- 4 # Number of variables randomly sampled as candidates at each split


## Function 
train.rf <- randomForest(x = train.diff.sd,
                         y = response,
                         importance = TRUE,
                         ntree = n.tree,
                         mtry = m.try,
                         nodesize = 2,
#                          na.action = na.omit(train.data),
                         type="classification",
                         do.trace = 100)

print(train.rf)


################################################################################
### Prediction #################################################################

## Create test-df
names(train.data[1:180,5:ncol(train.data)-1])
testData <- train.data[181:225,3:ncol(train.data)-1]


##  predict RInfo for new data set
test.predict <- predict(train.rf, testData,type="prob", index=2, na.rm=TRUE, progress="window", overwrite=TRUE, filename="ProbPred.png")
test.predict <- data.frame(predict(train.rf, testData))
test.predict

