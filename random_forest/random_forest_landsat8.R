################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST USING LANDSAT8 DATA                                          ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-02                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Google Drive/bifore/") # Linux
setwd("D:/Dropbox/Diplomarbeit/code/bifore/") # Windows

## Import dataset
data <- read.csv2("src/csv/hai/hai_greyvalues_landsat8_abundance.csv", 
                  dec = ".", header = TRUE, stringsAsFactors = FALSE)

## Select data for randomForest
attach(data) 
train.data <- data.frame(Plotname,
#                          Plotid,
#                          Status,
#                          Location,
#                          Longitude,
#                          Latitude,
                         B01,
                         B02,
                         B03,
                         B04,
                         B05,
                         B06,
                         B07,
                         B08,
                         B09,
                         B10,
                         B11,
                         BQA,
                         abundance
                         )
detach(data)
names(train.data)
################################################################################
### modify ?predictor variable #################################################

# Add character to numerical data and write it into a new column. 
# If Abundance would be a numerical value, rF would automatically perform a
# regression instead of a classification.

abundance.char <- paste(train.data$abundance, "a", sep = "") 
train.data <- cbind(train.data,abundance.char)

## Remove rows with NA values
train.data <- na.omit(train.data)


################################################################################
### Random Forest ##############################################################

## Define desired parameters
n.tree <- 500 # Number of trees to grow
m.try <- 7 # Number of variables randomly sampled as candidates at each split

## Function 
train.rf <- randomForest(train.data[,3:ncol(train.data)-1],
                         train.data[ ,names(train.data) %in% c("abundance.char")],
                         importance=TRUE,
                         na.action=na.omit,
                         #type="classification",
                         do.trace=100)

print(train.rf)


################################################################################
### Prediction #################################################################

## Create test-df

# testData <- 
# 
# 
# ##  predict RInfo for new data set
# test.predict <- predict(train.rf, testData[,1:ncol(testData)])