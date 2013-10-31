################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-31                                                        ##
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
data.modis <- read.csv2("csv/kili/MODIS_20060129_greyvalues_NA_derivate.csv",
                        dec = ".",
                        header = TRUE,
                        stringsAsFactors = FALSE
                        )

abundance <- read.csv2("csv/kili/kili_abundance.csv",
                       dec = ".",
                       header = TRUE,
                       stringsAsFactors = FALSE
                       )


################################################################################
### Combining and subsetting data ##############################################

tmp.abundance <- abundance[6]

## Modify abundance values - 2 digit numeric value
tmp.abundance.list <- as.list(as.numeric(t(tmp.abundance)))
tmp.abundance.list <- formatC(tmp.abundance.list, 
                              width = 2, 
                              format = "d", 
                              flag = "0")

## Modify abundance values - paste "A" in front to create a character
tmp.abundance <- as.data.frame(paste0("A", tmp.abundance.list))

names(tmp.abundance) <- "abundance"


## Select data for randomForest
attach(data.modis) 
tmp.data.modis <- data.frame(Plotid,
#                              Easting,
#                              Northing,
#                              Longitude,
#                              Latitude, 
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
#                              B12, 
#                              B13.1, 
#                              B13.2, 
#                              B14.1, 
#                              B14.2, 
#                              B15, 
#                              B16, 
#                              B17, 
                             B18, 
                             B19, 
                             B20, 
                             B21, 
                             B22, 
                             B23, 
                             B24, 
                             B25, 
                             B26, 
                             B27, 
                             B28, 
                             B29, 
                             B30, 
                             B31, 
                             B32, 
                             B33, 
                             B34, 
                             B35, 
                             B36 
                             )
detach(data.modis)
names(tmp.data.modis)

## Combine MODIS and abundance data 
train.data <- cbind(tmp.data.modis, tmp.abundance)
names(train.data)


## Remove rows with NA values
train.data <- na.omit(train.data)

################################################################################
### Random Forest ##############################################################

## Define desired parameters
n.tree <- 500 # Number of trees to grow
m.try <- 7 # Number of variables randomly sampled as candidates at each split


## Check colnames to get the right predictor values
names(train.data[,4:ncol(train.data)-2])
## Function 
train.rf <- randomForest(train.data[,4:ncol(train.data)-2],
                         train.data[,names(train.data) %in% c("abundance")],
                         importance = TRUE,
#                          na.action = na.omit(train.data),
#                          type="classification",
                         do.trace = 100)

print(train.rf)


################################################################################
### Prediction #################################################################

## Create test-df

# testData <- 
# 
# 
# ##  predict RInfo for new data set
# test.predict <- predict(train.rf, testData[,1:ncol(testData)])