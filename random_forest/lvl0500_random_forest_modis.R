################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-05                                                        ##
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
                    dec = ",",
                    header = TRUE,
                    stringsAsFactors = FALSE)



################################################################################
### Combining and subsetting data ##############################################

# ## Select data for randomForest
# attach(data) 
# tmp.data <- data.frame(Plotid,
# #                        Easting,
# #                        Northing,
# #                        Longitude,
# #                        Latitude, 
#                        B01, 
#                        B02, 
#                        B03, 
#                        B04, 
#                        B05, 
#                        B06, 
#                        B07, 
#                        B08, 
#                        B09, 
#                        B10, 
#                        B11, 
# #                        B12, 
# #                        B13.1, 
# #                        B13.2, 
# #                        B14.1, 
# #                        B14.2, 
# #                        B15, 
# #                        B16, 
# #                        B17, 
#                        B18, 
#                        B19, 
#                        B20, 
#                        B21, 
#                        B22, 
#                        B23, 
#                        B24, 
#                        B25, 
#                        B26, 
#                        B27, 
#                        B28, 
#                        B29, 
#                        B30, 
#                        B31, 
#                        B32, 
#                        B33, 
#                        B34, 
#                        B35, 
#                        B36 
#                        )
# detach(data)
# names(tmp.data)

# ## Combine MODIS and abundance data 
# train.data <- cbind(data.greyval, tmp.speciesnr)
# names(train.data)
# 
# 
# ## Deal with NA values using rfImpute function
# ?rfImpute
# 
# n.tree <- 500 # Number of trees to grow
# m.try <- 7 # Number of variables randomly sampled as candidates at each split
# 
# train.rf.imp <- rfImpute(x = train.data[,3:ncol(train.data)-1],
#                          y = train.data[,names(train.data) %in% c("SpeciesNr")],
#                          ntree = 500,
#                          iter = 5,
#                          mtry = 7,
#                          do.trace = 100)
# 
# print(train.rf.imp)
# # see http://stackoverflow.com/questions/8370455/how-to-use-random-forests-in-r-with-missing-values

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
train.rf <- randomForest(train.data[,5:ncol(train.data)-1],
                         train.data[,names(train.data) %in% c("SpeciesNr")],
                         importance = TRUE,
                         na.action = na.omit(train.data),
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
# test.predict <- predict(train.rf.imp, testData[,1:ncol(testData)])