################################################################################
## BiFoRe Scripts
##
## RANDOM FOREST
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-09-23
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Google Drive/bifore/")

## Import dataset
data <- read.csv2("src/csv/all_MODIS_20130706-1040_greyvalues_NA_abundance.csv", 
                  dec = ".", header = TRUE, stringsAsFactors = FALSE)

## Select data for randomForest
attach(data) 
train.data <- data.frame(Plotname, 
                        #Plotid, 
                        #Status, 
                        #Location, 
                        #Longitude, 
                        #Latitude, 
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
                        B12, 
                        B13.1, 
                        B13.2, 
                        B14.1, 
                        B14.2, 
                        B15, 
                        B16, 
                        B17, 
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
                        B36, 
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