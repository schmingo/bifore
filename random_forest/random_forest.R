################################################################################
## BiFoRe Scripts
##
## RANDOM FOREST
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-08-13
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest", "foreach", "doSNOW", "parallel")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
#setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
#setwd("Florian")

## Import dataset
data <- read.csv2("src/csv/all_greyvalues_modis_NA_abundance.csv", 
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


### Random Forest

n.cores <- detectCores() # detect cpu cores for parallelization

