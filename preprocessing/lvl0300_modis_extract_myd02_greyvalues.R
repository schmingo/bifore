################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM MODIS SATELLITE DATA FOR EACH CSV OBSERVATION      ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-24                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("rgdal", "doParallel", "raster", "matrixStats")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")

################################################################################
### Set filepaths ##############################################################

path.hdf <- "/home/schmingo/SAVE/Diplomarbeit/myd02_hdf/"
path.tif <- "/home/schmingo/SAVE/Diplomarbeit/myd02_tif/"

path.biodiversity.csv <- "csv/kili/biodiversity_data_cloudchecked.csv"


################################################################################
### Import biodiversity dataset ################################################

data.bio.raw <- read.csv2(path.biodiversity.csv,
#                           dec = ".",
                          header = TRUE, 
                          stringsAsFactors = TRUE)

data.bio.raw$lat <- as.numeric(data.bio.raw$lat, dec = ".")
data.bio.raw$lon <- as.numeric(data.bio.raw$lon, dec = ".")


data.bio.sp <- data.bio.raw


coordinates(data.bio.sp) <- c("lon", "lat")

################################################################################
### 