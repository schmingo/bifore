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

path.biodiversity.csv <- "csv/kili/biodiversity_data_subset.csv"


################################################################################
### Import biodiversity dataset ################################################

data.bio <- read.csv2(path.biodiversity.csv,
                      dec = ".",
                      header = TRUE, 
                      stringsAsFactors = TRUE)

################################################################################
### 