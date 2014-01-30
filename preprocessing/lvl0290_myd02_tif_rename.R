################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Rename MYD02 .tif files to get actual bandname                             ##
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

path.hdf <- "/media/schmingo/SIMON_1TB/Diplomarbeit/sample_myd02_hdf/"
path.tif <- "/media/schmingo/SIMON_1TB/Diplomarbeit/sample_myd02_tif/"
path.tif.calc <- "/media/schmingo/SIMON_1TB/Diplomarbeit/sample_myd02_tif_calc/"

path.biodiversity.csv <- "csv/kili/lvl0100_biodiversity_data.csv"

## Source modules
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0320_hdfExtractScales.R")

################################################################################
### Import biodiversity dataset ################################################

data.bio.raw <- read.csv2(path.biodiversity.csv,
                          dec = ".",
                          header = TRUE, 
                          stringsAsFactors = FALSE)


## Import biodiversity dataset as SpatialPointsDataframe objects
data.bio.sp <- data.bio.raw

coordinates(data.bio.sp) <- c("lon", "lat")
projection(data.bio.sp) <- "+init=epsg:4326"

################################################################################
### List .hdf and .tif for specific date and import .tif as RasterLayer Object##



## Begin foreach loop



### Extract date from biodiversity data
tmp.date <- data.bio.raw$date_nocloud[1]

### Reformat date
tmp.date <- paste0(substr(tmp.date, 1, 4),
                   substr(tmp.date, 6, 8),
                   ".",
                   substr(tmp.date, 10, 13))

### List .tif files
lst.tif <- list.files(path.tif, pattern = tmp.date, full.names = TRUE)