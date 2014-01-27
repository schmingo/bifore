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

path.hdf <- "/home/schmingo/SAVE/Diplomarbeit/sample_myd02_hdf/"
path.tif <- "/home/schmingo/SAVE/Diplomarbeit/sample_myd02_tif/"

path.biodiversity.csv <- "csv/kili/lvl0100_biodiversity_data.csv"


################################################################################
### Import biodiversity dataset ################################################

data.bio.raw <- read.csv2(path.biodiversity.csv,
                          dec = ".",
                          header = TRUE, 
                          stringsAsFactors = FALSE)

data.bio.sp <- data.bio.raw


coordinates(data.bio.sp) <- c("lon", "lat")

################################################################################
### List .hdf and .tif for specific date #######################################

### Extract date from biodiversity data
tmp.date <- data.bio.raw$date_nocloud[1]

### Reformat date
tmp.date <- paste0(substr(tmp.date, 1, 4),
                   substr(tmp.date, 6, 8),
                   ".",
                   substr(tmp.date, 10, 13))

### List .tif files
lst.tif <- list.files(path.tif, pattern = tmp.date, full.names = TRUE)

### List .hdf files
lst.hdf.1km <- list.files(path.hdf, pattern = paste("1KM", tmp.date, sep = ".*"), full.names = TRUE)
lst.hdf.hkm <- list.files(path.hdf, pattern = paste("HKM", tmp.date, sep = ".*"), full.names = TRUE)
lst.hdf.qkm <- list.files(path.hdf, pattern = paste("QKM", tmp.date, sep = ".*"), full.names = TRUE)

lst.tif
lst.hdf.1km
lst.hdf.hkm
lst.hdf.qkm

## Import files as RasterLayer objects
lst.tif.raster <- lapply(lst.tif, raster)
projection.tif.raster <- CRS(projection(lst.tif.raster[[1]]))
