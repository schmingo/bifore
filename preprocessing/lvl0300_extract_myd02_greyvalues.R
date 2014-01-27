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

## Import .tif files as RasterLayer objects
lst.tif.raster <- lapply(lst.tif, raster)
projection.tif.raster <- CRS(projection(lst.tif.raster[[1]]))


################################################################################
### Extraction of cell values ##################################################

registerDoParallel(cl <- makeCluster(detectCores()))

greyvalues.raw <- foreach(i = seq(lst.tif.raster), .packages = lib,
                          .combine = "cbind") %dopar% {
                            lst.tif.raster[[i]][cellFromXY(lst.tif.raster[[i]], data.bio.sp[1,])]
                          }

stopCluster(cl)


################################################################################
### Check extracted cell values for NA #########################################

greyvalues.na <- greyvalues.raw
greyvalues.na[, 1:ncol(greyvalues.na)][greyvalues.na[, 1:ncol(greyvalues.na)] > 32767] <- NA

## Convert greyvalue matrices to data.frame
greyvalues.raw <- data.frame(greyvalues.raw, stringsAsFactors = F)
greyvalues.na <- data.frame(greyvalues.na, stringsAsFactors = F)


################################################################################
### Extraction of radiance_scale and reflectance_scale from *.hdf ##############

print("Extract radiance_scale and reflectance_scale from original *.hdf")

modscales <- hdfExtractMODScale (lst.hdf.qkm,
                                 lst.hdf.hkm,
                                 lst.hdf.1km)

## Calculate new greyvalues (greyvalue * scalefactor)
greyvalues.calc <- greyvalues.na * as.numeric(modscales[["scales"]])

## Calculate first derivate (diff)
diff <- as.data.frame(rowDiffs(as.matrix(greyvalues.calc)))
diff <- cbind(0,diff) # add "0-column" because there is no slope for the first greyvalue

## combine dataframes
greyvalues.calc.diff <- data.frame(t(cbind(t(greyvalues.calc), t(diff))))

## Set colnames and rownames for new df
names(greyvalues.calc.diff) <- modscales[["bands"]]
row.names(greyvalues.calc.diff) <- c("greyvalues", "first_derivate")

## Testing rename colnames !!!BUG!!!
bands <- names(greyvalues.calc.diff)

for (i in bands) {
  bands[i] <- paste0("band_", i)
}

bands
