################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM MODIS SATELLITE DATA FOR EACH CSV OBSERVATION      ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-04                                                        ##
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

path.hdf <- "/home/schmingo/Diplomarbeit/myd02_hdf/"
path.tif <- "/home/schmingo/Diplomarbeit/myd02_tif/"
path.tif.na <- "/home/schmingo/Diplomarbeit/myd02_tif_na/"
path.tif.calc <- "/home/schmingo/Diplomarbeit/myd02_tif_calc/"
path.biodiversity.csv <- "csv/kili/lvl0100_biodiversity_data.csv"

path.biodiversity.csv.out <- "csv/kili/lvl0300_biodiversity_data.csv"

## Source modules
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0320_hdfExtractScales.R")

################################################################################
### Import biodiversity dataset ################################################

data.bio.raw <- read.csv2(path.biodiversity.csv,
                          dec = ".",
                          header = TRUE, 
                          stringsAsFactors = FALSE)

## Check actual time
starttime <- Sys.time()

################################################################################
### Get MYD bandnames ##########################################################

## Set date for extraction
tmp.date <- data.bio.raw$date_nocloud[1]

## Reformat date
tmp.date <- paste0(substr(tmp.date, 1, 4),
                   substr(tmp.date, 6, 8),
                   ".",
                   substr(tmp.date, 10, 13))

lst.hdf.1km <- list.files(path.hdf, 
                          pattern = paste("1KM", tmp.date, sep = ".*"), 
                          full.names = TRUE)
lst.hdf.hkm <- list.files(path.hdf, 
                          pattern = paste("HKM", tmp.date, sep = ".*"), 
                          full.names = TRUE)
lst.hdf.qkm <- list.files(path.hdf, 
                          pattern = paste("QKM", tmp.date, sep = ".*"), 
                          full.names = TRUE)

## Extract bandnames from *.hdf files
bandnames <- hdfExtractMODScale (lst.hdf.qkm,
                                 lst.hdf.hkm,
                                 lst.hdf.1km)
bandnames <- bandnames$bands



################################################################################
### List .hdf and .tif for specific date and import .tif as RasterLayer Object##

## List unique cloudless dates
lst.date <- unique(data.bio.raw$date_nocloud)
lst.date

foreach(a = lst.date) %do% {
  
  ## Extract date from biodiversity data
  tmp.date <- a
  #     tmp.date <- data.bio.raw$date_nocloud[1]
  
  ## Reformat date
  tmp.date <- paste0(substr(tmp.date, 1, 4),
                     substr(tmp.date, 6, 8),
                     ".",
                     substr(tmp.date, 10, 13))
  
  ## List .tif files
  lst.tif <- list.files(path.tif, pattern = tmp.date, full.names = TRUE)
  
  ## Import .tif files as RasterLayer objects
  lst.tif.raster <- lapply(lst.tif, raster)
  
  ### List .hdf files
  lst.hdf.1km <- list.files(path.hdf, 
                            pattern = paste("1KM", tmp.date, sep = ".*"), 
                            full.names = TRUE)
  lst.hdf.hkm <- list.files(path.hdf, 
                            pattern = paste("HKM", tmp.date, sep = ".*"), 
                            full.names = TRUE)
  lst.hdf.qkm <- list.files(path.hdf, 
                            pattern = paste("QKM", tmp.date, sep = ".*"), 
                            full.names = TRUE)
  
  
  ################################################################################
  ### Check raster for NA values #################################################
  
  print(paste0(tmp.date, " - Check raw raster files for NA values"))
  
  function.na <- function(x) {ifelse(x > 32767, x <- NA, x); return(x)}
  
  
  foreach (r = lst.tif.raster, n = lst.tif) %do% {
    calc(r, 
         fun = function.na, 
         filename = paste0(path.tif.na, basename(n)),
         overwrite = TRUE)
  }
  
  
  ################################################################################
  ### Extraction of radiance_scale and reflectance_scale from *.hdf ##############
  
  print(paste0(tmp.date, " - Extract scalefactors from *.hdf"))
  
  modscales <- hdfExtractMODScale (lst.hdf.qkm,
                                   lst.hdf.hkm,
                                   lst.hdf.1km)
  
  ################################################################################  
  ## Calculate new greyvalues (greyvalue * scalefactor) and write to new raster ##
  
  print(paste0(tmp.date, " - Calculate new greyvalues"))
  
  lst.tif.na <- list.files(path.tif.na, pattern = tmp.date, full.names = TRUE)
  lst.tif.na.raster <- lapply(lst.tif.na, raster)
  
  
  foreach(c = as.list(lst.tif.na.raster), 
          d = as.list(as.numeric(modscales[["scales"]])),
          g = as.list(lst.tif.na)) %do% {
            calc(c, fun = function(x) x * d, 
                 filename = paste0(path.tif.calc, basename(g)), 
                 format = "GTiff", 
                 overwrite = TRUE)        
          }
}  


################################################################################
### Extraction of cell values ##################################################

## List cloud-free dates from biodiversity dataset
lst.nocloud <- as.list(data.bio.raw$date_nocloud)

## Import biodiversity dataset as SpatialPointsDataframe objects
data.bio.sp <- data.bio.raw
coordinates(data.bio.sp) <- c("lon", "lat")
projection(data.bio.sp) <- "+init=epsg:4326"

# Extract date from biodiversity data
# tmp.date <- data.bio.raw$date_nocloud[1]
greyvalues <- foreach(a = lst.nocloud, b = seq(nrow(data.bio.sp)), .combine = "rbind") %do% {
  
  tmp.date <- a
  
  ## Reformat date
  tmp.date <- paste0(substr(tmp.date, 1, 4),
                     substr(tmp.date, 6, 8),
                     ".",
                     substr(tmp.date, 10, 13)) 
  
  
  ## List calculated raster images
  lst.tif.calc <- list.files(path.tif.calc, pattern = tmp.date, full.names = TRUE)
  lst.tif.calc.raster <- lapply(lst.tif.calc, raster)
  
  ## Extract cell values from all bands
  greyvalues.calc <- foreach(r = seq(lst.tif.calc.raster), 
                             .combine = "cbind") %do% {
                               lst.tif.calc.raster[[r]][cellFromXY(lst.tif.calc.raster[[r]], data.bio.sp[b,])]
                             }
  
}

greyvalues <- data.frame(greyvalues)

## Set names of greyvalues df
names(greyvalues) <- foreach(i = bandnames, j = names(greyvalues)) %do% {
  j <- paste0("greyval_band_", i)
}

names(greyvalues)


################################################################################
### Calculate first derivate (diff) ############################################

## Calculate diff
diff <- as.data.frame(rowDiffs(as.matrix(greyvalues)))
diff <- cbind(0,diff)

## Set names for diffs
names(diff) <- foreach(i = bandnames, 
                       j = names(diff)) %do% {
                         j <- paste0("deriv_band_", i)
                       }

names(diff)


################################################################################
### Pixelraster ################################################################

## List cloud-free dates from biodiversity dataset
lst.nocloud <- as.list(data.bio.raw$date_nocloud)

## Import biodiversity dataset as SpatialPointsDataframe objects
data.bio.sp <- data.bio.raw
coordinates(data.bio.sp) <- c("lon", "lat")
projection(data.bio.sp) <- "+init=epsg:4326"



matrix.sd <- foreach(a = lst.nocloud, b = seq(nrow(data.bio.sp)), .combine = "rbind") %do% {
  
  tmp.date <- a
  
  ## Reformat date
  tmp.date <- paste0(substr(tmp.date, 1, 4),
                     substr(tmp.date, 6, 8),
                     ".",
                     substr(tmp.date, 10, 13)) 
  
  
  ## List calculated tifs
  lst.tif.calc.1km <- list.files(path.tif.calc, pattern = paste("1KM", tmp.date, sep = ".*"), full.names = TRUE)
  lst.tif.calc.hkm <- list.files(path.tif.calc, pattern = paste("HKM", tmp.date, sep = ".*"), full.names = TRUE)
  lst.tif.calc.qkm <- list.files(path.tif.calc, pattern = paste("QKM", tmp.date, sep = ".*"), full.names = TRUE)
  
  ## Import listed tifs as raster images
  lst.rst.1km <- lapply(lst.tif.calc.1km, raster)
  lst.rst.hkm <- lapply(lst.tif.calc.hkm, raster)
  lst.rst.qkm <- lapply(lst.tif.calc.qkm, raster)
  
  
  ## Get pixel matrix indices for 1km resolution
  cells.1km <- cellFromXY(lst.rst.1km[[1]], data.bio.sp[b,])
  cells.adj.1km <- adjacent(lst.rst.1km[[1]], cells.1km, 
                            directions = 8, 
                            pairs = FALSE, 
                            sorted = TRUE)
  
  ## Get pixel matrix indices for 500m resolution
  cells.hkm <- cellFromXY(lst.rst.hkm[[1]], data.bio.sp[b,])
  cells.adj.hkm <- adjacent(lst.rst.hkm[[1]], cells.hkm, 
                            directions = matrix(c(1,1,1,1,1,
                                                  1,1,1,1,1,
                                                  1,1,0,1,1,
                                                  1,1,1,1,1,
                                                  1,1,1,1,1), 
                                                ncol = 5), 
                            pairs = FALSE, 
                            sorted = TRUE)
  
  ## Get pixel matrix indices for 250m resolution
  cells.qkm <- cellFromXY(lst.rst.qkm[[1]], data.bio.sp[b,])
  cells.adj.qkm <- adjacent(lst.rst.qkm[[1]], cells.qkm, 
                            directions = matrix(c(1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,0,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1), 
                                                ncol = 11),  
                            pairs = FALSE, 
                            sorted = TRUE)
  
  
  ## Calculate matrix sd for each 1km raster image
  pxl.rst.1km <- foreach(r = seq(lst.rst.1km), .combine = "cbind") %do% {
    cells.1km.sd <- sd(lst.rst.1km[[r]][cells.adj.1km])
    return(cells.1km.sd)
  }
  
  ## Calculate matrix sd for each 500m raster image
  pxl.rst.hkm <- foreach(r = seq(lst.rst.hkm), .combine = "cbind") %do% {
    cells.hkm.sd <- sd(lst.rst.hkm[[r]][cells.adj.hkm])
    return(cells.hkm.sd)
  }
  
  ## Calculate matrix sd for each 250m raster image
  pxl.rst.qkm <- foreach(r = seq(lst.rst.qkm), .combine = "cbind") %do% {
    cells.qkm.sd <- sd(lst.rst.qkm[[r]][cells.adj.qkm])
    return(cells.qkm.sd)
  }
  
  ## Combine calculated sd in a single row
  row.sd <- cbind(pxl.rst.qkm, pxl.rst.hkm, pxl.rst.1km)
  return(row.sd)
}

## Write calculated values to new df
matrix.sd <- data.frame(matrix.sd)

## Set names of matrix.sd df
names(matrix.sd) <- foreach(i = bandnames, j = names(matrix.sd)) %do% {
  j <- paste0("sd_band_", i)
}

names(matrix.sd)


################################################################################
### Combine dataframes #########################################################

greyvalues.diff.sd <- data.frame(t(cbind(data.bio.raw, 
                                         greyvalues, 
                                         diff, 
                                         matrix.sd)), 
                                 stringsAsFactors = FALSE)

## Set colnames
names(greyvalues.diff.sd) <- data.bio.raw$date_nocloud

write.table(greyvalues.diff.sd, 
            file = path.biodiversity.csv.out,
            dec = ".",
            quote = FALSE,
            col.names = TRUE,
            row.names = TRUE,
            sep = ";")

################################################################################
### Check actual time again ####################################################

endtime <- Sys.time()

time <- endtime - starttime
time
