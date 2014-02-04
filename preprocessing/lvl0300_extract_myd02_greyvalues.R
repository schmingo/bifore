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

path.hdf <- "/home/schmingo/Diplomarbeit/sample_myd02_hdf/"
path.tif <- "/home/schmingo/Diplomarbeit/sample_myd02_tif/"
path.tif.na <- "/home/schmingo/Diplomarbeit/sample_myd02_tif_na/"
path.tif.calc <- "/home/schmingo/Diplomarbeit/sample_myd02_tif_calc/"
path.biodiversity.csv <- "csv/kili/lvl0100_biodiversity_data.csv"

## Source modules
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0320_hdfExtractScales.R")

################################################################################
### Import biodiversity dataset ################################################

data.bio.raw <- read.csv2(path.biodiversity.csv,
                          dec = ".",
                          header = TRUE, 
                          stringsAsFactors = FALSE)


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
greyvalues <- foreach(a = lst.nocloud, b = seq(data.bio.sp), .combine = "rbind") %do% {
  
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
                               lst.tif.calc.raster[[r]][cellFromXY(lst.tif.calc.raster[[r]], data.bio.sp[b,])] ############# possible bug!! doublecheck if data.bio.sp[b,] is right!
                             }
  
}

greyvalues <- data.frame(greyvalues)

################################################################################
### Get bandnames for new df ###################################################

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

## Extract bandnames and scalefactor
modscales <- hdfExtractMODScale (lst.hdf.qkm,
                                 lst.hdf.hkm,
                                 lst.hdf.1km)


## Set names of greyvalues df
names(greyvalues) <- foreach(i = modscales$bands, j = names(greyvalues)) %do% {
  j <- paste0("greyval_band_", i)
}

names(greyvalues)


################################################################################
### Calculate first derivate (diff) ############################################

## Calculate diff
diff <- as.data.frame(rowDiffs(as.matrix(greyvalues)))
diff <- cbind(0,diff)

## Combine dataframes
greyvalues.diff <- cbind(greyvalues, diff)

## Set names for diffs
names(greyvalues.diff)[39:76] <- foreach(i = modscales$bands, 
                                         j = names(greyvalues.diff[39:76])) %do% {
                                           j <- paste0("deriv_band_", i)
                                         }

names(greyvalues.diff)[39:76]

################################################################################
### Pixelraster ################################################################
path.tif.calc <- "/home/schmingo/Diplomarbeit/sample_myd02_tif_calc/"

## List cloud-free dates from biodiversity dataset
lst.nocloud <- as.list(data.bio.raw$date_nocloud)

## Import biodiversity dataset as SpatialPointsDataframe objects
data.bio.sp <- data.bio.raw
coordinates(data.bio.sp) <- c("lon", "lat")
projection(data.bio.sp) <- "+init=epsg:4326"



pxl.matrix <- foreach(a = lst.nocloud, b = seq(data.bio.sp), .combine = "rbind") %do% {
  tmp.date <- a
  #   tmp.date <- data.bio.raw$date_nocloud[1]
  
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
  
  ## Extract cell values from all bands for each 1km raster
  pxl.rst.1km <- foreach(r = seq(lst.rst.1km), 
                         .combine = "cbind") %do% {
                           cells.1km <- cellFromXY(lst.rst.1km[[r]], data.bio.sp[b,])
                           
                           cells.adj.1km <- adjacent(lst.rst.1km[[r]], cells.1km, 
                                                     directions = 8, 
                                                     pairs = FALSE, 
                                                     sorted = TRUE)
                           
                           cells.1km.mtrx.sd <- sd(lst.rst.1km[[r]][cells.adj.1km])
                         }
  
  ## Extract cell values from all bands for each hkm raster
  pxl.rst.hkm <- foreach(r = seq(lst.rst.hkm), 
                         .combine = "cbind") %do% {
                           cells.hkm <- cellFromXY(lst.rst.hkm[[r]], data.bio.sp[b,])
                           
                           cells.adj.hkm <- adjacent(lst.rst.hkm[[r]], cells.hkm, 
                                                     directions = matrix(c(1,1,1,1,1,
                                                                           1,1,1,1,1,
                                                                           1,1,0,1,1,
                                                                           1,1,1,1,1,
                                                                           1,1,1,1,1), ncol = 5), 
                                                     pairs = FALSE, 
                                                     sorted = TRUE)
                           
                           cells.hkm.mtrx.sd <- sd(lst.rst.hkm[[r]][cells.adj.hkm])
                         }
  
  ## Extract cell values from all bands for each qkm raster
  pxl.rst.qkm <- foreach(r = seq(lst.rst.qkm), 
                         .combine = "cbind") %do% {
                           cells.qkm <- cellFromXY(lst.rst.qkm[[r]], data.bio.sp[b,])
                           
                           cells.adj.qkm <- adjacent(lst.rst.qkm[[r]], cells.qkm, 
                                                     directions = matrix(c(1,1,1,1,1,1,1,1,1,1,1,
                                                                           1,1,1,1,1,1,1,1,1,1,1,
                                                                           1,1,1,1,1,1,1,1,1,1,1,
                                                                           1,1,1,1,1,1,1,1,1,1,1,
                                                                           1,1,1,1,1,0,1,1,1,1,1,
                                                                           1,1,1,1,1,1,1,1,1,1,1,
                                                                           1,1,1,1,1,1,1,1,1,1,1,
                                                                           1,1,1,1,1,1,1,1,1,1,1,
                                                                           1,1,1,1,1,1,1,1,11,1,), ncol = 11),  
                                                     pairs = FALSE, 
                                                     sorted = TRUE)
                           
                           cells.qkm.mtrx.sd <- sd(lst.rst.qkm[[r]][cells.adj.qkm])
                         }
}

# cells.1km <- cellFromXY(lst.rst.1km[[1]], data.bio.sp[1,])
# cells.hkm <- cellFromXY(lst.tif.raster.hkm[[1]], data.bio.sp[1,])
# cells.qkm <- cellFromXY(lst.tif.raster.qkm[[1]], data.bio.sp[1,])
# 
# cells.adj.1km <- adjacent(lst.tif.raster.1km[[1]], cells.1km, 
#                           directions = 8, 
#                           pairs = FALSE, 
#                           sorted = TRUE)
# 
# cells.adj.hkm <- adjacent(lst.tif.raster.hkm[[1]], cells.hkm, 
#                           directions = matrix(c(1,1,1,1,1,
#                                                 1,1,1,1,1,
#                                                 1,1,0,1,1,
#                                                 1,1,1,1,1,
#                                                 1,1,1,1,1), ncol = 5), 
#                           pairs = FALSE, 
#                           sorted = TRUE)
# 
# cells.adj.qkm <- adjacent(lst.tif.raster.qkm[[1]], cells.qkm, 
#                           directions = matrix(c(1,1,1,1,1,1,1,1,1,1,1,
#                                                 1,1,1,1,1,1,1,1,1,1,1,
#                                                 1,1,1,1,1,1,1,1,1,1,1,
#                                                 1,1,1,1,1,1,1,1,1,1,1,
#                                                 1,1,1,1,1,0,1,1,1,1,1,
#                                                 1,1,1,1,1,1,1,1,1,1,1,
#                                                 1,1,1,1,1,1,1,1,1,1,1,
#                                                 1,1,1,1,1,1,1,1,1,1,1,
#                                                 1,1,1,1,1,1,1,1,11,1,), ncol = 11), 
#                           pairs = FALSE, 
#                           sorted = TRUE)
# 
# 
# 
# 
# cells.1km.mtrx.sd <- sd(lst.tif.raster.1km[[1]][cells.adj.1km])
# cells.hkm.mtrx.sd <- sd(cells.adj.hkm)
# cells.qkm.mtrx.sd <- sd(cells.adj.qkm)
# ################################################################################
# 
# 
# # ## combine dataframes
# # greyvalues.calc.diff <- data.frame(t(cbind(t(greyvalues.calc), t(diff))))
# # 
# # ## Set colnames and rownames for new df
# # colnames(greyvalues.calc.diff) <- paste0("band_", as.character(modscales[["bands"]]))
# # row.names(greyvalues.calc.diff) <- c("greyvalues", "first_derivate")
# 
# tmp.bio.df <- cbind(data.bio.raw[1,], greyvalues.calc, diff)
# colnames(tmp.bio.df)[69:106] <- paste0("greyval_band_", as.character(modscales[["bands"]]))
# colnames(tmp.bio.df)[107:144] <- paste0("deriv_band_", as.character(modscales[["bands"]]))
# rownames(tmp.bio.df) <- tmp.date
# # 
# # 
# # ## End foreach loop
# # 
# bio.df.greyvalues <- data.frame(t(tmp.bio.df), 
#                                 stringsAsFactors = FALSE)
# 
# }