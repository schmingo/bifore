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


## Import biodiversity dataset as SpatialPointsDataframe objects
data.bio.sp <- data.bio.raw

coordinates(data.bio.sp) <- c("lon", "lat")
projection(data.bio.sp) <- "+init=epsg:4326"

################################################################################
### List .hdf and .tif for specific date and import .tif as RasterLayer Object##

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
  
  function.na <- function(x) {ifelse(x > 32767, x <- NA, x); return(x)}
  
  
  foreach (r = lst.tif.raster, n = lst.tif) %do% {
    calc(r, 
         fun = function.na, 
         filename = paste0(path.tif.na, basename(n)),
         overwrite = TRUE)
  }
  
  
  ################################################################################
  ### Extraction of radiance_scale and reflectance_scale from *.hdf ##############
  
  print(paste("Extract radiance_scale and reflectance_scale from original *.hdf for ", tmp.date))
  
  modscales <- hdfExtractMODScale (lst.hdf.qkm,
                                   lst.hdf.hkm,
                                   lst.hdf.1km)
  
  ################################################################################  
  ## Calculate new greyvalues (greyvalue * scalefactor) and write to new raster ##
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
  
  ## List calculated raster images
  lst.tif.calc <- list.files(path.tif.calc, pattern = tmp.date, full.names = TRUE)
  lst.tif.calc.raster <- lapply(lst.tif.calc, raster)
  
  greyvalues.raw <- foreach(i = seq(lst.tif.calc.raster))
  #   registerDoParallel(cl <- makeCluster(detectCores()))
  
  greyvalues.raw <- foreach(i = seq(lst.tif.calc.raster), .combine = "cbind") %do% {
    lst.tif.calc.raster[[i]][cellFromXY(lst.tif.calc.raster[[i]], data.bio.sp[1,])]
  }
  
  stopCluster(cl)
  
}  
################################################################################
### Check extracted cell values for NA #########################################

greyvalues.na <- greyvalues.raw
greyvalues.na[, 1:ncol(greyvalues.na)][greyvalues.na[, 1:ncol(greyvalues.na)] > 32767] <- NA

## Convert greyvalue matrices to data.frame
greyvalues.raw <- data.frame(greyvalues.raw, stringsAsFactors = F)
greyvalues.na <- data.frame(greyvalues.na, stringsAsFactors = F)



################################################################################

greyvalues.calc <- greyvalues.na * as.numeric(modscales[["scales"]])

## Calculate first derivate (diff)
diff <- as.data.frame(rowDiffs(as.matrix(greyvalues.calc)))
diff <- cbind(0,diff) # add "0-column" because there is no slope for the first greyvalue

################################################################################
### Pixelraster ################################################################

cells.1km <- cellFromXY(lst.tif.raster.1km[[1]], data.bio.sp[1,])
cells.hkm <- cellFromXY(lst.tif.raster.hkm[[1]], data.bio.sp[1,])
cells.qkm <- cellFromXY(lst.tif.raster.qkm[[1]], data.bio.sp[1,])

cells.adj.1km <- adjacent(lst.tif.raster.1km[[1]], cells.1km, 
                          directions = 8, 
                          pairs = FALSE, 
                          sorted = TRUE)

cells.adj.hkm <- adjacent(lst.tif.raster.hkm[[1]], cells.hkm, 
                          directions = matrix(c(1,1,1,1,1,
                                                1,1,1,1,1,
                                                1,1,0,1,1,
                                                1,1,1,1,1,
                                                1,1,1,1,1), ncol = 5), 
                          pairs = FALSE, 
                          sorted = TRUE)

cells.adj.qkm <- adjacent(lst.tif.raster.qkm[[1]], cells.qkm, 
                          directions = matrix(c(1,1,1,1,1,1,1,1,1,
                                                1,1,1,1,1,1,1,1,1,
                                                1,1,1,1,1,1,1,1,1,
                                                1,1,1,1,1,1,1,1,1,
                                                1,1,1,1,0,1,1,1,1,
                                                1,1,1,1,1,1,1,1,1,
                                                1,1,1,1,1,1,1,1,1,
                                                1,1,1,1,1,1,1,1,1,
                                                1,1,1,1,1,1,1,1,1), ncol = 9), 
                          pairs = FALSE, 
                          sorted = TRUE)




cells.1km.mtrx.sd <- sd(lst.tif.raster.1km[[1]][cells.adj.1km])
cells.hkm.mtrx.sd <- sd(cells.adj.hkm)
cells.qkm.mtrx.sd <- sd(cells.adj.qkm)
################################################################################


# ## combine dataframes
# greyvalues.calc.diff <- data.frame(t(cbind(t(greyvalues.calc), t(diff))))
# 
# ## Set colnames and rownames for new df
# colnames(greyvalues.calc.diff) <- paste0("band_", as.character(modscales[["bands"]]))
# row.names(greyvalues.calc.diff) <- c("greyvalues", "first_derivate")

tmp.bio.df <- cbind(data.bio.raw[1,], greyvalues.calc, diff)
colnames(tmp.bio.df)[69:106] <- paste0("greyval_band_", as.character(modscales[["bands"]]))
colnames(tmp.bio.df)[107:144] <- paste0("deriv_band_", as.character(modscales[["bands"]]))
rownames(tmp.bio.df) <- tmp.date
# 
# 
# ## End foreach loop
# 
bio.df.greyvalues <- data.frame(t(tmp.bio.df), 
                                stringsAsFactors = FALSE)

}