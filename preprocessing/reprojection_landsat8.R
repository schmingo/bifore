################################################################################
## BiFoRe Scripts
##
## REPROJECT LANDSAT8 Data
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-07-23
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("rgdal", "parallel", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
setwd("hier_kommt_der_Flo ;-)") # Linux
setwd("hier_kommt_der_Flo ;-)") # Windows


### Landsat data

## List files
fls.ls <- list.files("src/satellite/Landsat8_2013-07-07_hai/Level1_GeoTIFF_Data_Product/", 
                     pattern = ".TIF$", full.names = TRUE)

# ## Reorder files
# tmp <- sapply(strsplit(substr(basename(fls.ls), 1, nchar(basename(fls.ls)) - 4), "_"), "[[", 2)
# fls.ls <- fls.ls[order(as.numeric(substr(tmp, 2, nchar(tmp))))]

## Import files as RasterLayer objects
rst.ls <- lapply(fls.ls, raster)
prj.ls <- CRS(projection(rst.ls[[1]]))


### Station data

## List files
fls.ex <- list.files("src/csv/", pattern = ".csv$", full.names = TRUE)

## Import files as SpatialPointsDataframe objects
tbl.ex <- lapply(fls.ex, function(i) {
  tmp.tbl <- read.csv2(i, dec = ".", stringsAsFactors = FALSE)
  coordinates(tmp.tbl) <- c("Longitude", "Latitude")
  projection(tmp.tbl) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  tmp.tbl <- spTransform(tmp.tbl, CRS = prj.ls)
  
  return(tmp.tbl)
})


### Sugar

## Parallelization
clstr <- makePSOCKcluster(n.cores <- detectCores())
clusterExport(clstr, c("lib", "rst.ls"))
clusterEvalQ(clstr, lapply(lib, function(i) require(i, character.only = TRUE, quietly = TRUE)))

## Reproject and save rasters
rst.ls.rpj <- parLapply(clstr, rst.ls, function(i) {
  projectRaster(i, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), 
                filename = paste("src/satellite/Landsat8_2013-07-07_hai/Level1_GeoTIFF_Data_Product/", 
                                 substr(basename(rst.ls[[1]]@file@name), 1, nchar(basename(rst.ls[[1]]@file@name)) - 4), 
                                 "_longlat", sep = ""), overwrite = TRUE, format = "GTiff")
})

## Deregister parallel backend
stopCluster(clstr)