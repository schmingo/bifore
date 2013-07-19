##########################
## Reproject MODIS Data ##
##########################

## Clear workspace
rm(list = ls(all = TRUE))

## set working directory
setwd("/home/schmingo/Diplomarbeit/") #Linux
setwd("D:/Diplomarbeit/") #Windows

## Load Libraries
lib <- c("rgdal", "raster", "parallel")
lapply(lib, function(...) require(..., character.only = TRUE))


### MODIS Data ###

## List MODIS files
files.list <- list.files("src/satellite/MODIS_2013-07-07_hai_sch/", 
                     pattern = ".tif", full.names = TRUE)

## Import MODIS files as RasterLayer objects
raster.list <- lapply(files.list, raster)
proj.list <- CRS(projection(raster.list[[1]]))


# ## Station data
# 
# # List files
# files.expl <- list.files("src/csv/", pattern = ".csv$", full.names = TRUE)
# 
# # Import files as SpatialPointsDataframe objects
# table.expl <- lapply(files.expl, function(i) {
#   temp.table <- read.csv2(i, dec = ".", stringsAsFactors = FALSE)
#   coordinates(temp.table) <- c("Longitude", "Latitude")
#   projection(temp.table) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   
#   temp.table <- spTransform(temp.table, CRS = proj.list)
#   
#   return(temp.table)
# })
# 
# 
# ## Sugar
# 
# # Parallelization
# clstr <- makePSOCKcluster(n.cores <- detectCores())
# clusterExport(clstr, c("lib", "raster.list"))
# clusterEvalQ(clstr, lapply(lib, function(i) require(i, character.only = TRUE, quietly = TRUE)))
# 
# # Reproject and save rasters
# raster.list.rpj <- parLapply(clstr, raster.list, function(i) {
#   projectRaster(i, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), 
#                 filename = paste("src/satellite/Landsat8_2013-07-07_hai/Level1_GeoTIFF_Data_Product/", 
#                                  substr(basename(raster.list[[1]]@file@name), 1, nchar(basename(raster.list[[1]]@file@name)) - 4), 
#                                  "_longlat", sep = ""), overwrite = TRUE, format = "GTiff")
# })
# 
# # Deregister parallel backend
# stopCluster(clstr)