################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## REPROJECT LANDSAT8 Data to WGS84 LatLong (EPSG: 4326)                      ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-07-23                                                        ##
##                                                                            ##
################################################################################
## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("rgdal", "parallel", "doParallel", "raster", "foreach")
lapply(lib, function(...) require(..., character.only = TRUE))


## Set filepaths and filenames
path.wd <- "/home/schmingo/Diplomarbeit/" # Linux

path.img <- "src/satellite/Landsat8/hai/"
path.out <- "src/satellite/Landsat8/hai/out/"

patt.corner <- "hai_plot_center.csv$"
patt.center <- "hai_corner.csv$"


################################################################################
### Set working directory ######################################################

setwd(path.wd)
# reproject_landsat8 <- function(lib,
#                                path.img,
#                                path.out
#                                )
# {
   
   print ("Reproject Landsat8 files ... ")
   
  ## Required packages
  sapply(lib, function(...) stopifnot(require(..., character.only = T)))
  
  ## Parallelization
  #clstr <- makeCluster(detectCores())
  #registerDoParallel(clstr)  

  ##############################################################################
  ### Import Landsat8 Data #####################################################

  ## List files
  fls.ls <- list.files(path.img, 
                       pattern = ".TIF$", 
                       full.names = TRUE)


  ## Import files as RasterLayer objects
  rst.ls <- lapply(fls.ls, raster)

  ##############################################################################
  ### Reproject Landsat 8 Data #################################################

  ## Reproject and save rasters
#   rst.ls.rpj <- foreach(i = rst.ls, .packages = lib) %dopar% {
#     projectRaster(i, crs = CRS("+init=epsg:4326"), 
#                   filename = paste(path.wd, 
#                                    path.out,
#                                    substr(basename(rst.ls[[1]]@file@name), 
#                                          1, 
#                                          nchar(basename(rst.ls[[1]]@file@name)) - 4), 
#                                   "_longlat"
#                                    , sep = ""), 
#                   overwrite = TRUE, 
#                   format = "GTiff")
#   }

  ## OLD FASHION WAY
  clstr <- makePSOCKcluster(n.cores <- detectCores())
  clusterExport(clstr, c("lib", "rst.ls"))
  clusterEvalQ(clstr, lapply(lib, function(i) require(i, character.only = TRUE, quietly = TRUE)))
  
  rst.ls.rpj <- parLapply(clstr, rst.ls, function(i) {
    projectRaster(i, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), 
                  filename = paste("src/satellite/Landsat8/hai/out/", 
                                   substr(basename(rst.ls[[1]]@file@name), 1, nchar(basename(rst.ls[[1]]@file@name)) - 4), 
                                   "_longlat", sep = ""), overwrite = TRUE, format = "GTiff")
  })



  ## Deregister parallel backend
  stopCluster(clstr)
  print("Reprojection done ... ")
}