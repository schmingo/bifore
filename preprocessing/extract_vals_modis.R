################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM MODIS SATELLITE DATA USING CORNER COORDINATES      ##
##                                                                            ##
## Important Note: This script will only work under Linux!                    ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-08-25                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("rgdal", "parallel", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set filepaths and filenames
path.wd <- "/home/schmingo/Diplomarbeit/" # Linux
#path.wd <- Florian
path.250.hdf <- "/home/schmingo/Diplomarbeit/src/satellite/RAW_MODIS_2013-07-07/MOD02QKM.A2013188.1120.005.2013188200351.hdf"
path.500.hdf <- "/home/schmingo/Diplomarbeit/src/satellite/RAW_MODIS_2013-07-07/MOD02HKM.A2013188.1120.005.2013188200351.hdf"
path.1km.hdf <- "/home/schmingo/Diplomarbeit/src/satellite/RAW_MODIS_2013-07-07/MOD021KM.A2013188.1120.005.2013188200351.hdf"

## Set working directory
setwd(path.wd)


### Import Landsat data

## List files
files.list.sat <- list.files("src/satellite//MOD02_2013-07-07", 
                             pattern = ".tif$", full.names = TRUE)

## Import files as RasterLayer objects
raster.layers <- lapply(files.list.sat, raster)
projection.layers <- CRS(projection(raster.layers[[1]]))


### Create extends from *.csv

## List CENTER files
files.all.center <- list.files("src/csv/", pattern = "all_plot_center.csv$", full.names = TRUE)

## Import CENTER files as SpatialPointsDataframe objects
table.all.center <- read.csv2(files.all.center, dec = ".", stringsAsFactors = FALSE)
coordinates(table.all.center) <- c("Longitude", "Latitude")
projection(table.all.center) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
 
table.all.center <- spTransform(table.all.center, CRS = projection.layers)

## List CORNER files
files.all.corner <- list.files("src/csv/", pattern = "all_plot_corner.csv$", full.names = TRUE)

## Import CORNER files as SpatialPointsDataframe objects
table.all <- read.csv2(files.all.corner, dec = ".", stringsAsFactors = FALSE)
coordinates(table.all) <- c("Longitude", "Latitude")
projection(table.all) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
table.all <- spTransform(table.all, CRS = projection.layers)

## Retrieve extent from CORNER coordinates
extent.all <- lapply(seq(1, nrow(table.all), 4), function(i) {
  extent(coordinates(table.all[i:(i+3), ]))
  })


### Extraction

## Parallelization
clstr <- makePSOCKcluster(n.cores <- 4)
clusterExport(clstr, c("lib", "raster.layers", "extent.all", "table.all.center", "table.all"))
clusterEvalQ(clstr, lapply(lib, function(i) require(i, character.only = TRUE, quietly = TRUE)))

## Extract and AVERAGE cell values
values.all <- parLapply(clstr, raster.layers, function(h) {
  temp.values <- sapply(extent.all, function(i) {
    temp.extract <- extract(h, i)
    
    if (length(temp.extract) > 1)
      temp.extract <- mean(temp.extract, na.rm = TRUE)
    
    return(temp.extract)
  })
  
  temp.df <- data.frame(table.all.center, ls_grey_value = temp.values)
  
  return(temp.df)
})

## Merge single data frames
values.all.new <- Reduce(function(...) merge(..., by = 1:6), values.all)
names(values.all.new)[7:44] <- sapply(strsplit(substr(basename(files.list.sat), 1, nchar(basename(files.list.sat)) - 4), "_"), "[[", 2)
coordinates(values.all.new) <- c("Longitude", "Latitude")

## Deregister parallel backend
stopCluster(clstr)


### Extract radiance_scale from original *.hdf

## Load extraction script
source("scripts/preprocessing/hdfExtractRadScale.R")
hdfExtractRadScale (path.wd,
                    path.250.hdf,
                    path.500.hdf,
                    path.1km.hdf
                    )


## Write data to new csv
write.table(values.all.new, file = "src/csv/all_greyvalues_modis.csv", dec = ".", quote = FALSE, 
            col.names = TRUE, row.names = FALSE, sep =";")