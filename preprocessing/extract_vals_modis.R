################################################################################
## BiFoRe Scripts
##
## EXTRACT GREYVALUES FROM MODIS SATELLITE DATA USING CORNER COORDINATES
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-08-01
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


### Import Landsat data

## List files
files.list.sat <- list.files("src/satellite/MODIS_2013-07-07_hai_sch_alb/", 
                             pattern = ".tif$", full.names = TRUE)

## Import files as RasterLayer objects
raster.layers <- lapply(files.list.sat, raster)
projection.layers <- CRS(projection(raster.layers[[1]]))


### Create extends from *.csv

## List CENTER files
files.all.center <- list.files("src/csv/", pattern = "all_plot_center_utm.csv$", full.names = TRUE)

## Import CENTER files as SpatialPointsDataframe objects
table.all.center <- read.csv2(files.all.center, dec = ".", stringsAsFactors = FALSE)
coordinates(table.all.center) <- c("utm_x", "utm_y")
projection(table.all.center) <- "+proj=utm +zone=32 ellps=WGS84 +units=m"
 
table.all.center <- spTransform(table.all.center, CRS = projection.layers)

## List CORNER files
files.all.corner <- list.files("src/csv/", pattern = "all_corner_utm.csv$", full.names = TRUE)

## Import CORNER files as SpatialPointsDataframe objects
table.all <- read.csv2(files.all.corner, dec = ".", stringsAsFactors = FALSE)
coordinates(table.all) <- c("utm_x", "utm_y")
projection(table.all) <- "+proj=utm +zone=32 ellps=WGS84 +units=m"
  
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
values.all.all <- Reduce(function(...) merge(..., by = 1:6), values.all)
names(values.all.all)[7:18] <- sapply(strsplit(substr(basename(files.list.sat), 1, nchar(basename(files.list.sat)) - 4), "_"), "[[", 2)
coordinates(values.all.all) <- c("utm_x", "utm_y")

## Deregister parallel backend
stopCluster(clstr)

## Reformat Colnames
tmp.names <- names(values.all.all)[5:(ncol(values.all.all)-1)]
tmp.bands <- as.numeric(sapply(strsplit(tmp.names, "B"), "[[", 2))
tmp.bands <- formatC(tmp.bands, width = 2, format = "d", flag = "0")

names(values.all.all)[5:(ncol(values.all.all)-1)] <- paste("B", tmp.bands, sep = "")

## Reorder Colnames
values.all.all <- data.frame(values.all.all)
values.all.all <- values.all.all[, c(1:7,10:17,8,9,18)] 

## Write data to new csv
write.table(values.all.all, file = "src/csv/all_greyvalues_modis.csv", dec = ".", quote = FALSE, 
            col.names = TRUE, row.names = FALSE, sep =";")