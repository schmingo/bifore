################################################################################
## BiFoRe Scripts
##
## EXTRACT GREYVALUES FROM LANDSAT 8 SATELLITE DATA USING CORNER COORDINATES
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
files.list.sat <- list.files("src/satellite/Landsat8_2013-07-07_hai/Level1_GeoTIFF_Data_Product/", 
                     pattern = ".TIF$", full.names = TRUE)

## Import files as RasterLayer objects
raster.layers <- lapply(files.list.sat, raster)
projection.layers <- CRS(projection(raster.layers[[1]]))


### Create extends from *.csv

## List CENTER files
files.hai.center <- list.files("src/csv/", pattern = "hai_plot_center.csv$", full.names = TRUE)

## Import CENTER files as SpatialPointsDataframe objects
table.hai.center <- read.csv2(files.hai.center, dec = ".", stringsAsFactors = FALSE)
coordinates(table.hai.center) <- c("Longitude", "Latitude")
projection(table.hai.center) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
 
table.hai.center <- spTransform(table.hai.center, CRS = projection.layers)

## List CORNER files
files.hai.corner <- list.files("src/csv/", pattern = "hai_corner.csv$", full.names = TRUE)

## Import CORNER files as SpatialPointsDataframe objects
table.hai <- read.csv2(files.hai.corner, dec = ".", stringsAsFactors = FALSE)
coordinates(table.hai) <- c("Longitude", "Latitude")
projection(table.hai) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
table.hai <- spTransform(table.hai, CRS = projection.layers)

## Retrieve extent from CORNER coordinates
extent.hai <- lapply(seq(1, nrow(table.hai), 4), function(i) {
  extent(coordinates(table.hai[i:(i+3), ]))
  })


### Extraction

## Parallelization
clstr <- makePSOCKcluster(n.cores <- 4)
clusterExport(clstr, c("lib", "raster.layers", "extent.hai", "table.hai.center", "table.hai"))
clusterEvalQ(clstr, lapply(lib, function(i) require(i, character.only = TRUE, quietly = TRUE)))

## Extract and AVERAGE cell values
values.hai <- parLapply(clstr, raster.layers, function(h) {
  temp.values <- sapply(extent.hai, function(i) {
    temp.extract <- extract(h, i)
    
    if (length(temp.extract) > 1)
      temp.extract <- mean(temp.extract, na.rm = TRUE)
    
    return(temp.extract)
  })
  
  temp.df <- data.frame(table.hai.center, ls_grey_value = temp.values)
  
  return(temp.df)
})

## Merge single data frames
values.hai.all <- Reduce(function(...) merge(..., by = 1:6), values.hai)
names(values.hai.all)[7:18] <- sapply(strsplit(substr(basename(files.list.sat), 1, nchar(basename(files.list.sat)) - 4), "_"), "[[", 2)
coordinates(values.hai.all) <- c("Longitude", "Latitude")

## Deregister parallel backend
stopCluster(clstr)

## Reformat Colnames
tmp.names <- names(values.hai.all)[5:(ncol(values.hai.all)-1)]
tmp.bands <- as.numeric(sapply(strsplit(tmp.names, "B"), "[[", 2))
tmp.bands <- formatC(tmp.bands, width = 2, format = "d", flag = "0")

names(values.hai.all)[5:(ncol(values.hai.all)-1)] <- paste("B", tmp.bands, sep = "")

## Reorder Colnames
values.hai.all <- data.frame(values.hai.all)
values.hai.all <- values.hai.all[, c(1:7,10:17,8,9,18)] 

## Write data to new csv
write.table(values.hai.all, file = "src/csv/hai_greyvalues_landsat8.csv", dec = ".", quote = FALSE, 
            col.names = TRUE, row.names = FALSE, sep =";")