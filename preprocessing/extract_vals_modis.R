################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM MODIS SATELLITE DATA USING CORNER COORDINATES      ##
##                                                                            ##
## Important Note: Parts of this script will only work under Linux!           ##
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
#path.wd <- "D:/Diplomarbeit/" # Windows

path.modis <- "/home/schmingo/Diplomarbeit/src/satellite/MOD02_2013-07-07/"
path.raw.modis <- "/home/schmingo/Diplomarbeit/src/satellite/RAW_MODIS_2013-07-07/"
path.250.hdf <- "MOD02QKM.A2013188.1120.005.2013188200351.hdf"
path.500.hdf <- "MOD02HKM.A2013188.1120.005.2013188200351.hdf"
path.1km.hdf <- "MOD021KM.A2013188.1120.005.2013188200351.hdf"


################################################################################
### Rename MODIS files #########################################################

# source("scripts/preprocessing/rename_modis_files.R")
# rename_modis_files (path.modis)


################################################################################
### Set working directory ######################################################

setwd(path.wd)

################################################################################
### Import MODIS data ##########################################################

## List files
files.list.sat <- list.files(path.modis, 
                             pattern = ".tif$", full.names = TRUE)


## Import files as RasterLayer objects
raster.layers <- lapply(files.list.sat, raster)
projection.layers <- CRS(projection(raster.layers[[1]]))


################################################################################
### Create extends from CSV-files ##############################################

## List CENTER files
files.all.center <- list.files("src/csv/", 
                               pattern = "all_plot_center.csv$", 
                               full.names = TRUE)

## Import CENTER files as SpatialPointsDataframe objects
table.all.center <- read.csv2(files.all.center, dec = ".", 
                              stringsAsFactors = FALSE)

coordinates(table.all.center) <- c("Longitude", "Latitude")
projection(table.all.center) <- "+init=epsg:4326"
 
table.all.center <- spTransform(table.all.center, CRS = projection.layers)

## List CORNER files
files.all.corner <- list.files("src/csv/", 
                               pattern = "all_plot_corner.csv$", 
                               full.names = TRUE)

## Import CORNER files as SpatialPointsDataframe objects
table.all <- read.csv2(files.all.corner, 
                       dec = ".", 
                       stringsAsFactors = FALSE)

coordinates(table.all) <- c("Longitude", "Latitude")
projection(table.all) <- "+init=epsg:4326"
  
table.all <- spTransform(table.all, CRS = projection.layers)

## Retrieve extent from CORNER coordinates
extent.all <- lapply(seq(1, nrow(table.all), 4), function(i) {
  extent(coordinates(table.all[i:(i+3), ]))
  })


################################################################################
### Extraction of cell values ##################################################

## Parallelization
clstr <- makePSOCKcluster(n.cores <- detectCores()-1)

clusterExport(clstr, c("lib", 
                       "raster.layers", 
                       "extent.all", 
                       "table.all.center", 
                       "table.all"))

clusterEvalQ(clstr, lapply(lib, function(i) require(i, 
                                                    character.only = TRUE, 
                                                    quietly = TRUE)))

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
greyvalues <- Reduce(function(...) merge(..., by = 1:6), values.all)

names(greyvalues)[7:44] <- substr(basename(files.list.sat),
                                      1,
                                      nchar(basename(files.list.sat))-4)

# coordinates(values.all.new) <- c("Longitude", "Latitude")

## Deregister parallel backend
stopCluster(clstr)

## Extract radiance_scale from original *.hdf (only in Linux environment)
source("scripts/preprocessing/hdfExtractRadScale.R")
radscales <- hdfExtractRadScale (path.raw.modis,
                                 path.250.hdf,
                                 path.500.hdf,
                                 path.1km.hdf)

print(radscales)

greyvalues <- data.frame(greyvalues,stringsAsFactors = F)
radscales <- data.frame(radscales, stringsAsFactors = F)
# Spaltennamen von greyvalues in radscales übernehmen
#names(radscales) <- names(greyvalues[7:44])

## merge dataframes
#henninger <- merge(greyvalues, radscales[2,], all.x=T, all.y=T)

## change column order back to normal
#henninger <- subset(henninger,
#                     select=c("Plotname","Plotid", "Status", "Location", "Longitude", 
#                   "Latitude", "B01", "B02", "B03", "B04", "B05", "B06", "B07", 
#                   "B08", "B09", "B10", "B11", "B12", "B13.1", "B13.2", "B14.1", 
#                   "B14.2", "B15", "B16", "B17", "B18", "B19", "B20", "B21",
#                   "B22", "B23", "B24", "B25", "B26", "B27", "B28", "B29", "B30",
#                   "B31", "B32", "B33", "B34", "B35", "B36")) 
hasseroeder <- as.numeric(radscales[["scales"]])
class(hasseroeder)

veltins <- data.frame(t(t(greyvalues[7:44]) * hasseroeder))
veltins <- greyvalues %*% diag(hasseroeder)

#sweep(greyvalues[7:44], MARGIN=2, hasseroeder, '*')
# klaustaler <- rbind.fill(greyvalues, radscales)
# hefeweizen <- rbind(greyvalues[7:ncol(greyvalues)], radscales)
# 
# veltins <- merge(greyvalues,radscales[2,], by = c(#"Plotname",
#                                                    #"Plotid", 
#                                                    #"Status",
#                                                    #"Location", 
#                                                    #"Longitude",
#                                                    #"Latitude",
#                                                "B01", "B02", "B03", "B04", "B05", 
#                                                "B06", "B07", "B08", "B09", "B10", 
#                                                "B11", "B12", "B13.1", "B13.2", 
#                                                "B14.1", "B14.2", "B15", "B16", 
#                                                "B17", "B18", "B19", "B20", "B21",
#                                                "B22", "B23", "B24", "B25", "B26",
#                                                "B27", "B28", "B29", "B30", "B31",
#                                                "B32", "B33", "B34", "B35", "B36"), 
#                  all=T)




## Write values to new CSV-file
# write.table(values.all.new, file = "src/csv/all_greyvalues_modis.csv", 
#             dec = ".", 
#             quote = FALSE, 
#             col.names = TRUE, 
#             row.names = FALSE, sep =";")