################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM MODIS SATELLITE DATA USING CORNER COORDINATES      ##
##                                                                            ##
## Important Note: Parts of this script will only work under Linux!           ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-09-16                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("rgdal", "parallel", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))


## Set filepaths and filenames
path.wd <- "/home/schmingo/Google Drive/bifore/"

path.modis <- "src/satellite/MOD02_2013-07-07_1120"
path.raw.modis <- "src/satellite/RAW_MODIS_2013-07-07_1120/"
path.250.hdf <- "MOD02QKM.A2013188.1120.005.2013188200351.hdf"
path.500.hdf <- "MOD02HKM.A2013188.1120.005.2013188200351.hdf"
path.1km.hdf <- "MOD021KM.A2013188.1120.005.2013188200351.hdf"

csv.out.NA <- "src/csv/all_MODIS_20130707-1120_greyvalues_NA.csv"
csv.out <- "src/csv/all_MODIS_20130707-1120_greyvalues.csv"
csv.out.abundance <- "src/csv/all_MODIS_20130707-1120_greyvalues_abundance.csv"
csv.out.abundance.NA <- "src/csv/all_MODIS_20130707-1120_greyvalues_NA_abundance.csv"


################################################################################
### Set working directory ######################################################

setwd(path.wd)


################################################################################
### Rename MODIS files #########################################################
# Rename MODIS *.tif to bandname-corresponding filenames (e.g.: B01, B13.1)

# source("scripts/preprocessing/modis_mod_renameTIF.R")
# rename_modis_files (path.modis)


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
### Extraction of cell values from extends #####################################

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

################################################################################
### Check for NA values ########################################################

# source("scripts/preprocessing/modis_mod_checkNA.R")
# greyvalues <- checkNA (...)
greyvalues.na <- greyvalues
greyvalues.na[, 7:ncol(greyvalues.na)][greyvalues.na[, 7:ncol(greyvalues.na)] > 32767] <- NA

################################################################################
### Extraction of radiance_scale and reflectance_scale from *.hdf ##############

## Extract radiance_scale and reflectance_scale from original *.hdf

source("scripts/preprocessing/modis_mod_hdfExtractScales.R")
modscales <- hdfExtractMODScale (path.raw.modis,
                                 path.250.hdf,
                                 path.500.hdf,
                                 path.1km.hdf)

print(modscales)

greyvalues <- data.frame(greyvalues,stringsAsFactors = F)
greyvalues.na <- data.frame(greyvalues.na,stringsAsFactors = F)
modscales <- data.frame(modscales, stringsAsFactors = F)

## Subset data frames
greyvalues.sub.front <- greyvalues[1:6]
modscales.sub.scales <- as.numeric(modscales[["scales"]])

## Calculate new greyvalues (greyvalue * scalefactor)
greyvalues.na.sub.calc <- data.frame(t(t(greyvalues.na[7:44]) * modscales.sub.scales))
greyvalues.sub.calc <- data.frame(t(t(greyvalues[7:44]) * modscales.sub.scales))

## Recombine data frames
greyvalues.na.calc <- cbind(greyvalues.sub.front, greyvalues.na.sub.calc)
greyvalues.calc <- cbind(greyvalues.sub.front, greyvalues.sub.calc)

## Write values to new CSV-file
write.table(greyvalues.na.calc, file = csv.out.NA, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, sep =";")

write.table(greyvalues.calc, file = csv.out, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, sep =";")

################################################################################
### add pseudo - abundance data ################################################

data.abundance <- cbind(greyvalues.calc, abundance=sample(1:20, nrow(greyvalues.calc), replace = TRUE))
data.na.abundance <- cbind(greyvalues.na.calc, abundance=sample(1:20, nrow(greyvalues.na.calc), replace = TRUE))

write.table(data.abundance, file = csv.out.abundance, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")

write.table(data.na.abundance, file = csv.out.abundance.NA, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")