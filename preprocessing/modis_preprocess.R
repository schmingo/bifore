################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM MODIS SATELLITE DATA USING CORNER COORDINATES      ##
##                                                                            ##
## Important Note: Parts of this script will only work under Linux!           ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-30                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("rgdal", "doParallel", "raster", "matrixStats")
lapply(lib, function(...) require(..., character.only = TRUE))


################################################################################
## Set filepaths and filenames #################################################

path.wd <- "/home/schmingo/Dropbox/Diplomarbeit/code/bifore/"

## Module scripts

path.modules <- "/home/schmingo/Diplomarbeit/bifore/preprocessing/"
path.hdfExtractScales <- "modis_mod_hdfExtractScales.R"
path.renameTIF <- "modis_mod_renameTIF.R"

## Satellite imagery
path.modis <- "src/satellite/MOD02_kili_20060129/"
path.modis.raw <- "src/satellite/MOD02_kili_20060129_RAW/"
path.250.hdf <- "MOD02QKM.A2006029.0750.005.2010203164844.hdf"
path.500.hdf <- "MOD02HKM.A2006029.0750.005.2010203164844.hdf"
path.1km.hdf <- "MOD021KM.A2006029.0750.005.2010203164844.hdf"

## Filepath and filename of output csv
path.csv <- "/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/csv/kili/"
csv.out.raw <- "MODIS_20060129_greyvalues_RAW.csv"
csv.out <- "MODIS_20060129_greyvalues.csv"
csv.out.NA <- "MODIS_20060129_greyvalues_NA.csv"
csv.out.NA.deriv <- "MODIS_20060129_greyvalues_NA_derivate.csv"

## Filepath to plot coordinates
path.coords <- "src/csv/kili/"
filename.coords <- "kili_plot_center_coordinates.csv"



################################################################################
### Rename MODIS files #########################################################

# Rename MODIS *.tif to bandname-corresponding filenames (e.g.: B01, B13.1)

# source(paste0(path.modules, path.renameTIF))
# rename_modis_files(path.wd, path.modis)


################################################################################
### Set working directory ######################################################

setwd(path.wd)


################################################################################
### Import MODIS data ##########################################################

## List files
files.list.mod <- list.files(path.modis,
                             pattern = ".tif$",
                             full.names = TRUE)


## Import files as RasterLayer objects
raster.layers <- lapply(files.list.mod, raster)
projection.layers <- CRS(projection(raster.layers[[1]]))


################################################################################
### Create extends from CSV-files ##############################################

## Import CENTER files as SpatialPointsDataframe objects
table.center <- read.csv2(paste0(path.coords, filename.coords),
                          dec = ".",
                          stringsAsFactors = FALSE)

coordinates(table.center) <- c("Longitude", "Latitude")
projection(table.center) <- "+init=epsg:4326"

table.center <- spTransform(table.center, CRS = projection.layers)


################################################################################
### Extraction of cell values ##################################################

registerDoParallel(cl <- makeCluster(detectCores() - 1))

greyvalues.raw <- foreach(i = seq(raster.layers), .packages = lib,
                          .combine = "cbind") %dopar% {
                            raster.layers[[i]][cellFromXY(raster.layers[[i]], table.center)]
                          }

stopCluster(cl)


################################################################################
### Combine dataframes #########################################################

## convert matrix to dataframe
greyvalues.raw <- as.data.frame(greyvalues.raw)

## set colnames
names(greyvalues.raw) <- substr(basename(files.list.mod),
                                1,
                                nchar(basename(files.list.mod))-4)


## Combine multiple dataframes to a single dataframe
greyvalues.raw <- cbind(table.center, greyvalues.raw)


################################################################################
### Check for NA values ########################################################

greyvalues <- greyvalues.raw
greyvalues.raw.na <- greyvalues.raw
greyvalues.raw.na[, 6:ncol(greyvalues.raw.na)][greyvalues.raw.na[, 6:ncol(greyvalues.raw.na)] > 32767] <- NA


################################################################################
### Extraction of radiance_scale and reflectance_scale from *.hdf ##############

print("Extract radiance_scale and reflectance_scale from original *.hdf")

source(paste0(path.modules, path.hdfExtractScales))
modscales <- hdfExtractMODScale (path.modis.raw,
                                 path.250.hdf,
                                 path.500.hdf,
                                 path.1km.hdf)

# print(modscales)

greyvalues.raw <- data.frame(greyvalues.raw, stringsAsFactors = F)
greyvalues.raw.na <- data.frame(greyvalues.raw.na, stringsAsFactors = F)
modscales <- data.frame(modscales, stringsAsFactors = F)

## Subset data frames
greyvalues.raw.sub.front <- greyvalues.raw[1:5]
modscales.sub.scales <- as.numeric(modscales[["scales"]])

## Calculate new greyvalues (greyvalue * scalefactor)
greyvalues.na.sub.calc <- data.frame(t(t(greyvalues.raw.na[6:ncol(greyvalues.raw.na)]) * modscales.sub.scales))
greyvalues.sub.calc <- data.frame(t(t(greyvalues.raw[6:ncol(greyvalues.raw.na)]) * modscales.sub.scales))

## Recombine data frames
greyvalues.na.calc <- cbind(greyvalues.raw.sub.front, greyvalues.na.sub.calc)
greyvalues.calc <- cbind(greyvalues.raw.sub.front, greyvalues.sub.calc)


################################################################################
### Calculate first derivate of greyvalues #####################################

sub.greyvalues.na.calc <- greyvalues.na.calc[6:ncol(greyvalues.na.calc)]
diffs <- rowDiffs(as.matrix(sub.greyvalues.na.calc)) # calculate first derivate (diff)

# paste dataframes
# add "0-column" because there is no slope for the first greyvalue
deriv.greyvalues.na.calc <- cbind(greyvalues.na.calc[1:5],0,diffs)
names(deriv.greyvalues.na.calc) <- names(greyvalues.na.calc) # write colnames to new df


################################################################################
### Write data to new csv ######################################################

write.table(greyvalues, 
            file = paste0(path.csv, csv.out.raw),
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE,
            sep = ";")

write.table(greyvalues.na.calc, 
            file = paste0(path.csv, csv.out.NA), 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE,
            sep = ";")

write.table(greyvalues.calc, 
            file = paste0(path.csv, csv.out), 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE,
            sep = ";")

write.table(deriv.greyvalues.na.calc, 
            file = paste0(path.csv, csv.out.NA.deriv),
            dec = ".",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
