################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM MODIS SATELLITE DATA USING CORNER COORDINATES      ##
##                                                                            ##
## Important Note: Parts of this script will only work under Linux!           ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-07                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("rgdal", "parallel", "raster", "matrixStats")
lapply(lib, function(...) require(..., character.only = TRUE))


################################################################################
## Set filepaths and filenames #################################################

path.wd <- "/home/schmingo/Dropbox/Diplomarbeit/code/bifore/"

## Module scripts

path.modules <- "/home/schmingo/Diplomarbeit/bifore/preprocessing/"
path.hdfExtractScales <- "modis_mod_hdfExtractScales.R"
path.renameTIF <- "modis_mod_renameTIF.R"

## Satellite imagery
path.modis <- "src/satellite/MOD02_2013-07-07_1120/"
path.modis.raw <- "src/satellite/RAW_MODIS_2013-07-07_1120/"
path.250.hdf <- "MOD02QKM.A2013188.1120.005.2013188200351.hdf"
path.500.hdf <- "MOD02HKM.A2013188.1120.005.2013188200351.hdf"
path.1km.hdf <- "MOD021KM.A2013188.1120.005.2013188200351.hdf"

## Filepath and filename of output csv
path.csv <- "/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/csv/"
csv.out.raw <- "MODIS_20130707-1120_greyvalues_RAW.csv"
csv.out <- "MODIS_20130707-1120_greyvalues.csv"
csv.out.NA <- "MODIS_20130707-1120_greyvalues_NA.csv"
csv.out.NA.deriv <- "MODIS_20130707-1120_greyvalues_NA_derivate.csv"


################################################################################
### Set working directory ######################################################

setwd(path.wd)


################################################################################
### Rename MODIS files #########################################################

# Rename MODIS *.tif to bandname-corresponding filenames (e.g.: B01, B13.1)

# source(paste0(path.modules, path.renameTIF))
# rename_modis_files (path.modis)


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

## List CENTER files
files.center <- list.files("src/csv/",
                           pattern = "all_plot_center.csv$",
                           full.names = TRUE)

## Import CENTER files as SpatialPointsDataframe objects
table.center <- read.csv2(files.center,
                          dec = ".",
                          stringsAsFactors = FALSE)

coordinates(table.center) <- c("Longitude", "Latitude")
projection(table.center) <- "+init=epsg:4326"
 
table.center <- spTransform(table.center, CRS = projection.layers)

## List CORNER files
files.corner <- list.files("src/csv/",
                           pattern = "all_plot_corner.csv$",
                           full.names = TRUE)

## Import CORNER files as SpatialPointsDataframe objects
table.corner <- read.csv2(files.corner,
                          dec = ".",
                          stringsAsFactors = FALSE)

coordinates(table.corner) <- c("Longitude", "Latitude")
projection(table.corner) <- "+init=epsg:4326"
  
table.corner <- spTransform(table.corner, CRS = projection.layers)

## Retrieve extent from CORNER coordinates
extent.all <- lapply(seq(1, nrow(table.corner), 4), function(i) {
  extent(coordinates(table.corner[i:(i+3), ]))
  })


################################################################################
### Extraction of cell values from extends #####################################

## Parallelization
clstr <- makePSOCKcluster(n.cores <- detectCores()-1)

clusterExport(clstr, c("lib", 
                       "raster.layers", 
                       "extent.all", 
                       "table.center", 
                       "table.corner"))

clusterEvalQ(clstr, lapply(lib, function(i) require(i, 
                                                    character.only = TRUE, 
                                                    quietly = TRUE)))

## Extract and AVERAGE cell values
print("Extracting cell values...(this may take up to 15 minutes)")
values.all <- parLapply(clstr, raster.layers, function(h) {
  temp.values <- sapply(extent.all, function(i) {
    temp.extract <- extract(h, i)
    
    if (length(temp.extract) > 1)
      temp.extract <- mean(temp.extract, na.rm = TRUE)
    
    return(temp.extract)
  })
  
  temp.df <- data.frame(table.center, greyvalue = temp.values)
  
  return(temp.df)
})

## Merge single data frames
greyvalues <- Reduce(function(...) merge(..., by = 1:6), values.all)

names(greyvalues)[7:44] <- substr(basename(files.list.mod),
                                  1,
                                  nchar(basename(files.list.mod))-4)

# coordinates(values.all.new) <- c("Longitude", "Latitude")

## Deregister parallel backend
stopCluster(clstr)


################################################################################
### Check for NA values ########################################################

greyvalues.raw <- greyvalues
greyvalues.raw.na <- greyvalues.raw
greyvalues.raw.na[, 7:ncol(greyvalues.raw.na)][greyvalues.raw.na[, 7:ncol(greyvalues.raw.na)] > 32767] <- NA


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
greyvalues.raw.sub.front <- greyvalues.raw[1:6]
modscales.sub.scales <- as.numeric(modscales[["scales"]])

## Calculate new greyvalues (greyvalue * scalefactor)
greyvalues.na.sub.calc <- data.frame(t(t(greyvalues.raw.na[7:44]) * modscales.sub.scales))
greyvalues.sub.calc <- data.frame(t(t(greyvalues.raw[7:44]) * modscales.sub.scales))

## Recombine data frames
greyvalues.na.calc <- cbind(greyvalues.raw.sub.front, greyvalues.na.sub.calc)
greyvalues.calc <- cbind(greyvalues.raw.sub.front, greyvalues.sub.calc)


################################################################################
### Calculate first derivate of greyvalues #####################################

sub.greyvalues.na.calc <- greyvalues.na.calc[7:ncol(greyvalues.na.calc)]
diffs <- rowDiffs(as.matrix(sub.greyvalues.na.calc)) # calculate first derivate (diff)

# paste dataframes
# add "0-column" because there is no slope for the first greyvalue
deriv.greyvalues.na.calc <- cbind(greyvalues.na.calc[1:6],0,diffs)
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
