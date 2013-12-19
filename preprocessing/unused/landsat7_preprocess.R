################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM LANDSAT 7 SATELLITE DATA USING CORNER COORDINATES  ##
##                                                                            ##
##                                                                            ##
## IMPORTANT NOTE: LANDSAT 7 SENSOR IS BROKEN!                                ##
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

## Filepath to wd
path.wd <- "D:/Dropbox/Diplomarbeit/code/bifore/"

## Satellite imagery
path.img <- "src/satellite/LS7_kili_20060129T0727Z_east_great/images/"
#path.out <- "src/satellite/Landsat8/hai/out/" (only necessary for reprojection)

path.coords <- "src/csv/kili/"
filename.coords <- "kili_plot_center_coordinates.csv"

path.modules <- "D:/Diplomarbeit/bifore/preprocessing/"
filename.mod.ExtractScales <- "landsat7_mod_ExtractScales.R"

## Filepath and filename of output csv
path.csv <- "D:/Dropbox/Diplomarbeit/code/bifore/src/csv/kili/"
csv.out.raw <- "ls7_kili_20060129T0727Z_greyvalues_RAW.csv"
csv.out <- "ls7_kili_20060129T0727Z_greyvalues.csv"
csv.out.NA <- "ls7_kili_20060129T0727Z_greyvalues_NA.csv"
csv.out.NA.deriv <- "ls7_kili_20060129T0727Z_NA_derivate.csv"

################################################################################
### Set working directory ######################################################

setwd(path.wd)


################################################################################
### Import Landsat data ########################################################

## List files
files.list.sat <- list.files(path.img, 
                     pattern = ".TIF$", full.names = TRUE)

## Import files as RasterLayer objects
raster.layers <- lapply(files.list.sat, raster)
projection.layers <- CRS(projection(raster.layers[[1]]))


################################################################################
### Create extends from CSV-file ###############################################

## Import CENTER files as SpatialPointsDataframe objects
table.center <- read.csv2(paste0(path.coords, filename.coords),
                          dec = ".",
                          stringsAsFactors = FALSE)

coordinates(table.center) <- c("Longitude", "Latitude")
projection(table.center) <- "+init=epsg:4326"
 
table.center <- spTransform(table.center, CRS = projection.layers)

## Set zero values to NA to remove border of imagery (parallelized)
registerDoParallel(cl <- makeCluster(detectCores() - 1))

raster.layers <- foreach(i = raster.layers, .packages = lib) %dopar% {
  i[i[] == 0] <- NA
  return(i)
}
stopCluster(cl)


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


## create dataframe
greyvalues.raw <- as.data.frame(greyvalues.raw)

## set colnames
names(greyvalues.raw) <- sapply(strsplit(substr(basename(files.list.sat),
                                                1,
                                                nchar(basename(files.list.sat)) - 4),
                                         "_"), "[[", 2)


## Combine multiple dataframes to a single dataframe
greyvalues <- cbind(table.center, greyvalues.raw)


## Reformat colnames
tmp.names <- names(greyvalues)[6:(ncol(greyvalues))]
tmp.bands <- as.numeric(sapply(strsplit(tmp.names, "B"), "[[", 2))
tmp.bands <- formatC(tmp.bands, width = 2, format = "d", flag = "0")

names(greyvalues)[6:(ncol(greyvalues))] <- paste("B",
                                                 tmp.bands,
                                                 sep = "")


# ###############################################################################
# ## Remove NA Values ###########################################################
# 
# greyvalues.raw <- greyvalues
# greyvalues.raw.na <- greyvalues.raw
# greyvalues.raw.na[, 7:ncol(greyvalues.raw.na)][greyvalues.raw.na[, 7:ncol(greyvalues.raw.na)] > 65535] <- NA
# 
# 
################################################################################
### Extract scalefactors and offsets from Landsat8 metadata ####################
# 
# print("Extracting scalefactors and offsets from metadata...")
# 
# source(paste0(path.modules, filename.mod.ExtractScales))
# 
# ls8scales <- ExtractLS8Scale(path.img)
# 
# ################################################################################
# ## calculate new greyvalues using scalefactors and offsets #####################
# 
# ## Subset data frames
# greyvalues.raw.sub.front <- greyvalues.raw[1:6]
# ls8scales.sub.scales <- as.numeric(ls8scales[["multiply scales"]])
# ls8scales.sub.offset <- as.numeric(ls8scales[["add offset"]])
# 
# ## Calculate new greyvalues (greyvalue * scalefactor)
# greyvalues.na.sub.calc <- data.frame(t(t(greyvalues.raw.na[7:ncol(greyvalues.raw.na)]) * ls8scales.sub.scales))
# greyvalues.sub.calc <- data.frame(t(t(greyvalues.raw[7:ncol(greyvalues.raw)]) * ls8scales.sub.scales))
# 
# ## Calculate new greyvalues (newgreyvalue + offset)
# greyvalues.na.sub.calc <- data.frame(greyvalues.na.sub.calc + t(ls8scales.sub.offset))
# greyvalues.sub.calc <- data.frame(greyvalues.sub.calc + t(ls8scales.sub.offset))
# 
# ## Recombine data frames
# greyvalues.na.calc <- cbind(greyvalues.raw.sub.front, greyvalues.na.sub.calc)
# greyvalues.calc <- cbind(greyvalues.raw.sub.front, greyvalues.sub.calc)
# 
# 
# ################################################################################
# ### Calculate first derivate of greyvalue ######################################
# 
# sub.greyvalues.na.calc <- greyvalues.na.calc[7:ncol(greyvalues.na.calc)]
# diffs <- rowDiffs(as.matrix(sub.greyvalues.na.calc)) # calculate first derivate (diff)
# 
# ## paste dataframes
# ## add "0-column" because there is no slope for the first greyvalue
# deriv.greyvalues.na.calc <- cbind(greyvalues.na.calc[1:6],0,diffs)
# names(deriv.greyvalues.na.calc) <- names(greyvalues.na.calc) # write colnames to new df
# 
# 
# ################################################################################
# ### Write data to new csv ######################################################
# 
# write.table(greyvalues.raw, 
#             file = paste0(path.csv, csv.out.raw),
#             dec = ".", 
#             quote = FALSE, 
#             col.names = TRUE, 
#             row.names = FALSE,
#             sep = ";")
# 
# write.table(greyvalues.calc, 
#             file = paste0(path.csv, csv.out), 
#             dec = ".", 
#             quote = FALSE, 
#             col.names = TRUE, 
#             row.names = FALSE,
#             sep = ";")
# 
# write.table(greyvalues.na.calc, 
#             file = paste0(path.csv, csv.out.NA), 
#             dec = ".", 
#             quote = FALSE, 
#             col.names = TRUE, 
#             row.names = FALSE,
#             sep = ";")
# 
# write.table(deriv.greyvalues.na.calc, 
#             file = paste0(path.csv, csv.out.NA.deriv),
#             dec = ".",
#             quote = FALSE,
#             col.names = TRUE,
#             row.names = FALSE,
#             sep = ";")
