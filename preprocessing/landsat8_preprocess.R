################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT GREYVALUES FROM LANDSAT 8 SATELLITE DATA USING CORNER COORDINATES  ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-11                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("rgdal", "parallel", "raster", "matrixStats")
lapply(lib, function(...) require(..., character.only = TRUE))

################################################################################
## Set filepaths and filenames

## Filepath to wd
path.wd <- "/home/schmingo/Dropbox/Diplomarbeit/code/bifore/"

## Filepath to Landsat8 images
path.img <- "src/satellite/Landsat8/hai/"
#path.out <- "src/satellite/Landsat8/hai/out/" (only necessary for reprojection)

## Filepath to csv files
path.csv <- "src/csv/hai/"

path.modules <- "/home/schmingo/Diplomarbeit/bifore/"
filename.mod.ExtractScales <- "preprocessing/landsat8_mod_ExtractScales.R"

## Filepath and filename of output csv
filename.csv.out <- "hai_greyvalues_landsat8.csv"
filename.csv.out.abundance <- "hai_greyvalues_landsat8_abundance.csv"
filename.csv.out.deriv <- "hai_greyvalues_landsat8_deriv.csv"
filename.csv.out.deriv.ab <- "hai_greyvalues_landsat8_deriv_abundance.csv"



################################################################################
### Set working directory ######################################################

setwd(path.wd)

################################################################################
### Reproject Landsat 8 Data ###################################################
                                                                                
#  source("scripts/preprocessing/landsat8_mod_reprojection.R") #BUG!
#  reproject_landsat8 (lib, path.img, path.out)

################################################################################
### Import Landsat data ########################################################

## List files
files.list.sat <- list.files(path.img, 
                     pattern = ".TIF$", full.names = TRUE)

## Import files as RasterLayer objects
raster.layers <- lapply(files.list.sat, raster)
projection.layers <- CRS(projection(raster.layers[[1]]))


################################################################################
### Create extends from CSV-files ##############################################

## List CENTER files
files.center <- list.files(path.csv,
                           pattern = "plot_center.csv$",
                           full.names = TRUE)

## Import CENTER files as SpatialPointsDataframe objects
table.center <- read.csv2(files.center,
                          dec = ".",
                          stringsAsFactors = FALSE)

coordinates(table.center) <- c("Longitude", "Latitude")
projection(table.center) <- "+init=epsg:4326"
 
table.center <- spTransform(table.center, CRS = projection.layers)

## List CORNER files
files.corner <- list.files(path.csv,
                           pattern = "_corner.csv$",
                           full.names = TRUE)

## Import CORNER files as SpatialPointsDataframe objects
table.corner <- read.csv2(files.corner,
                          dec = ".",
                          stringsAsFactors = FALSE)

coordinates(table.corner) <- c("Longitude", "Latitude")
projection(table.corner) <- "+init=epsg:4326"
  
table.corner <- spTransform(table.corner, CRS = projection.layers)

## Retrieve extent from CORNER coordinates
extent <- lapply(seq(1, nrow(table.corner), 4), function(i) {
  extent(coordinates(table.corner[i:(i+3), ]))
  })


################################################################################
### Extraction of cell values ##################################################

## Parallelization
clstr <- makePSOCKcluster(n.cores <- detectCores()-1
                          )
clusterExport(clstr, c("lib", 
                       "raster.layers", 
                       "extent", 
                       "table.center", 
                       "table.corner"))

clusterEvalQ(clstr, lapply(lib, function(i) require(i, 
                                                    character.only = TRUE, 
                                                    quietly = TRUE)))

## Extract and AVERAGE cell values
values <- parLapply(clstr, raster.layers, function(h) {
  temp.values <- sapply(extent, function(i) {
    temp.extract <- extract(h, i)
    
    if (length(temp.extract) > 1)
      temp.extract <- mean(temp.extract, na.rm = TRUE)
    
    return(temp.extract)
  })
  
  temp.df <- data.frame(table.center, ls_grey_value = temp.values)
  
  return(temp.df)
})

## Merge single data frames
values.all <- Reduce(function(...) merge(..., by = 1:6), values)

names(values.all)[7:18] <- sapply(strsplit(substr(basename(files.list.sat), 
                                                      1, 
                                                      nchar(basename(files.list.sat)) - 4), 
                                               "_"), "[[", 2)

# coordinates(values.all) <- c("Longitude", "Latitude")

## Deregister parallel backend
stopCluster(clstr)

## Reformat Colnames
tmp.names <- names(values.all)[7:(ncol(values.all)-1)]
tmp.bands <- as.numeric(sapply(strsplit(tmp.names, "B"), "[[", 2))
tmp.bands <- formatC(tmp.bands, width = 2, format = "d", flag = "0")

names(values.all)[7:(ncol(values.all)-1)] <- paste("B", 
                                                           tmp.bands, 
                                                           sep = "")

## Reorder Colnames
values.all <- data.frame(values.all)
values.all <- values.all[, c(1:6,9:17,7,8,18)]


################################################################################
### Extract scalefactors and offsets from Landsat8 metadata ####################
### Remove BQA band ############################################################

print("Extracting scalefactors and offsets from metadata...")
greyvalues <- greyvalues[, 1:ncol(greyvalues)-1]

source(paste0(path.modules,filename.mod.ExtractScales))

scales <- ExtractLS8Scale(path.img)


################################################################################
### Calculate first derivate of greyvalue ######################################

sub.values.all <- values.all[7:ncol(values.all)]
diffs <- rowDiffs(as.matrix(sub.values.all)) # calculate first derivate (diff)

# paste dataframes. 
# add "0-column" because there is no slope for the first greyvalue
deriv.values.all <- cbind(values.all[1:6],0,diffs)
names(deriv.values.all) <- names(values.all) # write colnames to new df



################################################################################
### Write data to new csv ######################################################

write.table(values.all, file = paste(path.csv, filename.csv.out, sep=""), 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")

write.table(deriv.values.all, file = paste(path.csv, filename.csv.out.deriv, sep=""), 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")
