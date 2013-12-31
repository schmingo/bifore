################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CHECK COORDINATES FOR CLOUDS USING MODIS CLOUDMASK AND MODISCLOUD-PACKAGE  ##
##                                                                            ##
## Ref.: - http://modis-atmos.gsfc.nasa.gov/_docs/CMUSERSGUIDE.pdf            ##
##       - MOD35 .hdf metadata                                                ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-12-19                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("modiscloud", "devtools", "doParallel", "rgdal", "foreach")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
# setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

## To preprocess MOD35_L2 and MOD03 files; both must be in the same directory.

path.tif.out <- ("/home/schmingo/SAVE/Diplomarbeit/modiscloud_out/2012/")
path.hdf.in <- ("/home/schmingo/SAVE/Diplomarbeit/modiscloud_mod35_mod03/2012/")

path.tif.cloudmask <- ("D:/Dropbox/Diplomarbeit/code/bifore/src/satellite/modiscloud_out/2004-2005/")
path.tif.cloudmask <- ("/home/schmingo/Dropbox/bifore/src/satellite/modiscloud_out/2004-2005/")

path.b0.cloudmask <- ("D:/Dropbox/Diplomarbeit/code/bifore/src/satellite/modiscloud_b0/")
path.b0.cloudmask <- ("/home/schmingo/Dropbox/bifore/src/satellite/modiscloud_b0/")

mrtpath <- ("/home/schmingo/apps/MRTSwath/bin/swath2grid")
# mrtpath <- ("C:/MRTSwath/bin/swath2grid")

################################################################################
### Import dataset #############################################################

data <- read.csv2("csv/kili/abundance_data_subset.csv",
                  dec = ".",
                  header = TRUE, 
                  stringsAsFactors = TRUE)

## Read date column as a date
data$date <- as.Date(data$date, format="%Y-%m-%d")


################################################################################
### Preprocessing MOD35_L2 and MOD03 | Run MRTSwath tool "swath2grid" ##########

# list.files(pattern = "MOD")
list.files(path = path.hdf.in, pattern = "MOD")

# Get the matching data/geolocation file pairs
fns_df <- check_for_matching_geolocation_files(moddir = path.hdf.in,
                                               modtxt = "MOD35_L2",
                                               geoloctxt = "MOD03",
                                               return_geoloc = FALSE,
                                               return_product = FALSE)
fns_df


# Box to subset
ul_lat <- -2.77
ul_lon <- 36.93
lr_lat <- -3.45
lr_lon <- 37.76


### For loop .hdf to .tif ######################################################

for(i in 1:nrow(fns_df)) {
  # Write parameter file for each .hdf
  prmfn <- write_MRTSwath_param_file(prmfn="/home/schmingo/Diplomarbeit/tmpMRTparams.prm",
                                     tifsdir=path.tif.out,
                                     modfn=fns_df$mod35_L2_fns[i],
                                     geoloc_fn=fns_df$mod03_fns[i],
                                     ul_lon=ul_lon,
                                     ul_lat=ul_lat,
                                     lr_lon=lr_lon,
                                     lr_lat=lr_lat)
  
  print(scan(file=prmfn, what="character", sep="\n"))
  
  # hdf to raster using parameter file and subset box
  run_swath2grid(mrtpath="swath2grid",
                 prmfn="/home/schmingo/Diplomarbeit/tmpMRTparams.prm",
                 tifsdir=path.tif.out,
                 modfn=fns_df$mod35_L2_fns[i],
                 geoloc_fn=fns_df$mod03_fns[i],
                 ul_lon=ul_lon,
                 ul_lat=ul_lat,
                 lr_lon=lr_lon,
                 lr_lat=lr_lat)
}


### foreach .hdf to .tif #######################################################
# registerDoParallel(cl <- makeCluster(detectCores() - 1))
# 
# foreach(i = 1:nrow(fns_df), .packages = lib) %dopar% {
#   # Write parameter file for each .hdf
#   prmfn <- write_MRTSwath_param_file(prmfn="tmpMRTparams.prm",
#                                      tifsdir=path.tif.out,
#                                      modfn=fns_df$mod35_L2_fns[i],
#                                      geoloc_fn=fns_df$mod03_fns[i],
#                                      ul_lon=ul_lon,
#                                      ul_lat=ul_lat,
#                                      lr_lon=lr_lon,
#                                      lr_lat=lr_lat)
#   
#   print(scan(file=prmfn, what="character", sep="\n"))
#   
#   # hdf to raster using parameter file and subset box
#   run_swath2grid(mrtpath="swath2grid",
#                  prmfn="tmpMRTparams.prm",
#                  tifsdir=path.tif.out,
#                  modfn=fns_df$mod35_L2_fns[i],
#                  geoloc_fn=fns_df$mod03_fns[i],
#                  ul_lon=ul_lon,
#                  ul_lat=ul_lat,
#                  lr_lon=lr_lon,
#                  lr_lat=lr_lat)
# }
# 
# stopCluster(cl)

################################################################################
### Extract values from a particular pixel #####################################

## get .tif list from swath2grid output
tiffns <- list.files(path.tif.cloudmask, pattern=".tif", full.names=TRUE)
tiffns

## Define image for pixel extraction and cloud identification 
fn <- tiffns[7]

## Define image as SpatialGridDataFrame
grd <- readGDAL(fn)

## Get CRS
grdproj <- CRS(proj4string(grd))
# grdproj
# grdbbox <- attr(grd, "bbox")
# grdbbox

# Get coordinates from a single observation
data.lat <- data$lat[35]
data.lon <- data$lon[35]

## Define image as raster
grdr <- raster(grd)

## Input the points x (longitude), then y (latitude)
point_to_sample <- c(data.lon, data.lat)
xycoords <- adf(matrix(data = point_to_sample, nrow = 1, ncol = 2))
names(xycoords) <- c("x", "y")

xy <- SpatialPoints(coords=xycoords, proj4string=grdproj)

## Extract single pixel from raster image
pixelval <- extract(grdr, xy, buffer = NULL)

# Have to convert to 8-bit binary string, and reverse to get the count correct
# (also reverse the 2-bit strings in the MODIS Cloud Mask table)
pixelval <- rev(t(digitsBase(pixelval, base= 2, 8)))

print(pixelval)

## Extract cloud indicator
cloud_indicator <- pixelval[2:3]
print(cloud_indicator)


################################################################################
### Download MODIS MOD02 files #################################################

# # install MODIS R-package
# install.packages("MODIS", repos="http://R-Forge.R-project.org")
# 
# # Load library
# library(MODIS)
# 
# ?getHdf
# getProduct() 

