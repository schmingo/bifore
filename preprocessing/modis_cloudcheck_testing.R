################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CHECK COORDINATES FOR CLOUDS USING MODIS CLOUDMASK AND MODISCLOUD-PACKAGE  ##
##                                                                            ##
## Ref.: - http://modis-atmos.gsfc.nasa.gov/_docs/CMUSERSGUIDE.pdf            ##
##       - MOD35 .hdf metadata                                                ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-06                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("modiscloud", "devtools", "doParallel", "rgdal", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
# setwd("/media/permanent/programming/r/bifore")

################################################################################
### Set filepaths ##############################################################

path.csv <- ("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/csv/kili/abundance_data_subset.csv") # update this path
# path.csv <- ("abundance_data_subset.csv")

path.tif.cloudmask <- ("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/satellite/sample_modiscloud_out/2002-2003/") # update this path
# path.tif.cloudmask <- ("sample_modiscloud_out/2002-2003")


################################################################################
### Import dataset #############################################################

data <- read.csv2(path.csv,
                  dec = ".",
                  header = TRUE, 
                  stringsAsFactors = TRUE)

## Read date column as a date
#data$date <- as.Date(data$date, format="%Y-%m-%d")
#data$date <- as.POSIXct(data$date, format="%Y-%m-%d")
data$date <- strftime(as.POSIXct(data$date, format="%Y-%m-%d"), format = "%Y%j")

str(data[1:15])


################################################################################
### Extract values from a particular pixel #####################################

## get .tif list from swath2grid output
tiffns <- list.files(path.tif.cloudmask, pattern=".tif", full.names=TRUE)
tiffns

###

# Parallelization
registerDoParallel(cl <- makeCluster(4)
# Convert data to spatial object
coordinates(data) <- ~ lon + lat
proj4string(data) <- CRS("+init=epsg:4326")

# Loop through all available dates
foreach(g = 1:nrow(data), .packages = lib) %dopar% {
  
  print(g)
  
  # Initialize while-loop
  current.date <- data$date[g]
  cloudy <- TRUE
  
  while (cloudy) {
    
    # List available *_b0.tif files of current date
    fls.avl <- tiffns[grep(current.date, tiffns)]
    fls.avl.b0 <- fls.avl[grep("b0", fls.avl)]
    
    # Import raster images
    rst.avl.b0 <- lapply(fls.avl.b0, raster)
    
    for (h in rst.avl.b0) {
      # Extract corresponding cell values from raster images
      val.avl.b0 <- extract(h, data[g, ])
      # Convert extracted values to binary format  
      bit.avl.b0 <- rev(t(digitsBase(val.avl.b0, base = 2, 8)))
      
      ## Cloud Indicator from MOD35 metadata
      # Unobstructed FOV Quality Flag
      # 00 = Cloudy                   
      # 01 = Uncertain                
      # 10 = Probably  Clear          
      # 11 = Confident  Clear 
      
      # Break out of for-loop in case of cloud absence
      if (!all(bit.avl.b0[2:3] == c(0, 0))) {
        cloudy <- FALSE
        break
      }  
    }
    
    # Increment date by 1 day in case of no cloud absence
    if (cloudy) {
      current.date <- as.Date(current.date, format = "%Y%j") + 1
      current.date <- strftime(current.date, format = "%Y%j")
    }
  }
  
  # Retrieve information about first cloud-free day
  return(paste0(dirname(fls.avl)[1], "/", names(h), ".tif"))
}
                   
# Deregister parallel backend                   
stopCluster(cl)
                   
###


# ## Define image for pixel extraction and cloud identification 
# fn <- tiffns[1]
# 
# ## Define image as SpatialGridDataFrame
# grd <- readGDAL(fn)
# 
# ## Get CRS
# grdproj <- CRS(proj4string(grd))
# # grdproj
# # grdbbox <- attr(grd, "bbox")
# # grdbbox
# 
# # Get coordinates from a single observation
# data.lat <- data$lat[27]
# data.lon <- data$lon[27]
# 
# ## Define image as raster
# grdr <- raster(grd)
# 
# ## Input the points x (longitude), then y (latitude)
# point_to_sample <- c(data.lon, data.lat)
# xycoords <- adf(matrix(data = point_to_sample, nrow = 1, ncol = 2))
# names(xycoords) <- c("x", "y")
# 
# xy <- SpatialPoints(coords=xycoords, proj4string=grdproj)
# 
# ## Extract single pixel from raster image
# pixelval <- extract(grdr, xy, buffer = NULL)
# 
# # Have to convert to 8-bit binary string, and reverse to get the count correct
# # (also reverse the 2-bit strings in the MODIS Cloud Mask table)
# pixelval <- rev(t(digitsBase(pixelval, base= 2, 8)))
# 
# print(pixelval)
# 
# ## Extract cloud indicator
# cloud_indicator <- pixelval[2:3]
# print(cloud_indicator)
# 
# ## Cloud Indicator from MOD35 metadata
# # Unobstructed FOV Quality Flag
# # 00 = Cloudy                   
# # 01 = Uncertain                
# # 10 = Probably  Clear          
# # 11 = Confident  Clear 
################################################################################
### Get Date from MODIS files ##############################################

tiffns <- list.files(path.tif.cloudmask, pattern=".tif", full.names=TRUE)
tiffns

mod.dates.df <- dates_from_fileslist(tiffns)
mod.dates.df

################################################################################
### Download MODIS MOD02 files #################################################

# # install MODIS R-package
# install.packages("MODIS", repos="http://R-Forge.R-project.org")
# 
# # Load library
# library(MODIS)
# 
?getHdf
# getProduct() 

