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
lib <- c("modiscloud", "devtools", "doParallel", "rgdal", "foreach", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

## To preprocess MOD35_L2 and MOD03 files; both must be in the same directory.

path.csv <- ("csv/kili/abundance_data_subset.csv")

path.hdf.in <- ("/home/schmingo/SAVE/Diplomarbeit/modiscloud_mod35_mod03/2002-2003/")

path.tif.cloudmask <- ("satellite/modiscloud_out/2002-2003/")

path.b0.cloudmask <- ("satellite/modiscloud_out/2002-2003/")

mrtpath <- ("/home/schmingo/apps/MRTSwath/bin/swath2grid")


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
### Preprocessing MOD35_L2 and MOD03 | Run MRTSwath tool "swath2grid" ##########

# List MOD-files
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


### For-loop .hdf to .tif ######################################################

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


################################################################################
### Check observations from csv file for cloudiness using tiffs ################

## get .tif list from swath2grid output
tiffns <- list.files(path.tif.cloudmask, pattern=".tif", full.names=TRUE)
tiffns

###

# Parallelization
registerDoParallel(cl <- makeCluster(4))
                   
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

