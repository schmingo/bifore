################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CONVERT MYD02 .HDF FILES TO .GEOTIFF                                       ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-22                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("modiscloud", "devtools")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Diplomarbeit/subsample_myd02_hdf/")

path.hdf.in <- "/home/schmingo/Diplomarbeit/subsample_myd02_hdf/"
tifsdir <- "/home/schmingo/Diplomarbeit/subsample_myd02_geotiff/"
mrtpath = ("/home/schmingo/apps/MRTSwath/bin/swath2grid")


## Load required modules
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modis_mod_writeMRTSwathParamFile.R")
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modis_mod_runSwath2Grid.R")


################################################################################
### Preprocessing MYD35_L2 and MYD03 | Run MRTSwath tool "swath2grid" ##########

### List hdf files
fls.myd <- list.files(path.hdf.in,
                      pattern="MYD",
                      full.names=TRUE)

fls.myd


check_for_matching_geolocation_files(moddir = path.hdf.in,
                                     modtxt = "MYD02",
                                     geoloctxt = "MYD03",
                                     return_geoloc = FALSE, 
                                     return_product = FALSE)

# Get the matching data/geolocation file pairs
fls.matching = check_for_matching_geolocation_files(moddir = path.hdf.in,
                                                    modtxt = "MYD02",
                                                    geoloctxt = "MYD03", 
                                                    return_geoloc = FALSE, 
                                                    return_product = FALSE)
fls.matching


# Box to subset
ul_lat <- -2.77
ul_lon <- 36.93
lr_lat <- -3.45
lr_lon <- 37.76


################################################################################
### For-loop .hdf to .tif ######################################################

for (i in 1:nrow(fls.matching)) {
  
  prmfn = writeMRTSwathParamFile(prmfn = "tmpMRTparams.prm", 
                                 tifsdir = tifsdir, 
                                 modfn = fls.matching$mod35_L2_fns[i], 
                                 geoloc_fn = fls.matching$mod03_fns[i], 
                                 # sds = sds, 
                                 ul_lon = ul_lon, 
                                 ul_lat = ul_lat, 
                                 lr_lon = lr_lon, 
                                 lr_lat = lr_lat)
  
  
  runSwath2Grid(mrtpath = mrtpath, 
                prmfn = "tmpMRTparams.prm", 
                tifsdir = tifsdir, 
                modfn = fls.matching$mod35_L2_fns[i], 
                geoloc_fn = fls.matching$mod03_fns[i], 
                ul_lon = ul_lon, 
                ul_lat = ul_lat, 
                lr_lon = lr_lon, 
                lr_lat = lr_lat)
}
