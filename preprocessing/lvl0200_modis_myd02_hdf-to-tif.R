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
setwd("/home/schmingo/Diplomarbeit/")

path.hdf.in <- "/home/schmingo/SAVE/Diplomarbeit/myd02_hdf/part1/"
tifsdir <- "/home/schmingo/SAVE/Diplomarbeit/myd02_tif/"
mrtpath = "/home/schmingo/apps/MRTSwath/bin/swath2grid"


## Load required modules
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0210_modis_mod_writeMRTSwathParamFile_1000.R")
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0210_modis_mod_writeMRTSwathParamFile_500.R")
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0210_modis_mod_writeMRTSwathParamFile_250.R")
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0220_modis_mod_runSwath2Grid.R")


################################################################################
### Preprocessing MYD35_L2 and MYD03 | Run MRTSwath tool "swath2grid" ##########

### List hdf files
fls.myd <- list.files(path.hdf.in,
                      pattern="MYD",
                      full.names=TRUE)

fls.myd

# Box to subset
ul_lat <- -2.77
ul_lon <- 36.93
lr_lat <- -3.45
lr_lon <- 37.76

## Check actual time
starttime <- Sys.time()


################################################################################
### For-loop .hdf to .tif 1000m per pixel ######################################

# Get the matching data/geolocation file pairs
fls.1km.matching = check_for_matching_geolocation_files(moddir = path.hdf.in,
                                                        modtxt = "MYD021KM",
                                                        geoloctxt = "MYD03", 
                                                        return_geoloc = FALSE, 
                                                        return_product = FALSE)
fls.1km.matching


for (i in 1:nrow(fls.1km.matching)) {
  
  prmfn = writeMRTSwathParamFile_1000(prmfn = "tmpMRTparams.prm", 
                                      tifsdir = tifsdir, 
                                      modfn = fls.1km.matching$mod35_L2_fns[i], 
                                      geoloc_fn = fls.1km.matching$mod03_fns[i], 
                                      # sds = sds, 
                                      ul_lon = ul_lon, 
                                      ul_lat = ul_lat, 
                                      lr_lon = lr_lon, 
                                      lr_lat = lr_lat)
  
  
  runSwath2Grid(mrtpath = mrtpath, 
                prmfn = "tmpMRTparams.prm", 
                tifsdir = tifsdir, 
                modfn = fls.1km.matching$mod35_L2_fns[i], 
                geoloc_fn = fls.1km.matching$mod03_fns[i], 
                ul_lon = ul_lon, 
                ul_lat = ul_lat, 
                lr_lon = lr_lon, 
                lr_lat = lr_lat)
}


################################################################################
### For-loop .hdf to .tif 500m per pixel #######################################

# Get the matching data/geolocation file pairs
fls.hkm.matching = check_for_matching_geolocation_files(moddir = path.hdf.in,
                                                        modtxt = "MYD02HKM",
                                                        geoloctxt = "MYD03", 
                                                        return_geoloc = FALSE, 
                                                        return_product = FALSE)
fls.hkm.matching


for (i in 1:nrow(fls.hkm.matching)) {
  
  prmfn = writeMRTSwathParamFile_500(prmfn = "tmpMRTparams.prm", 
                                     tifsdir = tifsdir, 
                                     modfn = fls.hkm.matching$mod35_L2_fns[i], 
                                     geoloc_fn = fls.hkm.matching$mod03_fns[i], 
                                     # sds = sds, 
                                     ul_lon = ul_lon, 
                                     ul_lat = ul_lat, 
                                     lr_lon = lr_lon, 
                                     lr_lat = lr_lat)
  
  
  runSwath2Grid(mrtpath = mrtpath, 
                prmfn = "tmpMRTparams.prm", 
                tifsdir = tifsdir, 
                modfn = fls.hkm.matching$mod35_L2_fns[i], 
                geoloc_fn = fls.hkm.matching$mod03_fns[i], 
                ul_lon = ul_lon, 
                ul_lat = ul_lat, 
                lr_lon = lr_lon, 
                lr_lat = lr_lat)
}


################################################################################
### For-loop .hdf to .tif 250m per pixel #######################################

# Get the matching data/geolocation file pairs
fls.qkm.matching = check_for_matching_geolocation_files(moddir = path.hdf.in,
                                                        modtxt = "MYD02QKM",
                                                        geoloctxt = "MYD03", 
                                                        return_geoloc = FALSE, 
                                                        return_product = FALSE)
fls.qkm.matching


for (i in 1:nrow(fls.qkm.matching)) {
  
  prmfn = writeMRTSwathParamFile_250(prmfn = "tmpMRTparams.prm", 
                                     tifsdir = tifsdir, 
                                     modfn = fls.qkm.matching$mod35_L2_fns[i], 
                                     geoloc_fn = fls.qkm.matching$mod03_fns[i], 
                                     # sds = sds, 
                                     ul_lon = ul_lon, 
                                     ul_lat = ul_lat, 
                                     lr_lon = lr_lon, 
                                     lr_lat = lr_lat)
  
  
  runSwath2Grid(mrtpath = mrtpath, 
                prmfn = "tmpMRTparams.prm", 
                tifsdir = tifsdir, 
                modfn = fls.qkm.matching$mod35_L2_fns[i], 
                geoloc_fn = fls.qkm.matching$mod03_fns[i], 
                ul_lon = ul_lon, 
                ul_lat = ul_lat, 
                lr_lon = lr_lon, 
                lr_lat = lr_lat)
}


################################################################################
### Remove unnecessary tifs ####################################################

do.call(file.remove,list(list.files(tifsdir, pattern="Aggr", full.names=TRUE)))
do.call(file.remove,list(list.files(tifsdir, pattern="Uncert", full.names=TRUE)))


################################################################################
### Check actual time again ####################################################

endtime <- Sys.time()

time <- endtime - starttime
time
