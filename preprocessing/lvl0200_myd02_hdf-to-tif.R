################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CONVERT MYD02 .HDF FILES TO .GEOTIFF                                       ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-08                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("modiscloud", "devtools", "foreach")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Diplomarbeit/")

path.hdf.in <- "/media/schmingo/Daten/Diplomarbeit/myd02-03_hdf/"
tifsdir <- "/media/schmingo/Daten/Diplomarbeit/myd02_tif/"
mrtpath <- "/home/schmingo/apps/MRTswath/bin/swath2grid"

## Load required modules
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0210_writeMRTSwathParamFile_1000.R")
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0210_writeMRTSwathParamFile_500.R")
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0210_writeMRTSwathParamFile_250.R")
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0220_runSwath2Grid.R")
source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0230_renameSuffix.R")


################################################################################
### Preprocessing MYD35_L2 and MYD03 | Run MRTSwath tool "swath2grid" ##########
################################################################################

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
################################################################################

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
################################################################################

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
################################################################################

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
################################################################################

do.call(file.remove,list(list.files(tifsdir, pattern="Aggr", full.names=TRUE)))
do.call(file.remove,list(list.files(tifsdir, pattern="Uncert", full.names=TRUE)))
do.call(file.remove,list(list.files(tifsdir, pattern="Band26", full.names=TRUE)))


################################################################################
### Check actual time again ####################################################
################################################################################

endtime <- Sys.time()

time <- endtime - starttime
time

################################################################################
### Rename .tif to proper filename e.g.: *_B20.tif #############################
################################################################################

lst.tif <- list.files(tifsdir, pattern=".tif", full.names=TRUE)

suffixes.in <- c("1KM_Emissive_b0.tif", 
                 "1KM_Emissive_b1.tif",
                 "1KM_Emissive_b2.tif",
                 "1KM_Emissive_b3.tif",
                 "1KM_Emissive_b4.tif",
                 "1KM_Emissive_b5.tif",
                 "1KM_Emissive_b6.tif",
                 "1KM_Emissive_b7.tif",
                 "1KM_Emissive_b8.tif",
                 "1KM_Emissive_b9.tif",
                 "1KM_Emissive_b10.tif",
                 "1KM_Emissive_b11.tif",
                 "1KM_Emissive_b12.tif",
                 "1KM_Emissive_b13.tif",
                 "1KM_Emissive_b14.tif",
                 "1KM_Emissive_b15.tif",
                 "1KM_RefSB_b0.tif",
                 "1KM_RefSB_b1.tif",
                 "1KM_RefSB_b2.tif",
                 "1KM_RefSB_b3.tif",
                 "1KM_RefSB_b4.tif",
                 "1KM_RefSB_b5.tif",
                 "1KM_RefSB_b6.tif",
                 "1KM_RefSB_b7.tif",
                 "1KM_RefSB_b8.tif",
                 "1KM_RefSB_b9.tif",
                 "1KM_RefSB_b10.tif",
                 "1KM_RefSB_b11.tif",
                 "1KM_RefSB_b12.tif",
                 "1KM_RefSB_b13.tif",
                 "1KM_RefSB_b14.tif",
                 "250_RefSB_b0.tif",
                 "250_RefSB_b1.tif",
                 "500_RefSB_b0.tif",
                 "500_RefSB_b1.tif",
                 "500_RefSB_b2.tif",
                 "500_RefSB_b3.tif",
                 "500_RefSB_b4.tif")                   

suffixes.out <- c("B20", 
                  "B21",
                  "B22",
                  "B23",
                  "B24",
                  "B25",
                  "B27",
                  "B28",
                  "B29",
                  "B30",
                  "B31",
                  "B32",
                  "B33",
                  "B34",
                  "B35",
                  "B36",
                  "B08",
                  "B09",
                  "B10",
                  "B11",
                  "B12",
                  "B13.1",
                  "B13.2",
                  "B14.1",
                  "B14.2",
                  "B15",
                  "B16",
                  "B17",
                  "B18",
                  "B19",
                  "B26",
                  "B01",
                  "B02",
                  "B03",
                  "B04",
                  "B05",
                  "B06",
                  "B07")

foreach(i = suffixes.in, j = suffixes.out) %do% {
  renameSuffix(files = lst.tif, 
               suffix.in = i, 
               suffix.out = j, 
               tifsdir)
}
