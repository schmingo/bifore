################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CHECK COORDINATES FOR CLOUDS USING MODIS CLOUDMASK                         ##
##                                                                            ##
## Ref.: http://modis-atmos.gsfc.nasa.gov/_docs/CMUSERSGUIDE.pdf              ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-12-09                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("modiscloud", "devtools", "doParallel")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
# setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

## MOD35_L2 and MOD03 files; both must be in the same directory.

tifsdir = ("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/satellite/sample_modiscloud_out/")
# path.tif.out = ("satellite/sample_modiscloud_out")
path.hdf.in = ("satellite/sample_modiscloud_in")
# path.35tif.in = ("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/satellite/sample_mod35L2_kilimanjaro_2002")
# path.03tif.in = ("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/satellite/sample_mod03_kilimanjaro_mrz2002")

mrtpath = ("/home/schmingo/apps/MRTSwath/bin/swath2grid")
#mrtpath = ("C:/MRTSwath_Win/bin")
#swath2grid <- get_path()

# source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modis_mod_writeMRTSwathParamFile.R")
# source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modis_mod_runSwath2Grid.R")




################################################################################
### Run MRTSwath tool "swath2grid" #############################################

# list.files(pattern = "MOD")
list.files(path = path.hdf.in, pattern = "MOD")

# check_for_matching_geolocation_files(moddir = path.hdf.in,
#                                      modtxt = "MOD35_L2", 
#                                      geoloctxt = "MOD03",
#                                      return_geoloc = FALSE, 
#                                      return_product = FALSE)

# Get the matching data/geolocation file pairs
fns_df = check_for_matching_geolocation_files(moddir = path.hdf.in,
                                              modtxt = "MOD35_L2",
                                              geoloctxt = "MOD03", 
                                              return_geoloc = FALSE, 
                                              return_product = FALSE)
fns_df

#check_for_matching_geolocation_files(modtxt = "MYD35_L2",geoloctxt = "MYD03", 
#                                     return_geoloc = FALSE, return_product = FALSE)


# Box to subset
ul_lat = -2.77
ul_lon = 36.93
lr_lat = -3.45
lr_lon = 37.76

# Parallelization
# registerDoParallel(cl <- makeCluster(4))

for (i in 1:nrow(fns_df)) {
  
#   foreach(sds = c("Cloud_Effective_Radius", "Cloud_Optical_Thickness", 
#                   "Cloud_Water_Path", 
#                   "Cirrus_Reflectance", "Cirrus_Reflectance_Flag"), 
#           .packages = "modiscloud") %do% {
  prmfn = write_MRTSwath_param_file(prmfn="/home/schmingo/Diplomarbeit/tmpMRTparams.prm", 
                                    tifsdir=tifsdir, 
                                    modfn=fns_df$mod35_L2_fns[i], 
                                    geoloc_fn=fns_df$mod03_fns[i], 
                                    ul_lon=ul_lon, 
                                    ul_lat=ul_lat, 
                                    lr_lon=lr_lon, 
                                    lr_lat=lr_lat)
  
  print(scan(file=prmfn, what="character", sep="\n"))           
  
  run_swath2grid(mrtpath="swath2grid", 
               prmfn="/home/schmingo/Diplomarbeit/tmpMRTparams.prm", 
               tifsdir=tifsdir, 
               modfn=fns_df$mod35_L2_fns[i], 
               geoloc_fn=fns_df$mod03_fns[i], 
               ul_lon=ul_lon, 
               ul_lat=ul_lat, 
               lr_lon=lr_lon, 
               lr_lat=lr_lat)
}
#             prmfn = writeMRTSwathParamFile(prmfn = "tmpMRTparams.prm", 
#                                            tifsdir = tifsdir, 
#                                            modfn = fns_df$mod35_L2_fns[i], 
#                                            geoloc_fn = fns_df$mod03_fns[i], 
# #                                            sds = sds, 
#                                            ul_lon = ul_lon, ul_lat = ul_lat, 
#                                            lr_lon = lr_lon, lr_lat = lr_lat)
#             
#             runSwath2Grid(mrtpath = mrtpath, 
#                           prmfn = "tmpMRTparams.prm", 
#                           tifsdir = tifsdir, 
#                           modfn = fns_df$mod35_L2_fns[i], 
#                           geoloc_fn = fns_df$mod03_fns[i], 
#                           ul_lon = ul_lon, ul_lat = ul_lat, lr_lon = lr_lon, lr_lat = lr_lat)
#           }
# }

# tiffns = list.files(tifsdir, pattern = ".tif", full.names = TRUE)
# tiffns
# 
# clusterEvalQ(cl, library(raster))
# clusterExport(cl, "tiffns")
# 
# tiffrst <- parLapply(cl, tiffns, function(x) { 
#   rst <- raster(x)
#   
#   rst.prj <- projectRaster(rst, crs = "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#   
#   if (ifelse(length(grep("Cloud_Effective_Radius", x) == 1) > 0, T, F) |
#       ifelse(length(grep("Cloud_Optical_Thickness", x) == 1) > 0, T, F)) {
#     scale.factor <- 0.009999999776482582
#   } else if (ifelse(length(grep("Cirrus_Reflectance", x) == 1) > 0, T, F)) {
#     scale.factor <- 1.9999999494757503e-4
#   } else {
#     scale.factor <- 1
#   }
#   
#   rst.prj.scl <- rst.prj * scale.factor
#   
#   writeRaster(rst.prj.scl, paste("tifs/prj_scl", basename(x), sep = "/"), 
#               format = "GTiff", overwrite = T)
#   
# #   rst.prj.scl.fcl <- focal(rst.prj.scl, w = 5, fun = sd, 
# #                            filename = paste("tifs/prj_scl_fcl", basename(x), sep = "/"), 
# #                            format = "GTiff", overwrite = T)
# })
# 
# stopCluster(cl)
