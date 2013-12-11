################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CHECK COORDINATES FOR CLOUDS USING MODIS CLOUDMASK AND MODISCLOUD-PACKAGE  ##
##                                                                            ##
## Ref.: http://modis-atmos.gsfc.nasa.gov/_docs/CMUSERSGUIDE.pdf              ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-12-11                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("modiscloud", "devtools", "doParallel", "rgdal")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
# setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

## MOD35_L2 and MOD03 files; both must be in the same directory.

path.tif.out <- ("satellite/sample_modiscloud_out/")
path.hdf.in <- ("satellite/sample_modiscloud_in")

mrtpath <- ("/home/schmingo/apps/MRTSwath/bin/swath2grid")




################################################################################
### Run MRTSwath tool "swath2grid" #############################################

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


for (i in 1:nrow(fns_df)) {

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
### Load a TIF #################################################################

## get .tif list from swath2grid output
tiffns <- list.files(path.tif.out, pattern=".tif", full.names=TRUE)
tiffns

fn <- tiffns[1]
grd <- readGDAL(fn)

grdproj <- CRS(proj4string(grd))
grdproj
grdbbox <- attr(grd, "bbox")
grdbbox


################################################################################
### Extract values from a particular pixel #####################################

# Greg's field site
greglat = 10.2971
greglon = -84.79282

grdr = raster(grd)

# Input the points x (longitude), then y (latitude)
point_to_sample = c(greglon, greglat)
xycoords = adf(matrix(data=point_to_sample, nrow=1, ncol=2))
names(xycoords) = c("x", "y")

xy = SpatialPoints(coords=xycoords, proj4string=grdproj)
#xy = spsample(x=grd, n=10, type="random")
pixelval = extract(grdr, xy)

# Have to convert to 8-bit binary string, and reverse to get the count correct
# (also reverse the 2-bit strings in the MODIS Cloud Mask table)
pixelval = rev(t(digitsBase(pixelval, base= 2, 8)))
print(pixelval)