################################################################################
## BiFoRe Scripts
##
## EXTRACT SCALEFACTOR FOR EACH BAND FROM MODIS FILES
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-08-21
##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

# Required packages
library(rgdal)

# Data folder
path.wd <- "/home/schmingo/Diplomarbeit/src/satellite/RAW_MODIS_2013-07-07/"
setwd(path.wd)

# GDALinfo from HDF
hdf.info <- GDALinfo("MOD021KM.A2013188.1120.005.2013188200351.hdf", 
                     returnScaleOffset = F)

# HDF attributes
attributes(hdf.info) # display attributes
subds <- attr(hdf.info, "subdsmdata") # display subdatasets

# Identify relevant SubDS via regular expression
refsb <- subds[grep("EOS_SWATH.*EV_1KM_RefSB$", subds)]
# Remove irrelevant parts of SubDS name
refsb <- unlist(strsplit(refsb, "="))[2]

# GDALinfo from SubDS
subds.info <- GDALinfo(refsb)

# Extract reflectance scale and offset from SubDS metadata
subds.mdata <- attr(subds.info, "mdata")

scales <- subds.mdata[grep("reflectance_scales", subds.mdata)]
offset <- subds.mdata[grep("reflectance_offsets", subds.mdata)]