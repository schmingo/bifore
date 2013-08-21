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

# # HDF attributes
# attributes(hdf.info) # display attributes
subds <- attr(hdf.info, "subdsmdata") # display subdatasets

# Identify relevant SubDS via regular expression
refsb <- subds[grep("EOS_SWATH.*EV_1KM_RefSB$", subds)]
emissive <- subds[grep("EOS_SWATH.*EV_1KM_Emissive$", subds)]
# Remove irrelevant parts of SubDS name
refsb <- unlist(strsplit(refsb, "="))[2]
emissive <- unlist(strsplit(emissive, "="))[2]
# GDALinfo from SubDS
subds.info.refsb <- GDALinfo(refsb)
subds.info.emissive <- GDALinfo(emissive)

# Extract radiance scale from SubDS metadata
subds.mdata.refsb <- attr(subds.info.refsb, "mdata")
subds.mdata.emissive <- attr(subds.info.emissive, "mdata")

scales.EV_1KM_RefSB <- subds.mdata.refsb[grep("radiance_scales", subds.mdata.refsb)]
scales.EV_1KM_Emissive <- subds.mdata.emissive[grep("radiance_scales", subds.mdata.emissive)]
print(scales.EV_1KM_RefSB)