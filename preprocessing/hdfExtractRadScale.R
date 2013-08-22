################################################################################
## BiFoRe Scripts
##
## EXTRACT RADIANCE SCALES FOR EACH MODIS-BAND FROM HDF FILES
## 
## 
## Important Note: This script will only work under Linux!
## 
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
refsb250 <- subds[grep("EOS_SWATH.*EV_250_Aggr1km_RefSB$", subds)]
refsb500 <- subds[grep("EOS_SWATH.*EV_500_Aggr1km_RefSB$", subds)]
band26 <- subds[grep("EOS_SWATH.*EV_Band26$", subds)]

# Remove irrelevant parts of SubDS name
refsb <- unlist(strsplit(refsb, "="))[2]
emissive <- unlist(strsplit(emissive, "="))[2]
refsb250 <- unlist(strsplit(refsb250, "="))[2]
refsb500 <- unlist(strsplit(refsb500, "="))[2]
band26 <- unlist(strsplit(band26, "="))[2]

# GDALinfo from SubDS
subds.info.refsb <- GDALinfo(refsb)
subds.info.emissive <- GDALinfo(emissive)
subds.info.refsb250 <- GDALinfo(refsb250)
subds.info.refsb500 <- GDALinfo(refsb500)
subds.info.band26 <- GDALinfo(band26)

# Extract radiance scale from SubDS metadata
subds.mdata.refsb <- attr(subds.info.refsb, "mdata")
subds.mdata.emissive <- attr(subds.info.emissive, "mdata")
subds.mdata.refsb250 <- attr(subds.info.refsb250, "mdata")
subds.mdata.refsb500 <- attr(subds.info.refsb500, "mdata")
subds.mdata.band26 <- attr(subds.info.band26, "mdata")

scales.EV_1KM_RefSB <- subds.mdata.refsb[grep("radiance_scales", subds.mdata.refsb)]
scales.EV_1KM_Emissive <- subds.mdata.emissive[grep("radiance_scales", subds.mdata.emissive)]
scales.EV_250_Aggr1km_RefSB <- subds.mdata.refsb250[grep("radiance_scales", subds.mdata.refsb250)]
scales.EV_500_Aggr1km_RefSB <- subds.mdata.refsb500[grep("radiance_scales", subds.mdata.refsb500)]
scales.EV_BAND26 <- subds.mdata.band26[grep("radiance_scales", subds.mdata.band26)]

# Print radiance scales
print(scales.EV_1KM_RefSB)
print(scales.EV_1KM_Emissive)
print(scales.EV_250_Aggr1km_RefSB)
print(scales.EV_500_Aggr1km_RefSB)
print(scales.EV_BAND26)