# Required packages
library(rgdal)

# Data folder
path.wd <- "/home/dogbert/Downloads"
setwd(path.wd)

# GDALinfo from HDF
hdf.info <- GDALinfo("MOD021KM.A2013226.2000.005.2013227040327.hdf", 
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