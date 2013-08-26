################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT RADIANCE SCALES FOR EACH MODIS-BAND FROM HDF FILES                 ##
##                                                                            ##
##                                                                            ##
## Important Note: This script will only work under Linux!                    ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-08-25                                                        ##
##                                                                            ##
################################################################################

hdfExtractRadScale <- function(path.wd,
                               path.250.hdf,
                               path.500.hdf,
                               path.1km.hdf
                               )
  {


  ## Required packages
  stopifnot(require(rgdal))

  ## Data folder
  setwd(path.wd)

  ## GDALinfo from HDF
  info.250.hdf <- GDALinfo(path.250.hdf, returnScaleOffset = F)
  info.500.hdf <- GDALinfo(path.500.hdf, returnScaleOffset = F)
  info.1km.hdf <- GDALinfo(path.1km.hdf, returnScaleOffset = F)
  
  
  ## HDF attributes
  #attributes(info.250.hdf) # display attributes
  subds.250 <- attr(info.250.hdf, "subdsmdata") # display subdatasets
  subds.500 <- attr(info.500.hdf, "subdsmdata") # display subdatasets
  subds.1km <- attr(info.1km.hdf, "subdsmdata") # display subdatasets
  
  
  ## Identify relevant SubDS via regular expression
  refsb.250 <- subds.250[grep("EOS_SWATH.*EV_250_RefSB$", subds.250)]
  refsb.500 <- subds.500[grep("EOS_SWATH.*EV_500_RefSB$", subds.500)]
  refsb.1km <- subds.1km[grep("EOS_SWATH.*EV_1KM_RefSB$", subds.1km)]
  emiss.1km <- subds.1km[grep("EOS_SWATH.*EV_1KM_Emissive$", subds.1km)]
  
  
  ## Remove irrelevant parts of SubDS name
  refsb.250 <- unlist(strsplit(refsb.250, "="))[2]
  refsb.500 <- unlist(strsplit(refsb.500, "="))[2]
  refsb.1km <- unlist(strsplit(refsb.1km, "="))[2]
  emiss.1km <- unlist(strsplit(emiss.1km, "="))[2]
  
  
  ## GDALinfo from SubDS
  info.subds.refsb.250 <- GDALinfo(refsb.250)
  info.subds.refsb.500 <- GDALinfo(refsb.500)
  info.subds.refsb.1km <- GDALinfo(refsb.1km)
  info.subds.emiss.1km <- GDALinfo(emiss.1km)
  
  
  ## Extract radiance scale from SubDS metadata
  mdata.subds.refsb.250 <- attr(info.subds.refsb.250, "mdata")
  mdata.subds.refsb.500 <- attr(info.subds.refsb.500, "mdata")
  mdata.subds.refsb.1km <- attr(info.subds.refsb.1km, "mdata")
  mdata.subds.emiss.1km <- attr(info.subds.emiss.1km, "mdata")
  
  scales.refsb.250 <- mdata.subds.refsb.250[grep("radiance_scales", mdata.subds.refsb.250)]
  scales.refsb.500 <- mdata.subds.refsb.500[grep("radiance_scales", mdata.subds.refsb.500)]
  scales.refsb.1km <- mdata.subds.refsb.1km[grep("radiance_scales", mdata.subds.refsb.1km)]
  scales.emiss.1km <- mdata.subds.emiss.1km[grep("radiance_scales", mdata.subds.emiss.1km)]
  
  scales.refsb.250 <- unlist(strsplit(scales.refsb.250, "="))[2]
  scales.refsb.500 <- unlist(strsplit(scales.refsb.500, "="))[2]
  scales.refsb.1km <- unlist(strsplit(scales.refsb.1km, "="))[2]
  scales.emiss.1km <- unlist(strsplit(scales.emiss.1km, "="))[2]
  
  
  ## write extracted values as numeric list
  sapply(strsplit(scales.refsb.250, ", "), as.numeric)
  sapply(strsplit(scales.refsb.500, ", "), as.numeric)
  sapply(strsplit(scales.refsb.1km, ", "), as.numeric)
  sapply(strsplit(scales.emiss.1km, ", "), as.numeric)
  
  
  ## return extracted values to call-script
  return(list(scales.refsb.250, scales.refsb.500, scales.refsb.1km, scales.emiss.1km))

}