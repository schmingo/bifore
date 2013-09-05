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
# ## Clear workspace
# rm(list = ls(all = TRUE))
# 
# 
# ## Required libraries
# lib <- c("rgdal", "parallel", "raster")
# lapply(lib, function(...) require(..., character.only = TRUE))
# 
# 
# ## Set filepaths and filenames
# path.wd <- "/home/schmingo/Diplomarbeit/" # Linux
# #path.wd <- "D:/Diplomarbeit/" # Windows
# 
# path.modis <- "/home/schmingo/Diplomarbeit/src/satellite/MOD02_2013-07-07/"
# path.raw.modis <- "/home/schmingo/Diplomarbeit/src/satellite/RAW_MODIS_2013-07-07/"
# path.250.hdf <- "MOD02QKM.A2013188.1120.005.2013188200351.hdf"
# path.500.hdf <- "MOD02HKM.A2013188.1120.005.2013188200351.hdf"
# path.1km.hdf <- "MOD021KM.A2013188.1120.005.2013188200351.hdf"

hdfExtractRadScale <- function(path.raw.modis,
                               path.250.hdf,
                               path.500.hdf,
                               path.1km.hdf
                               )
{

  
  print ("Extract Radiance Scale values from MODIS hdf files...")
  
  
  ## Required packages
  stopifnot(require(rgdal))


  
  ## GDALinfo from HDF
  info.250.hdf <- GDALinfo(paste(path.raw.modis,path.250.hdf, sep = ""), 
                           returnScaleOffset = F)
  info.500.hdf <- GDALinfo(paste(path.raw.modis,path.500.hdf, sep = ""), 
                           returnScaleOffset = F)
  info.1km.hdf <- GDALinfo(paste(path.raw.modis,path.1km.hdf, sep = ""), 
                           returnScaleOffset = F)
  
  
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
  
  
  ## Extract SubDS metadata
  mdata.subds.refsb.250 <- attr(info.subds.refsb.250, "mdata")
  mdata.subds.refsb.500 <- attr(info.subds.refsb.500, "mdata")
  mdata.subds.refsb.1km <- attr(info.subds.refsb.1km, "mdata")
  mdata.subds.emiss.1km <- attr(info.subds.emiss.1km, "mdata")
  
  ## Extract radiance-/reflectance_scales from metadata
  scales.refsb.250 <- mdata.subds.refsb.250[grep("reflectance_scales", mdata.subds.refsb.250)]
  scales.refsb.500 <- mdata.subds.refsb.500[grep("reflectance_scales", mdata.subds.refsb.500)]
  scales.refsb.1km <- mdata.subds.refsb.1km[grep("reflectance_scales", mdata.subds.refsb.1km)]
  scales.emiss.1km <- mdata.subds.emiss.1km[grep("radiance_scales", mdata.subds.emiss.1km)]
  
  scales.refsb.250 <- unlist(strsplit(scales.refsb.250, "="))[2]
  scales.refsb.500 <- unlist(strsplit(scales.refsb.500, "="))[2]
  scales.refsb.1km <- unlist(strsplit(scales.refsb.1km, "="))[2]
  scales.emiss.1km <- unlist(strsplit(scales.emiss.1km, "="))[2]
  
  ## Extract bandnames from metadata
  bands.refsb.250 <- mdata.subds.refsb.250[grep("band_names", mdata.subds.refsb.250)]
  bands.refsb.500 <- mdata.subds.refsb.500[grep("band_names", mdata.subds.refsb.500)]
  bands.refsb.1km <- mdata.subds.refsb.1km[grep("band_names", mdata.subds.refsb.1km)]
  bands.emiss.1km <- mdata.subds.emiss.1km[grep("band_names", mdata.subds.emiss.1km)]

  bands.refsb.250 <- unlist(strsplit(bands.refsb.250, "="))[2]
  bands.refsb.500 <- unlist(strsplit(bands.refsb.500, "="))[2]
  bands.refsb.1km <- unlist(strsplit(bands.refsb.1km, "="))[2]
  bands.emiss.1km <- unlist(strsplit(bands.emiss.1km, "="))[2]


  ## write extracted values as numeric list
  sapply(strsplit(scales.refsb.250, ", "), as.numeric)
  sapply(strsplit(scales.refsb.500, ", "), as.numeric)
  sapply(strsplit(scales.refsb.1km, ", "), as.numeric)
  sapply(strsplit(scales.emiss.1km, ", "), as.numeric)
  
  
  ##############################################################################
  ## Write Modis bandnames and radiance scales to new csv ######################
  
  ## paste bandnames and radiance scale
  bandnames <- paste(bands.refsb.250, 
                     bands.refsb.500, 
                     bands.refsb.1km,
                     bands.emiss.1km,
                     sep = ",")
  
  scales <- paste(scales.refsb.250,
                  scales.refsb.500,
                  scales.refsb.1km,
                  scales.emiss.1km,
                  sep = ", ")

  
  ## Write bandnames and radiance scales to separate dataframe
  bandnames <- data.frame(strsplit(unlist(bandnames), ","), 
                          stringsAsFactors = F)
  names(bandnames) <- "bands"
  
  scales <- data.frame(strsplit(unlist(scales), ", "), 
                       stringsAsFactors = F)
  names(scales) <- "scales"
  
  
  ## Rename "hi" and "lo" bands to numeric values
  for (i in seq(nrow(bandnames))) {
    if (bandnames[i,1] == "13lo")
      bandnames[i,1] <- "13.1"
    else if (bandnames[i,1] == "13hi")
      bandnames[i,1] <- "13.2"
    else if (bandnames[i,1] == "14lo")
      bandnames[i,] <- "14.1"
    else if (bandnames[i,1] == "14hi")
      bandnames[i,1] <- "14.2"
  }
  bandnames[,1] <- as.numeric(bandnames[,1])
  
  
  ## Write bandnames and radiance scales to a single dataframe
  radscales <- cbind(bandnames, scales)

  ## Order dataframe
  radscales <- radscales[ order(radscales[,1]), ]
  row.names(radscales) <- NULL

  ## return extracted values to call-script
  return(radscales)
}