################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT RADIANCE-/ REFLECTANCE SCALES FOR EACH MODIS-BAND FROM HDF FILES   ##
##                                                                            ##
##                                                                            ##
## Important Note: This script will only work under Linux!                    ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-12                                                        ##
##                                                                            ##
################################################################################
# ## Clear workspace
# rm(list = ls(all = TRUE))
# 
# 
# ## Required libraries
# lib <- c("rgdal", "doParallel", "raster", "matrixStats")
# lapply(lib, function(...) require(..., character.only = TRUE))
# 
# ## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
# 
# ################################################################################
# ### Set filepaths ##############################################################
# 
# path.hdf <- "/home/schmingo/Diplomarbeit/sample_myd02_hdf/"
# path.tif <- "/home/schmingo/Diplomarbeit/sample_myd02_tif/"
# path.biodiversity.csv <- "csv/kili/lvl0100_biodiversity_data.csv"
# 
# ## Source modules
# source("/home/schmingo/Diplomarbeit/bifore/preprocessing/modules/lvl0320_hdfExtractScales.R")
# 
# ################################################################################
# ### Import biodiversity dataset ################################################
# 
# data.bio.raw <- read.csv2(path.biodiversity.csv,
#                           dec = ".",
#                           header = TRUE, 
#                           stringsAsFactors = FALSE)
# 
# 
# ################################################################################
# ### List .hdf and .tif for specific date and import .tif as RasterLayer Object##
# 
# ## List unique cloudless dates
# lst.date <- unique(data.bio.raw$date_nocloud)
# lst.date
# 
# # foreach(a = lst.date) %do% {
#   
#   ## Extract date from biodiversity data
# #   tmp.date <- a
#   tmp.date <- data.bio.raw$date_nocloud[1]
#   
#   ## Reformat date
#   tmp.date <- paste0(substr(tmp.date, 1, 4),
#                      substr(tmp.date, 6, 8),
#                      ".",
#                      substr(tmp.date, 10, 13))
#   
#   ## List .tif files
#   lst.tif <- list.files(path.tif, pattern = tmp.date, full.names = TRUE)
#   
#   ## Import .tif files as RasterLayer objects
#   lst.tif.raster <- lapply(lst.tif, raster)
#   
#   ### List .hdf files
#   lst.hdf.1km <- list.files(path.hdf, 
#                             pattern = paste("1KM", tmp.date, sep = ".*"), 
#                             full.names = TRUE)
#   lst.hdf.hkm <- list.files(path.hdf, 
#                             pattern = paste("HKM", tmp.date, sep = ".*"), 
#                             full.names = TRUE)
#   lst.hdf.qkm <- list.files(path.hdf, 
#                             pattern = paste("QKM", tmp.date, sep = ".*"), 
#                             full.names = TRUE)
  
  
################################################################
################################################################
################################################################
################################################################
################################################################


hdfExtractMODScale <- function(lst.hdf.qkm,
                               lst.hdf.hkm,
                               lst.hdf.1km
)
{
  
  ## Required packages
  stopifnot(require(rgdal))
  
  
  ## GDALinfo from HDF
  info.250.hdf <- GDALinfo(lst.hdf.qkm, returnScaleOffset = F)
  info.500.hdf <- GDALinfo(lst.hdf.hkm, returnScaleOffset = F)
  info.1km.hdf <- GDALinfo(lst.hdf.1km, returnScaleOffset = F)
  
  
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
  
  ## write extracted values as numeric list
  sapply(strsplit(scales.refsb.250, ", "), as.numeric)
  sapply(strsplit(scales.refsb.500, ", "), as.numeric)
  sapply(strsplit(scales.refsb.1km, ", "), as.numeric)
  sapply(strsplit(scales.emiss.1km, ", "), as.numeric)
  
  ## Extract bandnames from metadata
  bands.refsb.250 <- mdata.subds.refsb.250[grep("band_names", mdata.subds.refsb.250)]
  bands.refsb.500 <- mdata.subds.refsb.500[grep("band_names", mdata.subds.refsb.500)]
  bands.refsb.1km <- mdata.subds.refsb.1km[grep("band_names", mdata.subds.refsb.1km)]
  bands.emiss.1km <- mdata.subds.emiss.1km[grep("band_names", mdata.subds.emiss.1km)]
  
  bands.refsb.250 <- unlist(strsplit(bands.refsb.250, "="))[2]
  bands.refsb.500 <- unlist(strsplit(bands.refsb.500, "="))[2]
  bands.refsb.1km <- unlist(strsplit(bands.refsb.1km, "="))[2]
  bands.emiss.1km <- unlist(strsplit(bands.emiss.1km, "="))[2]
  
  ## Exctract Offset from metadata
  offset.refsb.250 <- mdata.subds.refsb.250[grep("reflectance_offsets", mdata.subds.refsb.250)]
  offset.refsb.500 <- mdata.subds.refsb.500[grep("reflectance_offsets", mdata.subds.refsb.500)]
  offset.refsb.1km <- mdata.subds.refsb.1km[grep("reflectance_offsets", mdata.subds.refsb.1km)]
  offset.emiss.1km <- mdata.subds.emiss.1km[grep("radiance_offsets", mdata.subds.emiss.1km)]
  
  offset.refsb.250 <- unlist(strsplit(offset.refsb.250, "="))[2]
  offset.refsb.500 <- unlist(strsplit(offset.refsb.500, "="))[2]
  offset.refsb.1km <- unlist(strsplit(offset.refsb.1km, "="))[2]
  offset.emiss.1km <- unlist(strsplit(offset.emiss.1km, "="))[2]
  
  ## write extracted values as numeric list
  sapply(strsplit(offset.refsb.250, ", "), as.numeric)
  sapply(strsplit(offset.refsb.500, ", "), as.numeric)
  sapply(strsplit(offset.refsb.1km, ", "), as.numeric)
  sapply(strsplit(offset.emiss.1km, ", "), as.numeric)
  
  ##############################################################################
  ## Write Modis bandnames and radiance scales to a single dataframe ###########
  
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
  
  offsets <- paste(offset.refsb.250,
                   offset.refsb.500,
                   offset.refsb.1km,
                   offset.emiss.1km,
                   sep = ", ")
  
  
  ## Write bandnames, radiance scales and offsets to separate dataframe
  bandnames <- data.frame(strsplit(unlist(bandnames), ","), 
                          stringsAsFactors = F)
  names(bandnames) <- "bands"
  
  scales <- data.frame(strsplit(unlist(scales), ", "), 
                       stringsAsFactors = F)
  names(scales) <- "scales"
  
  offsets <- data.frame(strsplit(unlist(offsets), ", "), 
                        stringsAsFactors = F)
  names(offsets) <- "offsets"
  
  
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
  modscales <- data.frame(cbind(bandnames, scales, offsets))
  
  ## Order data frame
  modscales <- modscales[ order(modscales[,1]), ]
  row.names(modscales) <- NULL
  
  ## return extracted values to call-script
  return(modscales)
}