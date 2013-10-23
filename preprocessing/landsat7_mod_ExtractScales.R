################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT RADIOMETRIC SCALES AND OFFSET FROM LANDSAT7 METADATA               ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-23                                                        ##
##                                                                            ##
################################################################################

ExtractLS7Scale <- function(path.img)
{
  
  name.metadata <- list.files(path.img,
                              pattern = "_MTL.txt",
                              full.names = TRUE)
  
  ## Import metadata
  metadata <- scan(file = name.metadata,
                   what = character(),
                   sep = "")

################################################################################
## Extract radiometric scales ##################################################
  
  refscale.b1 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_1", metadata)+2])
  refscale.b2 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_2", metadata)+2])
  refscale.b3 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_3", metadata)+2])
  refscale.b4 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_4", metadata)+2])
  refscale.b5 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_5", metadata)+2])
  refscale.b6 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_6_VCID_1", metadata)+2])
  refscale.b7 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_7", metadata)+2])
  refscale.b8 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_8", metadata)+2])
  

################################################################################
## Extract radiometric offsets #################################################
  
  refoffset.b1 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_1", metadata)+2])
  refoffset.b2 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_2", metadata)+2])
  refoffset.b3 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_3", metadata)+2])
  refoffset.b4 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_4", metadata)+2])
  refoffset.b5 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_5", metadata)+2])
  refoffset.b6 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_6_VCID_1", metadata)+2])
  refoffset.b7 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_7", metadata)+2])
  refoffset.b8 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_8", metadata)+2])
  

################################################################################
## Write scales and offset to single dataframe #################################

  scales <- paste(refscale.b1,
                  refscale.b2,
                  refscale.b3,
                  refscale.b4,
                  refscale.b5,
                  refscale.b6,
                  refscale.b7,
                  refscale.b8,
                  sep = ", ")
  
  offset <- paste(refoffset.b1,
                  refoffset.b2,
                  refoffset.b3,
                  refoffset.b4,
                  refoffset.b5,
                  refoffset.b6,
                  refoffset.b7,
                  refoffset.b8,
                  sep = ", ")
  

## Write bandnames, radiance- /reflectance scales and offset to separate dataframes

  bandnames <- data.frame(c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08"))
  names(bandnames) <- "bands"
  
  scales <- data.frame(strsplit(unlist(scales), ","),stringsAsFactors = F)
  names(scales) <- "multiply scale"
  
  offset <- data.frame(strsplit(unlist(offset), ","),stringsAsFactors = F)
  names(offset) <- "add offset"
  
  
  scales.ls7 <- cbind(bandnames, scales, offset)
  
  return(scales.ls7)

}
