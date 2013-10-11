################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT RADIANCE-/ REFLECTANCE SCALES AND OFFSET FROM LANDSAT8 METADATA    ##                                                                        ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-11                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


path.img <- "D:/Dropbox/Diplomarbeit/code/bifore/src/satellite/Landsat8/hai/"

name.metadata <- list.files(path.img,
                            pattern = ".txt",
                            full.names = TRUE)

## Import metadata
metadata <- scan(file = name.metadata,
                 what = character(),
                 sep = "")

################################################################################
## Extract Reflectance Scale ###################################################

refscale.b1 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_1", metadata)+2])
refscale.b2 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_2", metadata)+2])
refscale.b3 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_3", metadata)+2])
refscale.b4 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_4", metadata)+2])
refscale.b5 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_5", metadata)+2])
refscale.b6 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_6", metadata)+2])
refscale.b7 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_7", metadata)+2])
refscale.b8 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_8", metadata)+2])
refscale.b9 <- as.numeric(metadata[grep("REFLECTANCE_MULT_BAND_9", metadata)+2])

radscale.b10 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_10", metadata)+2])
radscale.b11 <- as.numeric(metadata[grep("RADIANCE_MULT_BAND_11", metadata)+2])


################################################################################
## Extract Reflectance Offset ##################################################

refoffset.b1 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_1", metadata)+2])
refoffset.b2 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_2", metadata)+2])
refoffset.b3 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_3", metadata)+2])
refoffset.b4 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_4", metadata)+2])
refoffset.b5 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_5", metadata)+2])
refoffset.b6 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_6", metadata)+2])
refoffset.b7 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_7", metadata)+2])
refoffset.b8 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_8", metadata)+2])
refoffset.b9 <- as.numeric(metadata[grep("REFLECTANCE_ADD_BAND_9", metadata)+2])

radoffset.b10 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_10", metadata)+2])
radoffset.b11 <- as.numeric(metadata[grep("RADIANCE_ADD_BAND_11", metadata)+2])