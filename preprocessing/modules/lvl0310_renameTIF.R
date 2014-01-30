################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RENAME MODIS FILES                                                         ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-30                                                        ##
##                                                                            ##
################################################################################

rename_modis_files <- function(path.wd, path.modis)
{
  stopifnot(require(foreach))
  
  setwd(paste0(path.wd, path.modis))
  source("/home/schmingo/Diplomarbeit/bifore/preprocessing/renameSuffix.R")
  
  suffixes.in <- c("1KM_Emissive_b0.tif", 
                   "1KM_Emissive_b1.tif",
                   "1KM_Emissive_b2.tif",
                   "1KM_Emissive_b3.tif",
                   "1KM_Emissive_b4.tif",
                   "1KM_Emissive_b5.tif",
                   "1KM_Emissive_b6.tif",
                   "1KM_Emissive_b7.tif",
                   "1KM_Emissive_b8.tif",
                   "1KM_Emissive_b9.tif",
                   "1KM_Emissive_b10.tif",
                   "1KM_Emissive_b11.tif",
                   "1KM_Emissive_b12.tif",
                   "1KM_Emissive_b13.tif",
                   "1KM_Emissive_b14.tif",
                   "1KM_Emissive_b15.tif",
                   "1KM_RefSB_b0.tif",
                   "1KM_RefSB_b1.tif",
                   "1KM_RefSB_b2.tif",
                   "1KM_RefSB_b3.tif",
                   "1KM_RefSB_b4.tif",
                   "1KM_RefSB_b5.tif",
                   "1KM_RefSB_b6.tif",
                   "1KM_RefSB_b7.tif",
                   "1KM_RefSB_b8.tif",
                   "1KM_RefSB_b9.tif",
                   "1KM_RefSB_b10.tif",
                   "1KM_RefSB_b11.tif",
                   "1KM_RefSB_b12.tif",
                   "1KM_RefSB_b13.tif",
                   "1KM_RefSB_b14.tif",
                   "250_RefSB_b0.tif",
                   "250_RefSB_b1.tif",
                   "500_RefSB_b0.tif",
                   "500_RefSB_b1.tif",
                   "500_RefSB_b2.tif",
                   "500_RefSB_b3.tif",
                   "500_RefSB_b4.tif")                   
  
  suffixes.out <- c("B20.tif", 
                    "B21.tif",
                    "B22.tif",
                    "B23.tif",
                    "B24.tif",
                    "B25.tif",
                    "B27.tif",
                    "B28.tif",
                    "B29.tif",
                    "B30.tif",
                    "B31.tif",
                    "B32.tif",
                    "B33.tif",
                    "B34.tif",
                    "B35.tif",
                    "B36.tif",
                    "B08.tif",
                    "B09.tif",
                    "B10.tif",
                    "B11.tif",
                    "B12.tif",
                    "B13.1.tif",
                    "B13.2.tif",
                    "B14.1.tif",
                    "B14.2.tif",
                    "B15.tif",
                    "B16.tif",
                    "B17.tif",
                    "B18.tif",
                    "B19.tif",
                    "B26.tif",
                    "B01.tif",
                    "B02.tif",
                    "B03.tif",
                    "B04.tif",
                    "B05.tif",
                    "B06.tif",
                    "B07.tif",)
  
  foreach(i = suffixes.in, j = suffixes.out) %do% {
    renameSuffix(files = lst.tif, 
                 suffix.in = i, 
                 suffix.out = j, 
                 dir.out = "/home/schmingo/Diplomarbeit/sample_myd02_tif_rename/")
  }
  
  file.rename("band__EV_1KM_Emissive_b0.tif", "B20.tif")
  file.rename("band__EV_1KM_Emissive_b1.tif", "B21.tif")
  file.rename("band__EV_1KM_Emissive_b2.tif", "B22.tif")
  file.rename("band__EV_1KM_Emissive_b3.tif", "B23.tif")
  file.rename("band__EV_1KM_Emissive_b4.tif", "B24.tif")
  file.rename("band__EV_1KM_Emissive_b5.tif", "B25.tif")
  file.rename("band__EV_1KM_Emissive_b6.tif", "B27.tif")
  file.rename("band__EV_1KM_Emissive_b7.tif", "B28.tif")
  file.rename("band__EV_1KM_Emissive_b8.tif", "B29.tif")
  file.rename("band__EV_1KM_Emissive_b9.tif", "B30.tif")
  file.rename("band__EV_1KM_Emissive_b10.tif", "B31.tif")
  file.rename("band__EV_1KM_Emissive_b11.tif", "B32.tif")
  file.rename("band__EV_1KM_Emissive_b12.tif", "B33.tif")
  file.rename("band__EV_1KM_Emissive_b13.tif", "B34.tif")
  file.rename("band__EV_1KM_Emissive_b14.tif", "B35.tif")
  file.rename("band__EV_1KM_Emissive_b15.tif", "B36.tif")
  file.rename("band__EV_1KM_RefSB_b0.tif", "B08.tif")
  file.rename("band__EV_1KM_RefSB_b1.tif", "B09.tif")
  file.rename("band__EV_1KM_RefSB_b2.tif", "B10.tif")
  file.rename("band__EV_1KM_RefSB_b3.tif", "B11.tif")
  file.rename("band__EV_1KM_RefSB_b4.tif", "B12.tif")
  file.rename("band__EV_1KM_RefSB_b5.tif", "B13.1.tif")
  file.rename("band__EV_1KM_RefSB_b6.tif", "B13.2.tif")
  file.rename("band__EV_1KM_RefSB_b7.tif", "B14.1.tif")
  file.rename("band__EV_1KM_RefSB_b8.tif", "B14.2.tif")
  file.rename("band__EV_1KM_RefSB_b9.tif", "B15.tif")
  file.rename("band__EV_1KM_RefSB_b10.tif", "B16.tif")
  file.rename("band__EV_1KM_RefSB_b11.tif", "B17.tif")
  file.rename("band__EV_1KM_RefSB_b12.tif", "B18.tif")
  file.rename("band__EV_1KM_RefSB_b13.tif", "B19.tif")
  file.rename("band__EV_1KM_RefSB_b14.tif", "B26.tif")
  file.rename("band__EV_250_RefSB_b0.tif", "B01.tif")
  file.rename("band__EV_250_RefSB_b1.tif", "B02.tif")
  file.rename("band__EV_500_RefSB_b0.tif", "B03.tif")
  file.rename("band__EV_500_RefSB_b1.tif", "B04.tif")
  file.rename("band__EV_500_RefSB_b2.tif", "B05.tif")
  file.rename("band__EV_500_RefSB_b3.tif", "B06.tif")
  file.rename("band__EV_500_RefSB_b4.tif", "B07.tif")
}