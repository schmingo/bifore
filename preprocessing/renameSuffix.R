################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RENAME MODIS FILES                                                         ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-30                                                        ##
##                                                                            ##
################################################################################
# rm(list = ls(all = TRUE))
# 
# tifsdir <- "/home/schmingo/Diplomarbeit/sample_myd02_tif/"
# tifsdir.rename <- "/home/schmingo/Diplomarbeit/sample_myd02_tif_rename/"
# 
# lst.tif <- list.files(tifsdir, pattern=".tif", full.names=TRUE)

# suffixes.in <- c("1KM_Emissive_b0.tif", 
#                  "1KM_Emissive_b1.tif",
#                  "1KM_Emissive_b2.tif",
#                  "1KM_Emissive_b3.tif",
#                  "1KM_Emissive_b4.tif",
#                  "1KM_Emissive_b5.tif",
#                  "1KM_Emissive_b6.tif",
#                  "1KM_Emissive_b7.tif",
#                  "1KM_Emissive_b8.tif",
#                  "1KM_Emissive_b9.tif",
#                  "1KM_Emissive_b10.tif",
#                  "1KM_Emissive_b11.tif",
#                  "1KM_Emissive_b12.tif",
#                  "1KM_Emissive_b13.tif",
#                  "1KM_Emissive_b14.tif",
#                  "1KM_Emissive_b15.tif",
#                  "1KM_RefSB_b0.tif",
#                  "1KM_RefSB_b1.tif",
#                  "1KM_RefSB_b2.tif",
#                  "1KM_RefSB_b3.tif",
#                  "1KM_RefSB_b4.tif",
#                  "1KM_RefSB_b5.tif",
#                  "1KM_RefSB_b6.tif",
#                  "1KM_RefSB_b7.tif",
#                  "1KM_RefSB_b8.tif",
#                  "1KM_RefSB_b9.tif",
#                  "1KM_RefSB_b10.tif",
#                  "1KM_RefSB_b11.tif",
#                  "1KM_RefSB_b12.tif",
#                  "1KM_RefSB_b13.tif",
#                  "1KM_RefSB_b14.tif",
#                  "250_RefSB_b0.tif",
#                  "250_RefSB_b1.tif",
#                  "500_RefSB_b0.tif",
#                  "500_RefSB_b1.tif",
#                  "500_RefSB_b2.tif",
#                  "500_RefSB_b3.tif",
#                  "500_RefSB_b4.tif")                   
# 
# suffixes.out <- c("B20.tif", 
#                   "B21.tif",
#                   "B22.tif",
#                   "B23.tif",
#                   "B24.tif",
#                   "B25.tif",
#                   "B27.tif",
#                   "B28.tif",
#                   "B29.tif",
#                   "B30.tif",
#                   "B31.tif",
#                   "B32.tif",
#                   "B33.tif",
#                   "B34.tif",
#                   "B35.tif",
#                   "B36.tif",
#                   "B08.tif",
#                   "B09.tif",
#                   "B10.tif",
#                   "B11.tif",
#                   "B12.tif",
#                   "B13.1.tif",
#                   "B13.2.tif",
#                   "B14.1.tif",
#                   "B14.2.tif",
#                   "B15.tif",
#                   "B16.tif",
#                   "B17.tif",
#                   "B18.tif",
#                   "B19.tif",
#                   "B26.tif",
#                   "B01.tif",
#                   "B02.tif",
#                   "B03.tif",
#                   "B04.tif",
#                   "B05.tif",
#                   "B06.tif",
#                   "B07.tif")
# 
# files = lst.tif
# suffix.in <- "1KM_Emissive_b11.tif"
# suffix.out <- "B36.tif"
################################################################################

renameSuffix <- function(files, 
                         suffix.in, 
                         suffix.out, 
                         tifsdir.rename) {
  index <- grep(suffix.in, files)
  
  file.in <- as.list(files[index])
  
  foreach (k = file.in) %do% {
    file.out <- paste(sapply(c(1, 2), function(i) {
      sapply(strsplit(basename(k), "_"), "[[", i)
    }), collapse = "_")
    
    file.out <- paste0(tifsdir.rename, file.out, "_", suffix.out)
#     file.out <- paste(tifsdir.rename, 
#                       paste(file.out, 
#                             suffix.out, 
#                             sep = "_"), 
#                       sep = "/")
    
    file.rename(k, file.out)
  }
  
  
  
}
