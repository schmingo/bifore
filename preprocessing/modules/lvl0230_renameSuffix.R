################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RENAME MODIS FILES                                                         ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-31                                                        ##
##                                                                            ##
################################################################################

renameSuffix <- function(files, 
                         suffix.in, 
                         suffix.out, 
                         tifsdir) {
  
  index <- grep(suffix.in, files)
  
  file.in <- as.list(files[index])
  
  foreach (k = file.in) %do% {
    file.out <- substr(basename(k), 1, 22)
    file.out <- paste0(tifsdir, suffix.out, "_",file.out , ".tif")
    
    file.rename(k, file.out)
  }
}
