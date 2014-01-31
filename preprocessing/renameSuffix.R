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
    file.out <- paste(sapply(c(1, 2), function(i) {
      sapply(strsplit(basename(k), "_"), "[[", i)
    }), collapse = "_")
    
    file.out <- paste0(tifsdir, file.out, "_", suffix.out)
    
    file.rename(k, file.out)
  }
  
  
  
}
