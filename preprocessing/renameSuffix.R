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

renameSuffix <- function(files, 
                         suffix.in, 
                         suffix.out, 
                         tifsdir.rename) {
  index <- grep(suffix.in, files)
  
  file.in <- files[index]
  
  file.out <- paste(sapply(c(1, 2), function(i) {
    sapply(strsplit(basename(file.in), "_"), "[[", i)
  }), collapse = "_")
  
  file.out <- paste(tifsdir.rename, 
                    paste(file.out, 
                          suffix.out, 
                          sep = "_"), 
                    sep = "/")
  
  file.rename(file.in, file.out)
}
