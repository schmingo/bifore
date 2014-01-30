renameSuffix <- function(files, 
                         suffix.in, 
                         suffix.out, 
                         dir.out = ".",
                         ...) {
  
  index <- grep(suffix.in, files)
  
  file.in <- files[index]
  
  file.out <- paste(sapply(c(1, 2), function(i) {
    sapply(strsplit(basename(file.in), "_"), "[[", i)
  }), collapse = "_")
  
  file.out <- paste(dir.out, paste(file.out, suffix.out, sep = "_"), sep = "/")
  
  file.rename(file.in, file.out)
}