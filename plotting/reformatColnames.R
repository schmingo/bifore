# Function to reformat colnames of input data
reformatColnames <- function(x) {
  # Edit band names
  tmp.names <- names(x)[2:(ncol(x)-1)]
  tmp.bands <- as.numeric(sapply(strsplit(tmp.names, "B"), "[[", 2))
  tmp.bands <- formatC(tmp.bands, width = 2, format = "d", flag = "0")
  
  names(x)[2:(ncol(x)-1)] <- paste("B", tmp.bands, sep = "")
  
  # Rearrange columns in ascending order
  x <- x[, c(1, order(names(x)[2:ncol(x)]) + 1)]
  
  # Return data.frame
  return(x)
}
