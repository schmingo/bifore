################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CHECK MODIS GREYVALUES FOR OUT-OF-RANGE DATA                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-01                                                        ##
##                                                                            ##
################################################################################

# REFERING TO: Level 1B Product Data Dictionary V6.1.14
# http://mcst.gsfc.nasa.gov/content/l1b-documents
# http://mcst.gsfc.nasa.gov/sites/mcst.gsfc/files/file_attachments/M1055_PDD_D_072712final.pdf
# (P.80f)
# ...
# The valid range is [0-32767], inclusive.  Any value above 32767 represents 
# unusable data.  Table 2.2.3 shows the meaning of data values over 32767.
# Table 2.2.3:  Meaning of Data Values Outside of Valid Range
# 
# Value  Meaning
# --------------------------------------------------------------------------------
# 65535	 Fill Value (includes reflective band data at night mode and completely missing L1A scans)
# 65534	 L1A DN is missing within a scan
# 65533	 Detector is saturated
# 65532	 Cannot compute  zero point DN
# 65531	 Detector is dead (see comments below)
# 65530	 RSB dn** below the minimum of the scaling range
# 65529	 TEB radiance or RSB dn** exceeds the maximum of the scaling range
# 65528	 Aggregation algorithm failure
# 65527	 Rotation of Earth view Sector from nominal science collection position
# 65526	 Calibration coefficient b1 could not be computed
# 65501 - 65525	(reserved for future use)
# 65500	 NAD closed upper limit

################################################################################
## Clear workspace
rm(list = ls(all = TRUE))

# ## Required libraries
# lib <- c()
# lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/") # Windows

################################################################################
### Import dataset with extracted MODIS greyvalues #############################
data <- read.csv2("src/csv/all_MODIS_20130707-1120_RAW.csv", 
                  dec = ".", header = TRUE, stringsAsFactors = FALSE)

data.raw <- data


## Print unuseable data
print(ncol(data))
summary(data[,7:ncol(data)])

################################################################################
### Create error message vectors and dataframe  ################################

error.value <- c(65500:65535)
error.message <- c("NAD closed upper limit",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "(reserved for future use)",
                   "Calibration coefficient b1 could not be computed",
                   "Rotation of Earth view Sector from nominal science collection position",
                   "Aggregation algorithm failure",
                   "TEB radiance or RSB dn** exceeds the maximum of the scaling range",
                   "RSB dn** below the minimum of the scaling range",
                   "Detector is dead",
                   "Cannot compute  zero point DN",
                   "Detector is saturated",
                   "L1A DN is missing within a scan",
                   "FILL VALUE"
                   )
df.error <- data.frame(cbind(error.value, error.message))

error.list <- as.list(error.value)

################################################################################
### replace out-of-range data with actual meaning ##############################

data[, 7:ncol(data)][data[, 7:ncol(data)] == 65535] <- "FILL VALUE"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65534] <- "L1A DN is missing within a scan"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65533] <- "Detector is saturated"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65532] <- "Cannot compute  zero point DN"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65531] <- "Detector is dead"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65530] <- "RSB dn** below the minimum of the scaling range"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65529] <- "TEB radiance or RSB dn** exceeds the maximum of the scaling range"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65528] <- "Aggregation algorithm failure"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65527] <- "Rotation of Earth view Sector from nominal science collection position"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65526] <- "Calibration coefficient b1 could not be computed"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65501:65525] <- "(reserved for future use)"
data[, 7:ncol(data)][data[, 7:ncol(data)] == 65500] <- "NAD closed upper limit"


################################################################################
### Error message function #####################################################

error.statistics <- function(x) {
  ## count specific error value
  num.error <- sum(data.raw[, 7:ncol(data.raw)] == x)

  ## count number of all available greyvalues
  num.greyvals <- ncol(data.raw[,7:ncol(data.raw)])*nrow(data.raw)

  ## percental calculation
  error.percent <- (100/num.greyvals)*num.error

  ## output message
  paste("Error ", x, ": ", "count: ", num.error, "  = ", 
        error.percent, "%",  sep="")
}

## Call function for single error value
error.statistics(65533)

## Call function for all error values
lapply(error.list, error.statistics)


################################################################################
### Write out-of-range table ###################################################

write.table(data, 
            file = "src/csv/all_MODIS_20130707-1120_OOR-Error.csv", 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")