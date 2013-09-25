################################################################################
## BiFoRe Scripts
##
## CHECK MODIS GREYVALUES FOR UNUSEABLE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-08-12
##
################################################################################

'''
REFERING TO: Level 1B Product Data Dictionary V6.1.14
http://mcst.gsfc.nasa.gov/content/l1b-documents
http://mcst.gsfc.nasa.gov/sites/mcst.gsfc/files/file_attachments/M1055_PDD_D_072712final.pdf
(P.80f)
...
The valid range is [0-32767], inclusive.  Any value above 32767 represents 
unusable data.  Table 2.2.3 shows the meaning of data values over 32767.
Table 2.2.3:  Meaning of Data Values Outside of Valid Range

Value  Meaning
--------------------------------------------------------------------------------
65535	 Fill Value (includes reflective band data at night mode and completely missing L1A scans)
65534	 L1A DN is missing within a scan
65533	 Detector is saturated
65532	 Cannot compute  zero point DN
65531	 Detector is dead (see comments below)
65530	 RSB dn** below the minimum of the scaling range
65529	 TEB radiance or RSB dn** exceeds the maximum of the scaling range
65528	 Aggregation algorithm failure
65527	 Rotation of Earth view Sector from nominal science collection position
65526	 Calibration coefficient b1 could not be computed
65501 - 65525	(reserved for future use)
65500	 NAD closed upper limit


'''



## Clear workspace
rm(list = ls(all = TRUE))

# ## Required libraries
# lib <- c()
# lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
#setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("/home/schmingo/Google Drive/bifore/") # Windows
#setwd("Florian")


### Import dataset with extracted MODIS greyvalues
data <- read.csv2("src/csv/all_greyvalues_modis.csv", dec = ".",
                 header = TRUE, stringsAsFactors = FALSE)

## Print unuseable data
print(ncol(data))
summary(data[,7:ncol(data)])


### Replace values with NA

## v1
data[, 7:ncol(data)][data[, 7:ncol(data)] > 32767] <- NA

## v2
# data.sub <- data[,7:ncol(data)]
# data.sub[data.sub > 32767] <- NA
# data[,7:ncol(data)] <- data.sub

## write new table with NA values
write.table(data, file = "src/csv/all_greyvalues_modis_NA.csv", dec = ".", quote = FALSE, 
            col.names = TRUE, row.names = FALSE, sep =";")


### add random abundance data
data.abundance <- cbind(data, abundance=sample(1:20, nrow(data), replace = TRUE))

## Write new table with NA & random abundance data
write.table(data.abundance, file = "src/csv/all_greyvalues_modis_NA_abundance.csv", dec = ".", quote = FALSE, 
            col.names = TRUE, row.names = FALSE, sep =";")