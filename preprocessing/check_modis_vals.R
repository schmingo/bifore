################################################################################
## BiFoRe Scripts
##
## CHECK MODIS GREYVALUES FOR UNUSEABLE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-08-07
##
################################################################################

'''
REFERING TO: https://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=8&ved=0CHoQFjAH&url=ftp%3A%2F%2Fmcst.hbsss-sigma.com%2Fpub%2Fpermanent%2FMCST%2FL1B_docs%2FData_Dictionaries%2FL1B_V4.0.9_V4.1.1_Data_Dcny.doc&ei=kWQCUprgBM3NswaP3YCAAw&usg=AFQjCNGLskc85bT9K92d7tpSmLHdeZ13TQ&sig2=WGQhQZTUNG0X-bx7m68N9A&bvm=bv.50310824,d.Yms
(S.83f)
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
setwd("D:/Diplomarbeit/") # Windows
#setwd("Flo")


### Import dataset with extracted MODIS greyvalues
data <- read.csv2("src/csv/all_greyvalues_modis.csv", dec = ".",
                 header = TRUE, stringsAsFactors = FALSE)

### Print unuseable data

print(ncol(data))
summary(data[,7:ncol(data)])

'''
Funktionsbeschreibung:
1. Funktion
Der data frame soll nach allen Werten >32767 durchsucht werden und zu jedem
Fund Reihenname und Spaltenname ausgeben.

2. Funktion
Alle Werte im data frame >32767 sollen NA gesetzt werden
'''

### replace values with na

## v1
# data.sub <- data[,7:ncol(data)]
# data.sub[data.sub > 32767] <- NA
# data[,7:ncol(data)] <- data.sub

## v2
data[, 7:ncol(data)][data[, 7:ncol(data)] > 32767] <- NA


### write new table
write.table(data, file = "src/csv/all_greyvalues_modis_NA.csv", dec = ".", quote = FALSE, 
            col.names = TRUE, row.names = FALSE, sep =";")