################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CREATE BANDNAMES DATAFRAME FOR PLOTTING ETC.                               ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-03                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Set filepath
path.bandnames.csv <- "csv/kili/bandnames.csv"


################################################################################
### Create bandname sequences ##################################################
################################################################################

################################################################################
## "band_"

bands <- c()
for(i in seq(1:9)) bands[i] <- paste0("band_0", i)
for(i in seq(10:12)) bands[i+9] <- paste0("band_", i+9)
bands[13:16] <- c("band_13.1",
                  "band_13.2",
                  "band_14.1",
                  "band_14.2")
for(i in seq(15:36)) bands[i+16] <- paste0("band_", i+14)


################################################################################
## "greyvalue_band_"

greyval_bands <- c()
for(i in seq(1:9)) greyval_bands[i] <- paste0("greyvalue_band_0", i)
for(i in seq(10:12)) greyval_bands[i+9] <- paste0("greyvalue_band_", i+9)
greyval_bands[13:16] <- c("greyvalue_band_13.1",
                          "greyvalue_band_13.2",
                          "greyvalue_band_14.1",
                          "greyvalue_band_14.2")
for(i in seq(15:36)) greyval_bands[i+16] <- paste0("greyvalue_band_", i+14)


################################################################################
## "derivate_band_"

diff_bands <- c()
for(i in seq(1:9)) diff_bands[i] <- paste0("derivate_band_", i)
for(i in seq(10:12)) diff_bands[i+9] <- paste0("derivate_band_", i+9)
diff_bands[13:16] <- c("derivate_band_13.1",
                       "derivate_band_13.2",
                       "derivate_band_14.1",
                       "derivate_band_14.2")
for(i in seq(15:36)) diff_bands[i+16] <- paste0("derivate_band_", i+14)


################################################################################
## "sd_band_"

sd_bands <- c()
for(i in seq(1:9)) sd_bands[i] <- paste0("sd_band_", i)
for(i in seq(10:12)) sd_bands[i+9] <- paste0("sd_band_", i+9)
sd_bands[13:16] <- c("sd_band_13.1",
                     "sd_band_13.2",
                     "sd_band_14.1",
                     "sd_band_14.2")
for(i in seq(15:36)) sd_bands[i+16] <- paste0("sd_band_", i+14)


################################################################################
### Combine bandname sequences in a dataframe ##################################
################################################################################

df.bandnames <- cbind.data.frame(bands, greyval_bands, diff_bands, sd_bands)


################################################################################
### Write .csv #################################################################
################################################################################

write.table(df.bandnames,
            file = path.bandnames.csv,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
