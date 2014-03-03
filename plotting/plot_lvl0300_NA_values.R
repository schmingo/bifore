################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT EXTRACT GREYVALUES, FIRST DERIVATE & CALCULATED SD                    ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-03                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("ggplot2", "reshape2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Set filepath
path.lvl0300.csv <- "csv/kili/lvl0300_biodiversity_data.csv"
path.bandnames.csv <- "csv/kili/bandnames.csv"


## Import dataset
data.raw <- read.csv2(path.lvl0300.csv,
                      dec = ",",
                      header = TRUE, 
                      stringsAsFactors = FALSE)

bandnames <- read.csv2(path.bandnames.csv,
                       header = TRUE,
                       stringsAsFactors = FALSE)

################################################################################
### Subsetting #################################################################
################################################################################

df.greyval.all <- data.raw[69:106]
df.diff.all <- data.raw[107:144]
df.sd.all <- data.raw[145:182]

## Create df, count NA's for each MODIS band
df.NA <- cbind(colSums(is.na(df.greyval.all)),
               colSums(is.na(df.diff.all)),
               colSums(is.na(df.sd.all)))

names(df.NA) <- c("NA_greyvalues", "NA_diff", "NA_sd")


################################################################################
### Plotting ###################################################################
################################################################################

## Plot single subset (greyvalues, diff or sd)
qplot(y=df.NA[,1], 
      x=row.names(df.NA), 
      geom = "bar", 
      binwidth = 2, 
      stat="identity")


# ## Using barplot()
# barplot(height = df.NA,
#         width = length(row.names(df.NA)), 
#         beside = TRUE,
#         ylab = "count NA",
#         xlab = "MODIS bands")


