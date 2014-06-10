cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot NA values for each MODIS band, based on lvl0300 dataset.
##  
##  Version: 2014-03-16
##  
################################################################################
##
##  Copyright (C) 2014 Simon Schlauss (sschlauss@gmail.com)
##
##
##  This file is part of BiFoRe.
##  
##  BiFoRe is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  BiFoRe is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with BiFoRe.  If not, see <http://www.gnu.org/licenses/>.
##  
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "reshape2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/")
setwd("D:/Dropbox/Code/bifore/src/")

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
                       stringsAsFactors = TRUE)

################################################################################
### Subsetting #################################################################
################################################################################

df.greyval.all <- data.raw[179:216]
df.diff.all <- data.raw[217:254]
df.sd.all <- data.raw[255:292]

## Create df, count NA's for each MODIS band
df.NA <- cbind.data.frame(colSums(is.na(df.greyval.all)),
                          colSums(is.na(df.diff.all)),
                          colSums(is.na(df.sd.all)))

names(df.NA) <- c("greyvalues", "first_derivate", "standard_deviation")
df.NA$bandnames <- bandnames[,1]

df.NA.melt <- melt(df.NA, id.vars="bandnames")
names(df.NA.melt) <- c("MODIS_bands", "NA_values", "NA_count")


################################################################################
### Plotting ###################################################################
################################################################################

## Define output image | open image port
# png("images/lvl0300_na_values.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot <- ggplot(df.NA.melt, aes(x=MODIS_bands, y=NA_count, fill=NA_values)) + 
  geom_bar(position="dodge", stat="identity", width=1, colour="white") +
  scale_fill_grey(name = "NA values") +
#   coord_flip() +
  xlab("MODIS bands") +
  ylab("NA counts") +
  ggtitle("Summary of NA values for MODIS MYD02") +
  theme(axis.text.x=element_text(angle=90, hjust = 0, vjust = .5),
        plot.title = element_text(lineheight = .8, size = 20),
        legend.position=c(.9, .5))

plot

## Close image port
# graphics.off()