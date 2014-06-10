cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot extracted greyvalues and first derivate, based on lvl0300 dataset.
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
                       stringsAsFactors = FALSE)

################################################################################
### Subsetting greyvalues ######################################################
################################################################################

## greyvalues reflective bands 
data.greyval.reflective <- cbind.data.frame(data.raw[,3],
                                            data.raw[,179:199],
                                            data.raw[,206])
names(data.greyval.reflective) <- c("date_nocloud",
                                    as.character(bandnames[1:21,3]),
                                    as.character(bandnames[28,3]))
names(data.greyval.reflective)

greyval.reflective.melt <- melt(data.greyval.reflective, id = "date_nocloud", 
                                measured = c(data.greyval.reflective[,2:ncol(data.greyval.reflective)]))


## greyvalues emissive bands
data.greyval.emissive <- cbind.data.frame(data.raw[,3],
                                          data.raw[,200:205],
                                          data.raw[,207:216])
names(data.greyval.emissive) <- c("date_nocloud",
                                  as.character(bandnames[22:27,3]),
                                  as.character(bandnames[29:38,3]))
names(data.greyval.emissive)

greyval.emissive.melt <- melt(data.greyval.emissive, id = "date_nocloud", 
                              measured = c(data.greyval.emissive[,2:ncol(data.greyval.emissive)]))



################################################################################
### Plotting greyvalues ########################################################
################################################################################

## Define output image | open image port
# png("images/lvl0300_boxplot_greyvalues_reflective.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot.greyval.reflective <- ggplot(greyval.reflective.melt, aes(x = variable, y = value)) +
  geom_boxplot() +
  xlab("MYD02 reflective bands") +
  ylab("value") +
  ggtitle("MYD02 greyvalues reflective bands") +
  theme(plot.title = element_text(lineheight=.8, size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
plot.greyval.reflective 

# graphics.off()


## Define output image | open image port
# png("images/lvl0300_boxplot_greyvalues_emissive.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot.greyval.emissive <- ggplot(greyval.emissive.melt, aes(x = variable, y = value)) +
  geom_boxplot() +
  xlab("MYD02 emissive bands") +
  ylab("value") +
  ggtitle("MYD02 greyvalues emissive bands") +
  theme(plot.title = element_text(lineheight=.8, size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
plot.greyval.emissive

# graphics.off()


################################################################################
################################################################################
################################################################################
################################################################################
### Subsetting first derivate ##################################################
################################################################################

## greyvalues reflective bands 
data.deriv.reflective <- cbind.data.frame(data.raw[,3],
                                            data.raw[,217:237],
                                            data.raw[,244])
names(data.deriv.reflective) <- c("date_nocloud",
                                    as.character(bandnames[1:21,4]),
                                    as.character(bandnames[28,4]))
names(data.deriv.reflective)

deriv.reflective.melt <- melt(data.deriv.reflective, id = "date_nocloud", 
                                measured = c(data.deriv.reflective[,2:ncol(data.deriv.reflective)]))


## greyvalues emissive bands
data.deriv.emissive <- cbind.data.frame(data.raw[,3],
                                          data.raw[,238:243],
                                          data.raw[,245:254])
names(data.deriv.emissive) <- c("date_nocloud",
                                  as.character(bandnames[22:27,4]),
                                  as.character(bandnames[29:38,4]))
names(data.deriv.emissive)

deriv.emissive.melt <- melt(data.deriv.emissive, id = "date_nocloud", 
                              measured = c(data.deriv.emissive[,2:ncol(data.deriv.emissive)]))


################################################################################
### Plotting greyvalues ########################################################
################################################################################

## Define output image | open image port
# png("images/lvl0300_boxplot_derivate_reflective.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot.deriv.reflective <- ggplot(deriv.reflective.melt, aes(x = variable, y = value)) +
  geom_boxplot() +
  xlab("MYD02 reflective bands") +
  ylab("value") +
  ggtitle("MYD02 greyvalues reflective bands - first derivate") +
  theme(plot.title = element_text(lineheight=.8, size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
plot.deriv.reflective 

# graphics.off()

## Define output image | open image port
# png("images/lvl0300_boxplot_derivate_emissive.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot.deriv.emissive <- ggplot(deriv.emissive.melt, aes(x = variable, y = value)) +
  geom_boxplot() +
  xlab("MYD02 emissive bands") +
  ylab("value") +
  ggtitle("MYD02 greyvalues emissive bands - first derivate") +
  theme(plot.title = element_text(lineheight=.8, size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
plot.deriv.emissive

# graphics.off()
