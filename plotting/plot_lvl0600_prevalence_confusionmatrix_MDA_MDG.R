################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT PREVALENCE LVL0600 CONFUSION MATRIX, MEAN DECREASE ACCURACY AND       ##
## MEAN DECREASE GINI                                                         ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-28                                                        ##
##                                                                            ##
################################################################################
cat("\014")

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Import dataset #############################################################
################################################################################

data.raw.all <- read.csv2("csv/kili/lvl0600_rf_prevalence_species10_mean100.csv",
                          dec = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE)


################################################################################
### Plotting - prevalence - all species ########################################
################################################################################

## Define output image | open image port
png("images/lvl0400_prevalence_all_species.png", 
    width = 2048 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

plot <- ggplot(data.raw.all, aes(x=species, y=number.of.species)) + 
  geom_bar(stat="identity") +
  scale_fill_grey() +
  xlab("species") +
  ylab("prevalence") +
  ggtitle("Orthoptera prevalence Mt. Kilimanjaro 2002-2012") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20))

plot

## Close image port
graphics.off()
