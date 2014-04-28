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

data.raw <- read.csv2("csv/kili/lvl0600_rf_prevalence_species10_mean100.csv",
                      dec = ",",
                      header = TRUE,
                      row.names = 1,
                      stringsAsFactors = FALSE)

################################################################################
### Plotting - confusion matrix ################################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_confusionmatrix.png", 
    width = 2048 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

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
graphics.off()
