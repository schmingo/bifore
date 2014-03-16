################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT PREVALENCE                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-16                                                        ##
##                                                                            ##
################################################################################


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

data.raw.all <- read.csv2("csv/kili/lvl0400_prevalence_all_species.csv",
                          dec = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE)

data.raw.10 <- read.csv2("csv/kili/lvl0400_prevalence_data_10.csv",
                         dec = ",",
                         header = TRUE,
                         stringsAsFactors = FALSE)


################################################################################
### Plotting - prevalence - all species ########################################
################################################################################

## Define output image | open image port
# png("images/lvl0400_prevalence_all_spec.png", 
#     width = 2048 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot <- ggplot(data.raw.all, aes(x=species, y=prevalence)) + 
  geom_bar(stat="identity") +
  scale_fill_grey() +
  xlab("species") +
  ylab("prevalence") +
  ggtitle("Orthoptera prevalence Mt. Kilimanjaro 2002-2012") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20))

plot

## Close image port
# graphics.off()


################################################################################
### Plotting - prevalence - species with more than 10 observations in different plots
################################################################################

## Define output image | open image port
# png("images/lvl0400_prevalence_10.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot <- ggplot(data.raw.10, aes(x=species, y=prevalence)) + 
  geom_bar(stat="identity") +
  scale_fill_grey() +
  xlab("species") +
  ylab("prevalence") +
  ggtitle("Orthoptera prevalence Mt. Kilimanjaro 2002-2012 \n Species with more than 10 observations in different plots") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(lineheight = .8, size = 20))

plot

## Close image port
# graphics.off()
