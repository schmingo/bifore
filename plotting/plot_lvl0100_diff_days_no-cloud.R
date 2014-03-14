################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT DIFF DAYS NO-CLOUD                                                    ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-10                                                        ##
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

data.raw <- read.csv2("csv/kili/lvl0100_biodiversity_data_all_spec.csv",
                      dec = ".",
                      header = TRUE,
                      stringsAsFactors = FALSE)




################################################################################
### Subsetting #################################################################
################################################################################

data <- as.data.frame(data.raw[,4])
names(data) <- "diff_days_no_cloud"


################################################################################
### Plotting ###################################################################
################################################################################

## Define output image | open image port
# png("images/lvl0100_diff_days_no-cloud_all_spec.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

hist <- ggplot(data, aes(x = diff_days_no_cloud)) +
  geom_histogram(binwidth = .5) +
  xlab("days") +
#   ylab("count") +
  ggtitle("Gap between observations and cloud-free days") +
  theme(plot.title = element_text(lineheight=.8, size = 20))

hist


## Close image port
# graphics.off()