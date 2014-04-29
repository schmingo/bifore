cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT PREVALENCE LVL0600 CONFUSION MATRIX, MEAN DECREASE ACCURACY AND       ##
## MEAN DECREASE GINI                                                         ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-29                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "reshape2")
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
                      stringsAsFactors = FALSE)


################################################################################
### Subset / merge data ########################################################
################################################################################

df.classError <- cbind(data.raw[1], data.raw[5], data.raw[8])
df.varimp.MDA <- cbind(data.raw[1],data.raw[9:38])
df.varimp.MDG <- cbind(data.raw[1],data.raw[39:68])


df.classError.melt <- melt(df.classError, id="species")
df.varimp.MDA.melt <- melt(df.varimp.MDA, id="species")
df.varimp.MDG.melt <- melt(df.varimp.MDG, id="species")

  
################################################################################
### Plotting - Classification Error ############################################
################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_confusion_classError.png", 
#     width = 2048 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

ggplot(data=df.classError.melt,
       aes(x=species, y=value, colour=variable, group=variable)) +
  geom_line() +
  xlab(NULL) +
  ylab("Classification Error") +
  ggtitle("RandomForest prevalence - confusion matrix") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20))

## Close image port
# graphics.off()


################################################################################
### Plotting - Mean Decrease Accuracy ##########################################
################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_MeanDecreaseAccuracy.png", 
#     width = 2048 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

ggplot(data=df.varimp.MDA.melt,
       aes(x=species, y=value, colour=variable, group=variable)) +
  geom_line() +
  xlab(NULL) +
  ylab("Mean Decrease Accuracy") +
  ggtitle("RandomForest prevalence - Mean Decrease Accuracy") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20))

## Close image port
# graphics.off()
















# test_data <- data.frame(
#   var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
#   var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
#   date = seq.Date(as.Date("2002-01-01"), by="1 month", length.out=100))
# 
# test_data_long <- melt(test_data, id="date")  # convert to long format
# 
# ggplot(data=test_data_long,
#        aes(x=date, y=value, colour=variable)) +
#   geom_line()


