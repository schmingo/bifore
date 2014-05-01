cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT PREVALENCE LVL0600 CONFUSION MATRIX, MEAN DECREASE ACCURACY AND       ##
## MEAN DECREASE GINI                                                         ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-01                                                        ##
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

## Keep order by no.of.species in ggplot (x-axis)
data.raw$species <- factor(data.raw$species, 
                           levels=unique(data.raw$species), 
                           ordered=TRUE)



## recombine dataframes
df.classError <- cbind(data.raw[1], data.raw[5], data.raw[8])

## 20 commonest species
data.raw <- data.raw[1:15,]
df.varimp.MDA.reflect <- cbind(data.raw[1],data.raw[9:21])
df.varimp.MDA.emit <- cbind(data.raw[1],data.raw[22:38])
df.varimp.MDG.reflect <- cbind(data.raw[1],data.raw[39:51])
df.varimp.MDG.emit <- cbind(data.raw[1],data.raw[52:68])


## melt dataframes
df.classError.melt <- melt(df.classError, id="species")
df.varimp.MDA.reflect.melt <- melt(df.varimp.MDA.reflect, id="species")
df.varimp.MDA.emit.melt <- melt(df.varimp.MDA.emit, id="species")
df.varimp.MDG.reflect.melt <- melt(df.varimp.MDG.reflect, id="species")
df.varimp.MDG.emit.melt <- melt(df.varimp.MDG.emit, id="species")

  
################################################################################
### Plotting - Classification Error ############################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_confusion_classError.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.classError.melt,
       aes(x=species, y=value, colour=variable, group=variable)) +
  geom_line() +
  xlab(NULL) +
  ylab("Classification Error") +
  ggtitle("RandomForest prevalence - confusion matrix") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 12),
        plot.title = element_text(lineheight = .8, size = 20))

## Close image port
graphics.off()


################################################################################
### Plotting - Mean Decrease Accuracy ##########################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_MeanDecreaseAccuracy_reflective.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.varimp.MDA.reflect.melt,
       aes(x=species, y=value, group=variable)) +
  geom_line() +
  ylim(-2,15) +
  xlab(NULL) +
  ylab("Mean Decrease Accuracy") +
  theme_bw() +
  ggtitle("Prevalence - RandomForest - Mean Decrease Accuracy (reflective bands)") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20)) +
  facet_wrap(~variable, as.table=FALSE, ncol = 4)

## Close image port
graphics.off()

################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_MeanDecreaseAccuracy_emissive.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.varimp.MDA.emit.melt,
       aes(x=species, y=value, group=variable)) +
  geom_line() +
  ylim(-2,15) +
  xlab(NULL) +
  ylab("Mean Decrease Accuracy") +
  theme_bw() +
  ggtitle("Prevalence - RandomForest - Mean Decrease Accuracy (emissive bands)") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20)) +
  facet_wrap(~variable, as.table=FALSE, ncol = 4)



## Close image port
graphics.off()

