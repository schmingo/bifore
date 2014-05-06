cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT PREVALENCE LVL0600 CONFUSION MATRIX                                   ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-06                                                        ##
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

## Set filenames
file.in.confusion <- "csv/kili/lvl0600_rf_prevalence_species-cut_mean100_confusion.csv"

################################################################################
### Import dataset #############################################################
################################################################################

df.confusion <- read.csv2(file.in.confusion,
                          dec = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE)


## Add sum M0 & sum M1 to species column
df.confusion$species <- foreach(i=seq(1:nrow(df.confusion)), .combine="rbind") %do% {
  species.tmp <- paste0("sumM0=", df.confusion[i,9], 
                        " sumM1=", df.confusion[i,10],
                        " ", df.confusion[i,1])
  return(species.tmp)
}

################################################################################
### Subset / merge data ########################################################
################################################################################

## Keep order by no.of.species in ggplot (x-axis)
df.confusion$species <- factor(df.confusion$species, 
                               levels=unique(df.confusion$species), 
                               ordered=TRUE)

## 15 most abundant species
# df.classError <- df.varimp.MDA[1:15,]

## recombine dataframes
df.classError <- cbind(df.confusion[1], df.confusion[7], df.confusion[8])
df.PODFAR <- cbind(df.confusion[1], df.confusion[2], df.confusion[13], df.confusion[14])

## melt dataframes
df.classError.melt <- melt(df.classError, id="species")
df.PODFAR.melt <- melt(df.PODFAR, id="species")


################################################################################
### Plotting - Classification Error ############################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_confusion_classError.png", 
    width = 1024 * 6, 
    height = 1024 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.classError.melt,
       aes(x=species, y=value, colour=variable, group=variable)) +
  geom_line() +
  xlab(NULL) +
  ylab("Classification Error") +
  ggtitle("RandomForest prevalence - confusion matrix - Classification error") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 11),
        plot.title = element_text(lineheight = .8, size = 20))

## Close image port
graphics.off()


################################################################################
### Plotting - POD ~ FAR #######################################################
################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_confusion_POD-FAR.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

ggplot(data=df.PODFAR,
       aes(x=POD, y=FAR, size=2, colour=no.of.species)) +
  geom_point() +
#   scale_colour_grey(start = .7, end = 0) +   # !!! -> colour=factor(no.of.species)
  scale_colour_gradient(limits=c(10, 40)) +
  ggtitle("RandomForest prevalence - confusion matrix - POD~FAR") +
  theme_bw() +
  guides(size=FALSE) +
  theme(plot.title = element_text(lineheight = .8, size = 20))

## Close image port
# graphics.off()
