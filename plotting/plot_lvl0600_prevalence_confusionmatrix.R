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
lib <- c("ggplot2", "reshape2", "foreach")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/")
setwd("D:/Dropbox/Code/bifore/src/")

## Set filenames
file.in.confusion <- "csv/kili/lvl0600_rf_prevalence_species-cut_mean100_confusion.csv"

################################################################################
### Import dataset #############################################################
################################################################################

df.confusion <- read.csv2(file.in.confusion,
                          dec = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE)


################################################################################
### Subset / merge data ########################################################
################################################################################

## Keep order by no.of.prevalence in ggplot (x-axis)
df.confusion$species <- factor(df.confusion$species, 
                               levels=unique(df.confusion$species), 
                               ordered=TRUE)

## 15 most abundant species
# df.classError <- df.varimp.MDA[1:15,]

## recombine dataframes
df.classError <- cbind(df.confusion[1], df.confusion[7], df.confusion[8])
df.confusion.variables <- cbind(df.confusion[1], df.confusion[3:6])
df.confusion.sums <- cbind(df.confusion[1], df.confusion[9:12])
df.PODFAR <- cbind(df.confusion[1], df.confusion[2], df.confusion[13], df.confusion[14])
df.PODPOFD <- cbind(df.confusion[1], df.confusion[2], df.confusion[13], df.confusion[16])
df.statTest <- cbind(df.confusion[1], df.confusion[13:16])

## Modify df.classError species column (add sum M0 & sum M1)
df.classError$species <- foreach(i=seq(1:nrow(df.confusion)), .combine="rbind") %do% {
  species.tmp <- paste0("sumM0=", df.confusion[i,9], # 
                        " sumM1=", df.confusion[i,10],
                        " ", df.confusion[i,1])
  return(species.tmp)
}

## Keep order by no.of.prevalence in ggplot (x-axis)
df.classError$species <- factor(df.classError$species, 
                                levels=unique(df.classError$species), 
                                ordered=TRUE)

## melt dataframes
df.classError.melt <- melt(df.classError, id="species")
df.confusion.variables.melt <- melt(df.confusion.variables, id="species")
df.confusion.sums.melt <- melt(df.confusion.sums, id="species")
df.PODFAR.melt <- melt(df.PODFAR, id="species")
df.PODPOFD.melt <- melt(df.PODPOFD, id="species")
df.statTest.melt <- melt(df.statTest, id="species")


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
  ggtitle("Prevalence - confusion matrix - Classification error") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 11),
        plot.title = element_text(lineheight = .8, size = 20))

## Close image port
graphics.off()


################################################################################
### Plotting - confusion matrix variables ######################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_confusion_variables.png", 
    width = 1024 * 6, 
    height = 1024 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.confusion.variables.melt,
       aes(x=species, y=value, colour=variable, group=variable)) +
  geom_line() +
  xlab(NULL) +
  ylab("confusion matrix variables") +
  ggtitle("Prevalence - confusion matrix - confusion matrix variables") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 11),
        plot.title = element_text(lineheight = .8, size = 20))

## Close image port
graphics.off()


################################################################################
### Plotting - confusion matrix sums ###########################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_confusion_sums.png", 
    width = 1024 * 6, 
    height = 1024 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.confusion.sums.melt,
       aes(x=species, y=value, colour=variable, group=variable)) +
  geom_line() +
  xlab(NULL) +
  ylab("confusion matrix sums") +
  ggtitle("Prevalence - confusion matrix - confusion matrix sums") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 11),
        plot.title = element_text(lineheight = .8, size = 20))

## Close image port
graphics.off()


################################################################################
### Plotting - Statistical Tests ###############################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_confusion_statTests.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.statTest.melt,
       aes(x=species, y=value, colour=variable, group=variable)) +
  geom_line() +
  xlab(NULL) +
  ylab("value") +
  ylim(0,1) +
  ggtitle("Prevalence - confusion matrix - statistical testvalues") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 11),
        plot.title = element_text(lineheight = .8, size = 20))

## Close image port
graphics.off()

################################################################################
### Plotting - POD ~ FAR #######################################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_confusion_POD-FAR.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.PODFAR,
       aes(x=POD, y=FAR, size=2, colour=no.of.prevalence)) +
  geom_point() +
  ylim(0,1) +
  xlim(0,1) +
  #   scale_colour_grey(start = .7, end = 0) +   # !!! -> colour=factor(no.of.prevalence)
  scale_colour_gradient(limits=c(10, 40)) +
  ggtitle("Prevalence - confusion matrix - POD~FAR") +
  theme_bw() +
  guides(size=FALSE) +
  theme(plot.title = element_text(lineheight = .8, size = 20))

## Close image port
graphics.off()


################################################################################
### Plotting - POD ~ POFD ######################################################
################################################################################

## Define output image | open image port
png("images/lvl0600_prevalence_confusion_POD-POFD.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

ggplot(data=df.PODPOFD,
       aes(x=POD, y=POFD, size=2, colour=no.of.prevalence)) +
  geom_point() +
  ylim(0,1) +
  xlim(0,1) +
  #   scale_colour_grey(start = .7, end = 0) +   # !!! -> colour=factor(no.of.prevalence)
  scale_colour_gradient(limits=c(10, 40)) +
  ggtitle("Prevalence - confusion matrix - POD~POFD") +
  theme_bw() +
  guides(size=FALSE) +
  theme(plot.title = element_text(lineheight = .8, size = 20))

## Close image port
graphics.off()


