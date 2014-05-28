cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PRINCIPAL COMPONENT ANALYSIS                                               ##
## MEAN DECREASE GINI & MEAN DECREASE ACCURACY                                ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-28                                                        ##
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
file.in.varimp.MDA <- "csv/kili/lvl0600_rf_prevalence_species-cut_mean100_MDA.csv"
file.in.varimp.MDG <- "csv/kili/lvl0600_rf_prevalence_species-cut_mean100_MDG.csv"

################################################################################
### Import dataset #############################################################
################################################################################

df.varimp.MDA <- read.csv2(file.in.varimp.MDA,
                           dec = ",",
                           header = TRUE,
                           stringsAsFactors = FALSE)

df.varimp.MDG <- read.csv2(file.in.varimp.MDG,
                           dec = ",",
                           header = TRUE,
                           stringsAsFactors = FALSE)


################################################################################
### Prepare MDA dataset for Principal Component Analysis #######################
################################################################################

## Keep ordering by no.of.species
df.varimp.MDA$species <- factor(df.varimp.MDA$species, 
                                levels=unique(df.varimp.MDA$species), 
                                ordered=TRUE)

## Keep ordering by no.of.species
df.varimp.MDG$species <- factor(df.varimp.MDG$species, 
                                levels=unique(df.varimp.MDG$species), 
                                ordered=TRUE)


## select commonest species
df.varimp.MDA <- df.varimp.MDA[1:10,]
df.varimp.MDG <- df.varimp.MDG[1:10,]

## remove no.of.prevalence
df.varimp.MDA <- cbind(df.varimp.MDA[1],df.varimp.MDA[3:ncol(df.varimp.MDA)])
df.varimp.MDG <- cbind(df.varimp.MDG[1],df.varimp.MDG[3:ncol(df.varimp.MDG)])


## Transpose df
df.pca.MDA <- data.frame(t(df.varimp.MDA), stringsAsFactors=FALSE)
df.pca.MDG <- data.frame(t(df.varimp.MDG), stringsAsFactors=FALSE)

## Set new colnames
names(df.pca.MDA) <- as.character(df.varimp.MDA[,1])
names(df.pca.MDG) <- as.character(df.varimp.MDG[,1])

## Remove first data row (= redundant)
df.pca.MDA <- df.pca.MDA[-1,]
df.pca.MDG <- df.pca.MDG[-1,]

## Converts factors to numeric values
df.pca.MDA <- data.matrix(df.pca.MDA)
df.pca.MDG <- data.matrix(df.pca.MDG)


## Reduce row.names (bandnames)
row.names(df.pca.MDA) <- sapply(strsplit(as.character(row.names(df.pca.MDA)), "d"), "[[", 2)
row.names(df.pca.MDG) <- sapply(strsplit(as.character(row.names(df.pca.MDG)), "d"), "[[", 2)


################################################################################
### Plotting - PCA - Mean Decrease Accuracy ####################################
################################################################################

## Select species for PCA
# df.pca.MDA <- df.pca.MDA[,1:10]

summary(df.pca.MDA)

pca.MDA <- princomp(df.pca.MDA, scores = TRUE)


plot(pca.MDA)
summary(pca.MDA)
loadings(pca.MDA)

## Define output image | open image port
png("images/lvl0600_PCA_MDA.png", 
    width = 1024 * 6, 
    height = 1024 * 6, 
    units = "px", 
    res = 600)

biplot(pca.MDA, main = "PCA - Mean Decrease Accuracy")
# ?biplot.princomp

## Close image port
graphics.off()

################################################################################
### Plotting - PCA - Mean Decrease Gini ########################################
################################################################################

## Select species for PCA
# df.pca.MDG <- df.pca.MDG[,1:10]

summary(df.pca.MDG)

pca.MDG <- princomp(df.pca.MDG, scores = TRUE)


plot(pca.MDG)
summary(pca.MDG)
loadings(pca.MDG)

## Define output image | open image port
png("images/lvl0600_PCA_MDG.png", 
    width = 1024 * 6, 
    height = 1024 * 6, 
    units = "px", 
    res = 600)

biplot(pca.MDG, main = "PCA - Mean Decrease Gini")
# ?biplot.princomp

## Close image port
graphics.off()
