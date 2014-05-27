cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PRINCIPAL COMPONENT ANALYSIS                                               ##
## MEAN DECREASE GINI & MEAN DECREASE ACCURACY                                ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-27                                                        ##
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
### Subset / merge data ########################################################
################################################################################

## Keep order by no.of.species in ggplot (x-axis)
df.varimp.MDA$species <- factor(df.varimp.MDA$species, 
                                levels=unique(df.varimp.MDA$species), 
                                ordered=TRUE)

## Keep order by no.of.species in ggplot (x-axis)
df.varimp.MDG$species <- factor(df.varimp.MDG$species, 
                                levels=unique(df.varimp.MDG$species), 
                                ordered=TRUE)



## recombine dataframes

## 15 commonest species
df.varimp.MDA <- df.varimp.MDA[1:15,]
df.varimp.MDG <- df.varimp.MDG[1:15,]

df.varimp.MDA <- cbind(df.varimp.MDA[1],df.varimp.MDA[3:ncol(df.varimp.MDA)])
df.varimp.MDG <- cbind(df.varimp.MDG[1],df.varimp.MDG[3:ncol(df.varimp.MDG)])

################################################################################
### Plotting - Mean Decrease Accuracy - Principal Components Analysis ##########
################################################################################
# df.pca.MDA <- data.frame(t(df.varimp.MDA))

## Transpose df
df.pca.MDA <- data.frame(t(df.varimp.MDA), stringsAsFactors=FALSE)

## Set new colnames
names(df.pca.MDA) <- as.character(df.varimp.MDA[,1])

## Remove first data row (= redundant)
df.pca.MDA <- df.pca.MDA[-1,]

## Converts factors to numeric values
df.pca.MDA <- data.matrix(df.pca.MDA)


## Modify row.names
row.names(df.pca.MDA) <- sapply(strsplit(as.character(row.names(df.pca.MDA)), "d"), "[[", 2)

summary(df.pca.MDA)

## Select species for PCA
df.pca.MDA <- df.pca.MDA[,1:6]

pca.MDA <- princomp(df.pca.MDA, scores = TRUE)
plot(pca.MDA)
summary(pca.MDA)
loadings(pca.MDA)
biplot(pca.MDA)
?biplot.princomp
