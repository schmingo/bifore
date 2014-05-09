cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT PREVALENCE LVL0600 CONFUSION MATRIX, MEAN DECREASE ACCURACY AND       ##
## MEAN DECREASE GINI                                                         ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-05                                                        ##
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

df.varimp.MDA.reflect <- cbind(df.varimp.MDA[1],df.varimp.MDA[2:14])
df.varimp.MDA.emit <- cbind(df.varimp.MDA[1],df.varimp.MDA[15:ncol(df.varimp.MDA)])
df.varimp.MDG.reflect <- cbind(df.varimp.MDG[1],df.varimp.MDG[2:14])
df.varimp.MDG.emit <- cbind(df.varimp.MDG[1],df.varimp.MDG[15:ncol(df.varimp.MDA)])


## melt dataframes
df.varimp.MDA.melt <- melt(df.varimp.MDA, id="species")
df.varimp.MDG.melt <- melt(df.varimp.MDG, id="species")
df.varimp.MDA.reflect.melt <- melt(df.varimp.MDA.reflect, id="species")
df.varimp.MDA.emit.melt <- melt(df.varimp.MDA.emit, id="species")
df.varimp.MDG.reflect.melt <- melt(df.varimp.MDG.reflect, id="species")
df.varimp.MDG.emit.melt <- melt(df.varimp.MDG.emit, id="species")


################################################################################
### Plotting - Mean Decrease Accuracy - by bands ###############################
################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_bybands_MDA_reflective.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)
# 
# ggplot(data=df.varimp.MDA.reflect.melt,
#        aes(x=species, y=value, group=variable)) +
#   geom_line() +
#   ylim(-2,15) +
#   xlab(NULL) +
#   ylab("Mean Decrease Accuracy") +
#   theme_bw() +
#   ggtitle("Prevalence - RandomForest - Mean Decrease Accuracy (reflective bands)") +
#   theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
#         plot.title = element_text(lineheight = .8, size = 20)) +
#   facet_wrap(~variable, as.table=FALSE, ncol = 4)
# 
# ## Close image port
# graphics.off()
# 
# ################################################################################
# 
# ## Define output image | open image port
# png("images/lvl0600_prevalence_bybands_MDA_emissive.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)
# 
# ggplot(data=df.varimp.MDA.emit.melt,
#        aes(x=species, y=value, group=variable)) +
#   geom_line() +
#   ylim(-2,15) +
#   xlab(NULL) +
#   ylab("Mean Decrease Accuracy") +
#   theme_bw() +
#   ggtitle("Prevalence - RandomForest - Mean Decrease Accuracy (emissive bands)") +
#   theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
#         plot.title = element_text(lineheight = .8, size = 20)) +
#   facet_wrap(~variable, as.table=FALSE, ncol = 4)
# 
# ## Close image port
# graphics.off()

################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_bybands_MDA.png", 
#     width = 748 * 6, 
#     height = 1024 * 6, 
#     units = "px", 
#     res = 600)

ggplot(data=df.varimp.MDA.melt,
       aes(x=species, y=value, group=variable)) +
  geom_line() +
  ylim(-1,13) +
  xlab(NULL) +
  ylab("Mean Decrease Accuracy") +
  theme_bw() +
  ggtitle("Prevalence - RandomForest - Mean Decrease Accuracy") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20)) +
  facet_wrap(~variable, as.table=TRUE, ncol = 3)

## Close image port
# graphics.off()


################################################################################
### Plotting - Mean Decrease Gini - by bands ###################################
################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_bybands_MDG_reflective.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)
# 
# ggplot(data=df.varimp.MDG.reflect.melt,
#        aes(x=species, y=value, group=variable)) +
#   geom_line() +
#   ylim(-2,15) +
#   xlab(NULL) +
#   ylab("Mean Decrease Gini") +
#   theme_bw() +
#   ggtitle("Prevalence - RandomForest - Mean Decrease Gini (reflective bands)") +
#   theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
#         plot.title = element_text(lineheight = .8, size = 20)) +
#   facet_wrap(~variable, as.table=FALSE, ncol = 4)
# 
# ## Close image port
# graphics.off()
# 
# ################################################################################
# 
# ## Define output image | open image port
# png("images/lvl0600_prevalence_bybands_MDG_bybands.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)
# 
# ggplot(data=df.varimp.MDG.emit.melt,
#        aes(x=species, y=value, group=variable)) +
#   geom_line() +
#   ylim(-2,15) +
#   xlab(NULL) +
#   ylab("Mean Decrease Gini") +
#   theme_bw() +
#   ggtitle("Prevalence - RandomForest - Mean Decrease Gini (emissive bands)") +
#   theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
#         plot.title = element_text(lineheight = .8, size = 20)) +
#   facet_wrap(~variable, as.table=FALSE, ncol = 4)
# 
# ## Close image port
# graphics.off()

################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_bybands_MDG.png", 
#     width = 748 * 6, 
#     height = 1024 * 6, 
#     units = "px", 
#     res = 600)

ggplot(data=df.varimp.MDG.melt,
       aes(x=species, y=value, group=variable)) +
  geom_line() +
  ylim(0,5) +
  xlab(NULL) +
  ylab("Mean Decrease Gini") +
  theme_bw() +
  ggtitle("Prevalence - RandomForest - Mean Decrease Gini") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20)) +
  facet_wrap(~variable, as.table=TRUE, ncol = 3)



## Close image port
# graphics.off()




################################################################################
### Plotting - Mean Decrease Accuracy - by species #############################
################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_byspecies_MDA.png", 
#     width = 1024 * 6, 
#     height = 1024 * 6, 
#     units = "px", 
#     res = 600)

ggplot(data=df.varimp.MDA.melt,
       aes(x=variable, y=value, group=species)) +
  geom_line() +
  ylim(-1,13) +
  xlab("MODIS bands") +
  ylab("Mean Decrease Accuracy") +
  theme_bw() +
  ggtitle("Prevalence - RandomForest - Mean Decrease Accuracy - by species") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20)) +
  facet_wrap(~species, as.table=TRUE, ncol = 3)

## Close image port
# graphics.off()


################################################################################
### Plotting - Mean Decrease Gini - by species #################################
################################################################################

## Define output image | open image port
# png("images/lvl0600_prevalence_byspecies_MDG.png", 
#     width = 1024 * 6, 
#     height = 1024 * 6, 
#     units = "px", 
#     res = 600)

ggplot(data=df.varimp.MDG.melt,
       aes(x=variable, y=value, group=species)) +
  geom_line() +
  ylim(0,5) +
  xlab("MODIS bands") +
  ylab("Mean Decrease Gini") +
  theme_bw() +
  ggtitle("Prevalence - RandomForest - Mean Decrease Gini - by species") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20)) +
  facet_wrap(~species, as.table=TRUE, ncol = 3)

## Close image port
# graphics.off()


################################################################################
### Plotting - Mean Decrease Accuracy - Principal Components Analysis ##########
################################################################################
# df.varimp.MDA.princomp <- data.frame(t(df.varimp.MDA))

## Transpose df
df.varimp.MDA.princomp <- as.data.frame(t(df.varimp.MDA))

## Write species into first row
df.varimp.MDA.princomp <- cbind(rownames(df.varimp.MDA.princomp), df.varimp.MDA.princomp)

## Remove rownames
row.names(df.varimp.MDA.princomp) <- NULL
names(df.varimp.MDA.princomp) <- dimnames(df.varimp.MDA[[2]])

dimnames(df.varimp.MDA)[[2]]

.
.
.

princomp()
