cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot prevalence variable importance measures (Mean Decrease Gini &
##  Mean Decrease Accuracy) of lvl0600 for the most common species. Also plots
##  variable importance measures foreach MODIS band.
##  
##  Version: 2014-05-27
##  
################################################################################
##
##  Copyright (C) 2014 Simon Schlauss (sschlauss@gmail.com)
##
##
##  This file is part of BiFoRe.
##  
##  BiFoRe is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  BiFoRe is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with BiFoRe.  If not, see <http://www.gnu.org/licenses/>.
##  
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
