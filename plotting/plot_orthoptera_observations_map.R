cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot Orthoptera observations at Mt. Kilimanjaro
##  
##  Version: 2014-03-16
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
lib <- c("ggplot2", "ggmap")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/")
setwd("D:/Dropbox/Code/bifore/src/")


################################################################################
### Import data ################################################################
################################################################################

data.raw <- read.csv2("csv/kili/lvl0400_rf_number-of-species_all.csv",
                     dec = ",",
                     header = TRUE, 
                     stringsAsFactors = TRUE,
                     )

data.raw.10 <- read.csv2("csv/kili/lvl0400_rf_number-of-species_10.csv",
                         dec = ",",
                         header = TRUE, 
                         stringsAsFactors = TRUE,
                         )

## Read date column as a date
data.raw$date_observation <- as.Date(data.raw$date_observation, format="%Y-%j")
data.raw.10$date_observation <- as.Date(data.raw.10$date_observation, format="%Y-%j")


data.sp <- data.raw
data.sp.10 <- data.raw.10


################################################################################
### Subsetting #################################################################
################################################################################

data.observations <- data.raw[,1:13]
data.observations.10 <- data.raw.10[,1:13]

## Create additional year column
data.observations$year <- as.numeric(format(data.observations$date_observation, "%Y"))
data.observations.10$year <- as.numeric(format(data.observations.10$date_observation, "%Y"))


################################################################################
### Plot all orthoptera observations ###########################################
################################################################################

## Define output image | open image port
png("images/lvl0400_map_orthoptera_observations.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

kili.extent <- get_map(location = c(36.93865,
                                    -3.454621,
                                    37.76235,
                                    -2.775392),
                       scale = "auto",
                       maptype = "satellite",
                       color = "bw",
                       source = "google")

kilimap <- ggmap(kili.extent, 
                 extent = "normal",
                 maprange = TRUE)

orthoptera.obs1 <- geom_point(aes(x = lon,
                                  y = lat,
                                  size = 1,
                                  colour = nr.of.species),
                              show_guide = FALSE,
                              data = data.observations)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "Artenzahl",
                                     limits=c(0, 30))

style.plot1 <- theme(legend.background = element_rect(colour = "black"),
               plot.title = element_text(size = 20))

labeling.plot1 <- labs(title = "Artenzahl Mt. Kilimanjaro 2002 - 2012")

kilimap + orthoptera.obs1 + colourscale + labeling.plot1 + style.plot1


## Close image port
graphics.off()


################################################################################
### Plot Orthoptera Observations per year ######################################
################################################################################

## Define output image | open image port
png("images/lvl0400_map_orthoptera_observations_year.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)


kili.extent <- get_map(location = c(36.93865,
                                    -3.454621,
                                    37.76235,
                                    -2.775392),
                       scale = "auto",
                       maptype = "satellite",
                       color = "bw",
                       source = "google")

kilimap <- ggmap(kili.extent, 
                 extent = "normal",
                 maprange = TRUE)

orthoptera.obs1 <- geom_point(aes(x = lon,
                                  y = lat,
                                  colour = nr.of.species),
                              show_guide = FALSE,
                              data = data.observations)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "Artenzahl",
                                     limits=c(0, 30))

style.plot2 <- theme(legend.background = element_rect(colour = "black"),
                     plot.title = element_text(size = 20))

labeling.plot2 <- labs(title = "Artenzahl Mt. Kilimanjaro 2002 - 2012")

kilimap + orthoptera.obs1 + colourscale + facet_wrap(~ year) + style.plot2 + labeling.plot2


# ## Close image port
graphics.off()


################################################################################
### Plot orthoptera observations ###############################################
### species with less than 10 obs in different plots removed ###################
################################################################################

## Define output image | open image port
png("images/lvl0400_map_orthoptera_observations_10.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

kili.extent <- get_map(location = c(36.93865,
                                    -3.454621,
                                    37.76235,
                                    -2.775392),
                       scale = "auto",
                       maptype = "satellite",
                       color = "bw",
                       source = "google")

kilimap <- ggmap(kili.extent, 
                 extent = "normal",
                 maprange = TRUE)

orthoptera.obs1 <- geom_point(aes(x = lon,
                                  y = lat,
                                  size = 1,
                                  colour = nr.of.species),
                              show_guide = FALSE,
                              data = data.observations.10)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "Artenzahl",
                                     limits=c(0, 30))

style.plot1 <- theme(legend.background = element_rect(colour = "black"),
                     plot.title = element_text(size = 20))

labeling.plot1 <- labs(title = "Artenzahl Mt. Kilimanjaro 2002 - 2012 (10)")


kilimap + orthoptera.obs1 + colourscale + labeling.plot1 + style.plot1


## Close image port
graphics.off()


################################################################################
### Plot Orthoptera Observations per year ######################################
################################################################################

## Define output image | open image port
png("images/lvl0400_map_orthoptera_observations_year_10.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)


kili.extent <- get_map(location = c(36.93865,
                                    -3.454621,
                                    37.76235,
                                    -2.775392),
                       scale = "auto",
                       maptype = "satellite",
                       color = "bw",
                       source = "google")

kilimap <- ggmap(kili.extent, 
                 extent = "normal",
                 maprange = TRUE)

orthoptera.obs1 <- geom_point(aes(x = lon,
                                  y = lat,
                                  colour = nr.of.species),
                              show_guide = FALSE,
                              data = data.observations.10)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "Artenzahl",
                                     limits=c(0, 30))

style.plot2 <- theme(legend.background = element_rect(colour = "black"),
                     plot.title = element_text(size = 20))

labeling.plot2 <- labs(title = "Artenzahl Mt. Kilimanjaro 2002 - 2012 (10)")

kilimap + orthoptera.obs1 + colourscale + facet_wrap(~ year) + style.plot2 + labeling.plot2


# ## Close image port
graphics.off()


################################################################################
### Plot Phaneroptera.sparsa ###################################################
################################################################################

# ## Subset
# data.observations1 <- data.raw
# data.observations1$year <- as.numeric(format(data.observations1$date, "%Y"))
# data.observations1 <- cbind(data.observations1[1:11],
#                        data.observations1[ncol(data.observations1)],
#                        data.observations1$Phaneroptera.sparsa)
# 
# names(data.observations1)[13] <- "Phaneroptera.sparsa"
# 
# data.observations1.1 <- na.omit(data.observations1)
# 
# ## Define output image | open image port
# # png("images/lvl0050_map_Phaneroptera.sparsa_year_all_spec.png", 
# #     width = 1024 * 6, 
# #     height = 748 * 6, 
# #     units = "px", 
# #     res = 600)
# 
# kili.extent <- get_map(location = c(36.93865,
#                                     -3.454621,
#                                     37.76235,
#                                     -2.775392),
#                        scale = "auto",
#                        maptype = "satellite",
#                        color = "bw",
#                        source = "google")
# 
# kilimap <- ggmap(kili.extent, 
#                  extent = "normal",
#                  maprange = TRUE)
# 
# species1.1 <- geom_point(aes(x = lon,
#                              y = lat,
#                              colour = as.factor(Phaneroptera.sparsa)),
#                          show_guide = TRUE,
#                          data = data.observations1.1)
# 
# style.plot3 <- theme(legend.background = element_rect(colour = "black"),
#                plot.title = element_text(size = 20))
# 
# labeling.plot3 <- labs(title = "Phaneroptera sparsa")
# 
# colourscale <- scale_colour_brewer(type = "qual", palette = 2, name = "Prävalenz Index")
# 
# 
# kilimap + species1.1 + colourscale + facet_wrap(~ year) + labeling.plot3 + style.plot3
  

## Close image port
# graphics.off()