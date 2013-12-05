################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOTTING ORTHOPTERA OBSERVATIONS AT MT. KILIMANJARO                        ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-12-02                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "ggmap")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Import data ################################################################

data <- read.csv2("csv/kili/abundance_data_subset.csv",
                     dec = ".",
                     header = TRUE, 
                     stringsAsFactors = TRUE,
                     )

## Read date column as a date
data$date <- as.Date(data$date, format="%Y-%m-%d")

data.sp <- data


################################################################################
### Subsetting #################################################################

data.species <- data[,1:11]

## Create additional year column
data.species$year <- as.numeric(format(data.species$date, "%Y"))


################################################################################
### Plots ######################################################################

### Plot all orthoptera observations ###########################################

## Define output image | open image port
# png("images/map_kili_orthoptera_observations.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

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
                              data = data.species)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "Number of species"
                                     )


kilimap + orthoptera.obs1 + colourscale


# ## Close image port
# graphics.off()

### Plot Orthoptera Observations per year ######################################

## Define output image | open image port
# png("images/map_kili_orthoptera_observations_year.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)


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
                              data = data.species)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "Number of species"
                                     )

kilimap + orthoptera.obs1 + colourscale + facet_wrap(~ year)


# ## Close image port
# graphics.off()

### Plot Phaneroptera.sparsa ###################################################

## Subset
data.species1 <- data
data.species1$year <- as.numeric(format(data.species1$date, "%Y"))
data.species1 <- cbind(data.species1[1:11],
                       data.species1[ncol(data.species1)],
                       data.species1$Phaneroptera.sparsa)

names(data.species1)[13] <- "Phaneroptera.sparsa"

data.species1.1 <- na.omit(data.species1)

## Define output image | open image port
# png("images/map_kili_Phaneroptera.sparsa_year.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

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

species1.1 <- geom_point(aes(x = lon,
                             y = lat,
                             colour = as.factor(Phaneroptera.sparsa)),
                         show_guide = TRUE,
                         data = data.species1.1)

style <- theme(legend.background = element_rect(colour = "black"),
               plot.title = element_text(size = 20))

labeling <- labs(title = "Phaneroptera sparsa")

colourscale <- scale_colour_brewer(type = "qual", palette = 2, name = "Prävalenz Index")


kilimap + species1.1 + colourscale + facet_wrap(~ year) + labeling + style
  

# ## Close image port
# graphics.off()