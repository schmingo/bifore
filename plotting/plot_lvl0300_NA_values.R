################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT EXTRACT GREYVALUES, FIRST DERIVATE & CALCULATED SD                    ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-10                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("ggplot2", "reshape2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Set filepath
path.lvl0300.csv <- "csv/kili/lvl0300_biodiversity_data.csv"
path.bandnames.csv <- "csv/kili/bandnames.csv"


## Import dataset
data.raw <- read.csv2(path.lvl0300.csv,
                      dec = ",",
                      header = TRUE, 
                      stringsAsFactors = FALSE)

bandnames <- read.csv2(path.bandnames.csv,
                       header = TRUE,
                       stringsAsFactors = TRUE)

################################################################################
### Subsetting #################################################################
################################################################################

df.greyval.all <- data.raw[69:106]
df.diff.all <- data.raw[107:144]
df.sd.all <- data.raw[145:182]

## Create df, count NA's for each MODIS band
df.NA <- cbind.data.frame(colSums(is.na(df.greyval.all)),
                          colSums(is.na(df.diff.all)),
                          colSums(is.na(df.sd.all)))

names(df.NA) <- c("greyvalues", "first_derivate", "standard_deviation")
df.NA$bandnames <- bandnames[,1]

df.NA.melt <- melt(df.NA, id.vars="bandnames")
names(df.NA.melt) <- c("MODIS_bands", "NA_values", "NA_count")


################################################################################
### Plotting ###################################################################
################################################################################

## Plot single subset (greyvalues, diff or sd)
# qplot(y=df.NA[,1], 
#       x=df.NA$bandnames, 
#       geom = "bar", 
#       binwidth = 2, 
#       stat="identity",
#       position="dodge")


# ## Using barplot()
# barplot(height = df.NA,
#         width = length(row.names(df.NA)), 
#         beside = TRUE,
#         ylab = "count NA",
#         xlab = "MODIS bands")


## Define output image | open image port
# png("images/lvl0310_na_values.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot <- ggplot(df.NA.melt, aes(x=MODIS_bands, y=NA_count, fill=NA_values)) + 
  geom_bar(position="dodge", stat="identity", width=1, colour="white") +
  scale_fill_grey() +
#   coord_flip() +
  xlab("MODIS bands") +
  ylab("NA counts") +
  ggtitle("Summary of NA values for MODIS MYD02") +
  theme(axis.text.x=element_text(angle=90, hjust = 0, vjust = .5),
        plot.title = element_text(lineheight = .8, size = 20))

plot

## Close image port
# graphics.off()