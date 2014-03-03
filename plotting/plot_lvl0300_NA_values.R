################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT EXTRACT GREYVALUES, FIRST DERIVATE & CALCULATED SD                    ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-03                                                        ##
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
                       stringsAsFactors = FALSE)

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

names(df.NA) <- c("NA_greyvalues", "NA_diff", "NA_sd")
df.NA$bandnames <- bandnames[,1]

df.NA.melt <- melt(df.NA, id.vars="bandnames")


################################################################################
### Plotting ###################################################################
################################################################################

## Plot single subset (greyvalues, diff or sd)
qplot(y=df.NA[,1], 
      x=df.NA$bandnames, 
      geom = "bar", 
      binwidth = 2, 
      stat="identity",
      position="dodge")


# ## Using barplot()
# barplot(height = df.NA,
#         width = length(row.names(df.NA)), 
#         beside = TRUE,
#         ylab = "count NA",
#         xlab = "MODIS bands")


ggplot(df.NA.melt, aes(df.NA.melt$variable, df.NA.melt$value, fill = df.NA.melt$bandnames)) +
  geom_bar(stat= "identity", position="dodge") + 
  theme(axis.text.x=element_text(angle=-90))

ggplot(df.NA.melt, aes(x=df.NA.melt$bandnames, y=df.NA.melt$value, fill=df.NA.melt$variable)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x=element_text(angle=-90)) +
  ggtitle("Summary of NA values for MODIS MYD02")
