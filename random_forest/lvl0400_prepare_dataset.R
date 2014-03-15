################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PREPARE DATASET FOR RANDOMFOREST                                           ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-15                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("sampling")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################
################################################################################

path.biodiversity.allspec <- "csv/kili/lvl0400_biodiversity_data_all_spec.csv"
path.biodiversity.data10 <- "csv/kili/lvl0400_biodiversity_data_10.csv"
path.biodiversity.strat.plot <- "csv/kili/lvl0400_biodiversity_data_strat_plot.csv"



################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("csv/kili/lvl0300_biodiversity_data_all_spec.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Subsetting data ############################################################
################################################################################

### Eliminate columns containing NA values and combine data in several ways ####
### to get different dataframe combinations ####################################

df.basics <- data.raw[1:13]
df.species <- data.raw[14:203]
df.greyval.all <- data.raw[204:241]
df.diff.all <- data.raw[242:279]
df.sd.all <- data.raw[280:317]


################################################################################
### Remove bands containing NA values ##########################################
################################################################################


df.na.greyval <- data.frame(colSums(is.na(df.greyval.all)))
names(df.na.greyval) <- c("NAs out of 225")

df.na.diff <- data.frame(colSums(is.na(df.diff.all)))
names(df.na.diff) <- c("NAs out of 225")

df.na.sd <- data.frame(colSums(is.na(df.sd.all)))
names(df.na.sd) <- c("NAs out of 225")


## Eliminate columns containing NA values
df.greyval <- cbind(data.raw[204:213], data.raw[222:241])  ## greyvalues
df.diff <- cbind(data.raw[243:251], data.raw[261:279])  ## diff
df.sd <- cbind(data.raw[280:289], data.raw[298:317])    ## sd


## Recombine dataframes
data <- cbind(df.basics, 
              df.species,
              df.greyval, 
              df.diff, 
              df.sd)


## Write new table - all species - bands containing NA values removed
write.table(data, 
            file = path.biodiversity.allspec,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


################################################################################
### Remove species with less than 10 observations ##############################
################################################################################

data.list <- split(data, data$plot)
tst.list <- do.call("rbind", lapply(seq(data.list), function(i) {
  matrix <- as.matrix(data.list[[i]][, 14:203])
  t <- apply(matrix, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

index.species10 <- which(apply(tst.list, 2, sum, na.rm = TRUE) >= 10) + 13
data10 <- data[, c(1:13, index.species10, 203:ncol(data))]
# names(data10)

## Write table
write.table(data10, 
            file = path.biodiversity.data10,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


################################################################################
### Stratified sampling - only one random observation per plot #################
################################################################################

set.seed(50)

data.strat <- data[strata(data, 
                           stratanames = "plot", 
                           size = rep(1,length(unique(data$plot))),
                           method = "srswor")$ID_unit, ]


## Write table
write.table(data.strat, 
            file = path.biodiversity.strat.plot,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
