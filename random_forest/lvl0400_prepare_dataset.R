################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PREPARE DATASET FOR RANDOMFOREST                                           ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-26                                                        ##
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

path.biodiversity.in <- "csv/kili/lvl0300_biodiversity_data.csv"
path.biodiversity.allspec <- "csv/kili/lvl0400_biodiversity_data_all_spec.csv"
path.biodiversity.data10 <- "csv/kili/lvl0400_biodiversity_data_10.csv"
path.biodiversity.strat.plot <- "csv/kili/lvl0400_biodiversity_data_strat_plot.csv"
file.all.spec.prevalence <- "csv/kili/lvl0400_prevalence_all_species.csv"
file.data10.prevalence <- "csv/kili/lvl0400_prevalence_data_10.csv"


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2(path.biodiversity.in,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Subsetting data ############################################################
################################################################################

### Eliminate columns containing NA values and combine data in several ways ####
### to get different dataframe combinations ####################################

df.basics <- data.raw[1:13]
df.species <- data.raw[14:178]
df.greyval.all <- data.raw[179:216]
df.diff.all <- data.raw[217:254]
df.sd.all <- data.raw[255:292]


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
df.greyval <- cbind(data.raw[179:188], data.raw[197:216])  ## greyvalues
df.diff <- cbind(data.raw[218:226], data.raw[236:254])  ## diff
df.sd <- cbind(data.raw[255:264], data.raw[273:292])    ## sd


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
### Calculate prevalence for all species #######################################
################################################################################

## Subset species dataset
data.all.spec <- data[14:178]

## Calculate prevalence
prevalence.all <- data.frame(colSums(data.all.spec > 0, na.rm = TRUE), 
                         row.names = NULL)

## Combine new df
df.prevalence.all.spec <- cbind(names(data.all.spec), prevalence.all)

## Set colnames
colnames(df.prevalence.all.spec) <- c("species", "prevalence")


## Write table
write.table(df.prevalence.all.spec, file = file.all.spec.prevalence,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


################################################################################
### Remove species with less than 10 observations in different plots ###########
################################################################################

data.list <- split(data, data$plot)
tst.list <- do.call("rbind", lapply(seq(data.list), function(i) {
  matrix <- as.matrix(data.list[[i]][, 14:178])
  t <- apply(matrix, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

index.species10 <- which(apply(tst.list, 2, sum, na.rm = TRUE) >= 10) + 13
data10 <- data[, c(1:13, index.species10, 178:ncol(data))]
names(data10)

## Write table
write.table(data10, 
            file = path.biodiversity.data10,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


################################################################################
### Prevalence - species with less than 10 observations in different plots #####
################################################################################

## Subset species dataset
data10.spec <- data10[14:69]

## Calculate prevalence
prevalence.10 <- data.frame(colSums(data10.spec > 0, na.rm = TRUE), 
                            row.names = NULL)

## Combine new df
df.prevalence.10 <- cbind(names(data10.spec), prevalence.10)

## Set colnames
colnames(df.prevalence.10) <- c("species", "prevalence")


## Write table
write.table(df.prevalence.10, file = file.data10.prevalence,
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


################################################################################
### Create df indicating if species has been observed or not ###################
################################################################################

## Read as matrix
spec.matrix <- as.matrix(df.species)

## Replace NA with 0
spec.matrix[is.na(spec.matrix)] <- 0

## Replace values >=1 with 1
spec.matrix <- ifelse(spec.matrix >= 1,1,0)
