################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PREPARE DATASET FOR RANDOMFOREST - ALL OBSERVATIONS                        ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-01                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c()
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################
################################################################################

file.in.0300 <- "csv/kili/lvl0300_biodiversity_data.csv"
file.out.specno.all <- "csv/kili/lvl0400_specno_all.csv"
file.out.specno.10 <- "csv/kili/lvl0400_specno_10.csv"
file.out.prevalence.all <- "csv/kili/lvl0400_prevalence_all.csv"
file.out.prevalence.10 <- "csv/kili/lvl0400_prevalence_10.csv"
file.out.presabs.all <- "csv/kili/lvl0400_presence-absence_all.csv"
file.out.presabs.10 <- "csv/kili/lvl0400_presence-absence_10.csv"



# file.biodiversity.in <- "csv/kili/lvl0300_biodiversity_data.csv"
# path.biodiversity.allspec <- "csv/kili/lvl0400_all_spec.csv"
# path.biodiversity.data10 <- "csv/kili/lvl0400_biodiversity_data_10.csv"
# path.biodiversity.strat.plot <- "csv/kili/lvl0400_biodiversity_data_strat_plot.csv"
# file.all.spec.prevalence <- "csv/kili/lvl0400_prevalence_all_species.csv"
# file.data10.prevalence <- "csv/kili/lvl0400_prevalence_data_10.csv"
# file.presence.absence <- "csv/kili/lvl0400_presence-absence.csv"


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2(file.in.0300,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Subsetting data ############################################################
################################################################################

### Eliminate columns containing NA values and combine data in several ways ####
### to get different dataframe combinations ####################################

df.sub.basics <- cbind(data.raw[1:8], data.raw[10:13])
df.sub.specno <- data.raw[9]
df.sub.species <- data.raw[14:178]
df.sub.greyval.all <- data.raw[179:216]
df.sub.diff.all <- data.raw[217:254]
df.sub.sd.all <- data.raw[255:292]


################################################################################
### Remove bands containing NA values ##########################################
################################################################################

## Create NA value tables
# df.na.greyval <- data.frame(colSums(is.na(df.sub.greyval.all)))
# names(df.na.greyval) <- c("NAs out of 225")
# 
# df.na.diff <- data.frame(colSums(is.na(df.sub.diff.all)))
# names(df.na.diff) <- c("NAs out of 225")
# 
# df.na.sd <- data.frame(colSums(is.na(df.sub.sd.all)))
# names(df.na.sd) <- c("NAs out of 225")


## Eliminate columns containing NA values
df.sub.greyval <- cbind(data.raw[179:188], data.raw[197:216])  ## greyvalues
df.sub.diff <- cbind(data.raw[218:226], data.raw[236:254])  ## diff
df.sub.sd <- cbind(data.raw[255:264], data.raw[273:292])    ## sd


## Recombine dataframes
data <- cbind(df.sub.basics,
              df.sub.specno, 
              df.sub.species,
              df.sub.greyval, 
              df.sub.diff, 
              df.sub.sd)


################################################################################
### Remove species with less than 10 observations in different plots ###########
################################################################################

data10.list <- split(data, data$plot)
data10.tmp.list <- do.call("rbind", lapply(seq(data10.list), function(i) {
  matrix <- as.matrix(data10.list[[i]][, 14:178])
  t <- apply(matrix, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

data10.species.index <- which(apply(data10.tmp.list, 2, sum, na.rm = TRUE) >= 10) + 13
data10 <- data[, c(1:13, data10.species.index, 178:ncol(data))]
# names(data10)


## Subset data10
df.sub.10.basics <- data10[1:12]
df.sub.10.specno <- data10[13]
df.sub.10.species <- data10[14:69]
df.sub.10.greyval <- data10[70:99]
df.sub.10.diff <- data10[100:127]
df.sub.10.sd <- data10[128:157]

################################################################################
### Create speciesnumber dataframe for RandomForest ############################
################################################################################

### For all species
data.rf.specno <- cbind(df.sub.basics,
                        df.sub.specno,
                        df.sub.species,
                        df.sub.greyval,
                        df.sub.diff,
                        df.sub.sd)

## Write table - all species - bands containing NA values removed
write.table(data.rf.specno, 
            file = file.out.specno.all,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

################################################################################
### For species with less than 10 observations in different plots

data.rf.specno <- cbind(df.sub.10.basics,
                        df.sub.10.specno,
                        df.sub.10.species,
                        df.sub.10.greyval,
                        df.sub.10.diff,
                        df.sub.10.sd)

## Write table - all species - bands containing NA values removed
write.table(data.rf.specno, 
            file = file.out.specno.10,
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
prevalence.all <- data.frame(colSums(df.sub.species > 0, na.rm = TRUE), 
                             row.names = NULL)

## Combine new df
df.prevalence.all.spec <- cbind(names(df.sub.species), prevalence.all)

## Set colnames
colnames(df.prevalence.all.spec) <- c("species", "prevalence")


## Write table
write.table(df.prevalence.all.spec, file = file.out.prevalence.all,
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
            file = file.out.specno.10,
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
write.table(df.prevalence.10, file = file.out.prevalence.10,
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
### Create presence-absence df #################################################
################################################################################

## Read as matrix
pres.abs.matrix <- as.matrix(df.sub.10.species)

## Replace NA with 0
pres.abs.matrix[is.na(pres.abs.matrix)] <- 0

## Replace values >=1 with 1
pres.abs.matrix <- ifelse(pres.abs.matrix >= 1,1,0)

## Combine dataframes
df.presence.absence <- cbind(df.sub.10.basics,
                             as.data.frame(pres.abs.matrix),
                             df.sub.10.greyval,
                             df.sub.10.diff,
                             df.sub.10.sd)
# names(df.presence.absence)

write.table(df.presence.absence, file = file.out.presabs.10,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
