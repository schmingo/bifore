cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PREPARE DATASET FOR RANDOMFOREST - ONE RANDOM OBSERVATION PER PLOT         ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-03                                                        ##
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

file.in.0300 <- "csv/kili/lvl0300_biodiversity_data.csv"
file.out.braunblanq.all <- "csv/kili/lvl0400_rf_strat_braun-blanquet_all.csv"
file.out.braunblanq.cut <- "csv/kili/lvl0400_rf_strat_braun-blanquet_cut.csv"
file.out.specno.all <- "csv/kili/lvl0400_rf_strat_number-of-species_all.csv"
file.out.specno.cut <- "csv/kili/lvl0400_rf_strat_number-of-species_cut.csv"
file.out.prevalence.all <- "csv/kili/lvl0400_rf_strat_prevalence_all.csv"
file.out.prevalence.cut <- "csv/kili/lvl0400_rf_strat_prevalence_cut.csv"

file.out.simple.noofspecies.all <- "csv/kili/lvl0400_strat_simple_number-of-species_all.csv"
file.out.simple.noofspecies.cut <- "csv/kili/lvl0400_strat_simple_number-of-species_cut.csv"


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2(file.in.0300,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Remove bands containing NA values ##########################################
################################################################################

data.raw <- cbind(data.raw[1:8], # basics
                  data.raw[10:13], # basics
                  data.raw[9], # no.of.species
                  data.raw[14:178], # species
                  data.raw[179:188], # greyvalues
                  data.raw[197:215], # greyvalues
                  data.raw[216:226], # diff
                  data.raw[236:254], # diff
                  data.raw[255:264], # sd
                  data.raw[273:292]) # sd

# names(data.raw)

## Check greyvalues, diff and sd colums for NA values
anyNA(data.raw[179:267])


################################################################################
### Remove species with less than x observations in different plots ############
### Most abundant species ######################################################
################################################################################

## Set minimum observations in different plots
obs <- 15

data.cut.list <- split(data.raw, data.raw$plot)
data.cut.tmp.list <- do.call("rbind", lapply(seq(data.cut.list), function(i) {
  matrix <- as.matrix(data.cut.list[[i]][, 14:178])
  t <- apply(matrix, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

data.cut.species.index <- which(apply(data.cut.tmp.list, 2, sum, na.rm = TRUE) >= obs) + 13

data.cut <- data.raw[, c(1:13, data.cut.species.index, 179:ncol(data.raw))]

# names(data.cut)


################################################################################
### Stratified sampling - only one random observation per plot #################
################################################################################

set.seed(50)

data.raw <- data.raw[strata(data.raw, 
                            stratanames = "plot", 
                            size = rep(1,length(unique(data.raw$plot))),
                            method = "srswor")$ID_unit, ]

data.cut <- data.cut[strata(data.cut, 
                            stratanames = "plot", 
                            size = rep(1,length(unique(data.cut$plot))),
                            method = "srswor")$ID_unit, ]
names(data.raw)
names(data.cut)


################################################################################
### Subsetting data ############################################################
################################################################################

## Subset data.raw (all species)
df.sub.all.basics <- data.raw[1:12]
df.sub.all.specno <- data.raw[13]
df.sub.all.species <- data.raw[14:178]
df.sub.all.greyval <- data.raw[179:208]
df.sub.all.diff <- data.raw[209:237]
df.sub.all.sd <- data.raw[238:267]


## Subset data.cut (most abundant species)
df.sub.cut.basics <- data.cut[1:length(df.sub.all.basics)]
len <- length(df.sub.cut.basics)

df.sub.cut.specno <- data.cut[len + 1]
len <- len + length(df.sub.cut.specno)

df.sub.cut.species <- data.cut[(len + 1):(len + length(data.cut.species.index))]
len <- len + length(df.sub.cut.species)

df.sub.cut.greyval <- data.cut[(len + 1):(len + length(df.sub.all.greyval))]
len <- len + length(df.sub.cut.greyval)

df.sub.cut.diff <- data.cut[(len + 1):(len + length(df.sub.all.diff))]
len <- len + length(df.sub.cut.diff)

df.sub.cut.sd <- data.cut[(len + 1):(len + length(df.sub.all.sd))]


## calculate new no.of.species (cut sophisticates old no.of.species)
df.sub.cut.specno <- data.frame(apply(df.sub.cut.species,
                                      1,
                                      function(x) sum(!is.na(x[1:ncol(df.sub.cut.species)]))))
names(df.sub.cut.specno) <- "nr.of.species"



################################################################################
################################################################################
### BRAUN-BLANQUET #############################################################
### All species + most abundant species ########################################
################################################################################
################################################################################

## All species #################################################################
data.rf.braunblanq.all <- cbind(df.sub.all.basics,
                                df.sub.all.species,
                                df.sub.all.greyval,
                                df.sub.all.diff,
                                df.sub.all.sd)

write.table(data.rf.braunblanq.all,
            file = file.out.braunblanq.all,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

## Most abundant species (cut) #################################################
data.rf.braunblanq.cut <- cbind(df.sub.cut.basics,
                                df.sub.cut.species,
                                df.sub.cut.greyval,
                                df.sub.cut.diff,
                                df.sub.cut.sd)

## write braun-blanquet table for most abundant species
write.table(data.rf.braunblanq.cut, 
            file = file.out.braunblanq.cut,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


# ## Test data.cut (most abundant species)
# test.data.cut.matrix <- as.matrix(df.sub.cut.species)
# test.data.cut.matrix[is.na(test.data.cut.matrix)] <- 0
# test.data.cut.matrix <- ifelse(test.data.cut.matrix >= 1,1,0)
# df.test.data.cut <- as.data.frame(colSums(test.data.cut.matrix, na.rm = TRUE))
# 
# print(df.test.data.cut)
# nrow(df.test.data.cut)
# summary(df.test.data.cut) # cut shold be >= obs


################################################################################
################################################################################
### Create number of species dataframe for RandomForest ########################
### All species + most abundant species ########################################
################################################################################
################################################################################

## All species #################################################################
data.rf.specno.all <- cbind(df.sub.all.basics,
                            df.sub.all.specno,
                            df.sub.all.greyval,
                            df.sub.all.diff,
                            df.sub.all.sd)

write.table(data.rf.specno.all, 
            file = file.out.specno.all,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

## Most abundant species (cut) #################################################
data.rf.specno.cut <- cbind(df.sub.cut.basics,
                            df.sub.cut.specno,
                            df.sub.cut.greyval,
                            df.sub.cut.diff,
                            df.sub.cut.sd)

write.table(data.rf.specno.cut, 
            file = file.out.specno.cut,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


################################################################################
################################################################################
### Create prevalence dataframe ################################################
### All species + most abundant species ########################################
################################################################################
################################################################################

## All species #################################################################
## Read as matrix
matrix.prevalence.all <- as.matrix(df.sub.all.species)

## Replace NA with 0
matrix.prevalence.all[is.na(matrix.prevalence.all)] <- 0

## Replace values >=1 with 1
matrix.prevalence.all <- ifelse(matrix.prevalence.all >= 1,1,0)

## Combine dataframes
data.rf.prevalence.all <- cbind(df.sub.all.basics,
                                as.data.frame(matrix.prevalence.all),
                                df.sub.all.greyval,
                                df.sub.all.diff,
                                df.sub.all.sd)

write.table(data.rf.prevalence.all,
            file = file.out.prevalence.all,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

## Most abundant species (cut) #################################################
## Read as matrix
matrix.prevalence.cut <- as.matrix(df.sub.cut.species)

## Replace NA with 0
matrix.prevalence.cut[is.na(matrix.prevalence.cut)] <- 0

## Replace values >=1 with 1
matrix.prevalence.cut <- ifelse(matrix.prevalence.cut >= 1,1,0)

## Combine dataframes
data.rf.prevalence.cut <- cbind(df.sub.cut.basics,
                               as.data.frame(matrix.prevalence.cut),
                               df.sub.cut.greyval,
                               df.sub.cut.diff,
                               df.sub.cut.sd)

write.table(data.rf.prevalence.cut,
            file = file.out.prevalence.cut,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


################################################################################
################################################################################
### Create simple number of species table for all plots ########################
### All species + most abundant species ########################################
################################################################################
################################################################################

## Calculate no.of.species for all species
df.noofspecies.all <- data.frame(colSums(df.sub.all.species > 0, na.rm = TRUE), 
                                 row.names = NULL)
df.noofspecies.cut <- data.frame(colSums(df.sub.cut.species > 0, na.rm = TRUE), 
                                row.names = NULL)

## Combine new df
df.noofspecies.all <- cbind(names(df.sub.all.species), df.noofspecies.all)
df.noofspecies.cut <- cbind(names(df.sub.cut.species), df.noofspecies.cut)

## Set colnames
colnames(df.noofspecies.all) <- c("species", "number.of.species")
colnames(df.noofspecies.cut) <- c("species", "number.of.species")

## Write table
write.table(df.noofspecies.all, 
            file = file.out.simple.noofspecies.all,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

write.table(df.noofspecies.cut, 
            file = file.out.simple.noofspecies.cut,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
