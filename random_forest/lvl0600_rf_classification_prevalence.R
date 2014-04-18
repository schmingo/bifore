################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
## FURTHER PREVALENCE CALCULATIONS FOR ALL SPECIES                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-18                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
cat("\014")
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("csv/kili/lvl0400_rf_strat_prevalence_10.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Combining data for randomForest ############################################
################################################################################

## List species
lst.species <- names(data.raw[13:67])
lst.species

species <- lst.species[47]


## Split incoming dataset
df.greyval <- data.raw[68:97]  ## greyvalues
df.diff <- data.raw[98:125]  ## diff
df.sd <- data.raw[126:155]  ## sd 

## Select species data
df.species <- data.frame(data.raw[,names(data.raw) %in% c(species)])
names(df.species) <- species

tmp.species <- df.species

# summary(tmp.species)

## Create multiple dataframes with single species as predictor datasets
df.spec.greyval <- cbind(df.greyval, tmp.species)
# df.spec.diff <- cbind(df.diff, tmp.species)
# df.spec.greyval.diff <- cbind(df.greyval, df.diff, tmp.species)
# df.spec.greyval.sd <- cbind(df.greyval, df.sd, tmp.species)
# df.spec.diff.sd <- cbind(df.diff, df.sd, tmp.species)


## Define Random Forest input data #############################################
train.data <- df.spec.greyval ## Insert input dataset here!


################################################################################
### Random Forest function #####################################################
### Classification - single species ############################################
################################################################################

predictor_modisVAL <- train.data[,1:ncol(train.data)-1]
response_speciesCLASS <- as.factor(train.data[,ncol(train.data)])

## Function ####################################################################
train.rf <- randomForest(x = predictor_modisVAL,
                         y = response_speciesCLASS,
                         importance = TRUE,
                         ntree = 500,
                         mtry = 2,
                         nodesize = 2,
                         type="classification",
                         do.trace = 100)


train.rf$confusion
train.rf$confusion[1,1]
train.rf$confusion[1,2]
train.rf$confusion[2,1]
train.rf$confusion[2,2]
