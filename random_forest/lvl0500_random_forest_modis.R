################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-18                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
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

data.raw <- read.csv2("csv/kili/lvl0300_biodiversity_data.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Subsetting data ############################################################
################################################################################

### Eliminate columns containing NA values and combine data in several ways ####
### to get different dataframe combinations ####################################

df.greyval.all <- data.raw[69:106]
df.diff.all <- data.raw[107:144]
df.sd.all <- data.raw[145:182]


## Create NA tables
df.na.greyval <- data.frame(colSums(is.na(df.greyval.all)))
names(df.na.greyval) <- c("NAs out of 225")

df.na.diff <- data.frame(colSums(is.na(df.diff.all)))
names(df.na.diff) <- c("NAs out of 225")

df.na.sd <- data.frame(colSums(is.na(df.sd.all)))
names(df.na.sd) <- c("NAs out of 225")


## Eliminate columns containing NA values
df.greyval <- cbind(data.raw[69:78], data.raw[87:106])  ## greyvalues
df.diff <- cbind(data.raw[108:116], data.raw[126:144])  ## diff
df.sd <- cbind(data.raw[145:154], data.raw[163:182])    ## sd 


################################################################################
### Species number #############################################################

## Extract species number column for converting it to a factor. Necessary for RandomForest classification
tmp.speciesnr <- data.raw[9]

## Modify biodiversity values to 2 digit numeric value
tmp.speciesnr.list <- as.list(as.numeric(t(tmp.speciesnr)))
tmp.speciesnr.list <- formatC(tmp.speciesnr.list, 
                              width = 2, 
                              format = "d", 
                              flag = "0")

## Modify biodiversity values: paste "SP" in front to create a character
tmp.speciesnr <- as.data.frame(paste0("SP", tmp.speciesnr.list))

## Set columnname
names(tmp.speciesnr) <- "SpeciesNr"

## Create multiple dataframes with species number as predictor datasets
df.spnr.greyval <- cbind(df.greyval, tmp.speciesnr)
df.spnr.diff <- cbind(df.diff, tmp.speciesnr)
df.spnr.greyval.sd <- cbind(df.greyval, df.sd, tmp.speciesnr)
df.spnr.diff.sd <- cbind(df.diff, df.sd, tmp.speciesnr)


################################################################################
### Subset by species ##########################################################

specfreq <- data.frame(colSums(data.raw[14:68], na.rm = TRUE))
# specfreq


## Define speciesname to subset
# Pnorisa.squalus data[63,] (112 observations)
species <- "Pnorisa.squalus"

## Select species data
species.df <- data.frame(data.raw[,names(data.raw) %in% c(species)])
names(species.df) <- species

## Replace NA-values by 0
species.df[is.na(species.df)] <- 0

tmp.species.list <- as.list(t(species.df))
tmp.species.list <- formatC(tmp.species.list, 
                            width = 2, 
                            format = "d", 
                            flag = "0")

tmp.species <- as.data.frame(paste0("PR", tmp.species.list))

## Create multiple dataframes with single species as predictor datasets
df.spec.greyval <- cbind(df.greyval, tmp.species)
df.spec.diff <- cbind(df.diff, tmp.species)
df.spec.greyval.sd <- cbind(df.greyval, df.sd, tmp.species)
df.spec.diff.sd <- cbind(df.diff, df.sd, tmp.species)


################################################################################
## Define Random Forest input data #############################################
################################################################################

df.input.rf <- df.spec.greyval ## Insert input dataset here!

predictor <- df.input.rf[,1:ncol(df.input.rf)-1]
response <- as.factor(df.input.rf[,ncol(df.input.rf)])


################################################################################
### Random Forest function #####################################################
################################################################################

# ## Define desired parameters
n.tree <- 500 # Number of trees to grow
m.try <- 2 # Number of variables randomly sampled as candidates at each split


## Function 
train.rf <- randomForest(x = predictor,
                         y = response,
                         importance = TRUE,
                         ntree = n.tree,
                         mtry = m.try,
                         nodesize = 2,
#                          na.action = na.omit(train.data),
                         type="classification",
                         do.trace = 100)

print(train.rf)


################################################################################
### Prediction #################################################################

# ## Create test-df
# names(train.data[1:180,5:ncol(train.data)-1])
# testData <- train.data[181:225,3:ncol(train.data)-1]
# 
# 
# ##  predict RInfo for new data set
# test.predict <- predict(train.rf, testData,type="prob", index=2, na.rm=TRUE, progress="window", overwrite=TRUE, filename="ProbPred.png")
# test.predict <- data.frame(predict(train.rf, testData))
# test.predict

