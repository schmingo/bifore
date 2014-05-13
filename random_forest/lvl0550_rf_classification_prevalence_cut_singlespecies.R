cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST SINGLE SPECIES                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-06                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/")
setwd("D:/Dropbox/Code/bifore/src/")

## Set species to test
s <- "Pnorisa.squalus"


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("csv/kili/lvl0400_rf_strat_prevalence_cut.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Combining data for randomForest ############################################
################################################################################

set.seed(50)

## List species
names(data.raw)

lst.species <- names(data.raw[(which(names(data.raw)=="coordN")+1):(which(names(data.raw)=="greyval_band_1")-1)])
lst.species

## Split incoming dataset
df.greyval <- data.raw[(which(names(data.raw)=="greyval_band_1")):(which(names(data.raw)=="greyval_band_36"))]

# ## Split incoming dataset
# df.greyval <- data.raw[178:207]  ## greyvalues
# df.diff <- data.raw[208:235]  ## diff
# df.sd <- data.raw[236:265]  ## sd 

## Select species data
df.species <- data.frame(data.raw[,names(data.raw) %in% c(s)])
names(df.species) <- s

tmp.species <- df.species

# summary(tmp.species)

## Create multiple dataframes with single species as predictor datasets
df.spec.greyval <- cbind(df.greyval, tmp.species)
# df.spec.diff <- cbind(df.diff, tmp.species)
# df.spec.greyval.diff <- cbind(df.greyval, df.diff, tmp.species)
# df.spec.greyval.sd <- cbind(df.greyval, df.sd, tmp.species)
# df.spec.diff.sd <- cbind(df.diff, df.sd, tmp.species)


## Define Random Forest input data #############################################

df.input.rf <- df.spec.greyval ## Insert input dataset here!
train.data <- df.input.rf


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
                         nodesize = 2,
                         type="classification",
                         do.trace = 100)


rf.plot <- plot(train.rf, type="l", main = paste0("Prevalence ", s, 
                                        "\n RandomForest classification \n", 
                                        "Mean squared error"))

rf.plot

print(train.rf)

print("Observed classes")
table(tmp.species)


################################################################################
### Further interpretation #####################################################
################################################################################

varimp <- importance(train.rf)


varimp.plot <- varImpPlot(train.rf, sort = TRUE, n.var = 30,
                          main = paste0(s," - Variable importance"))
