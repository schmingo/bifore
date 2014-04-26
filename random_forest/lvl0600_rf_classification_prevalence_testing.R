################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
## FURTHER PREVALENCE CALCULATIONS FOR ALL SPECIES                            ##
## (VARIABLE IMPORTANCE AND CONFUSION MATRIX)                                 ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-26                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
cat("\014")
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest", "foreach", "doParallel")
lapply(lib, function(...) require(..., character.only = TRUE))

# ncores <- detectCores()-1

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/csv/kili/")

## Timekeeping
starttime <- Sys.time()


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("lvl0400_rf_strat_prevalence_10.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Combining data for randomForest ############################################
################################################################################
set.seed(50)

## List species
lst.species <- names(data.raw[13:55])
lst.species

## Split incoming dataset
df.greyval <- data.raw[56:85]  ## greyvalues
df.diff <- data.raw[86:113]  ## diff
df.sd <- data.raw[114:143]  ## sd 


s <- lst.species[37]

## Select species data
df.species <- data.frame(data.raw[,names(data.raw) %in% c(s)])
names(df.species) <- s

tmp.species <- df.species

# summary(tmp.species)

## Create dataframe with single species as predictor dataset
df.spec.greyval <- cbind(df.greyval, tmp.species)



## Define Random Forest input data ###########################################
train.data <- df.spec.greyval


##############################################################################
### Random Forest function ###################################################
### Classification - single species ##########################################
##############################################################################

predictor_modisVAL <- train.data[,1:ncol(train.data)-1]
response_speciesCLASS <- as.factor(train.data[,ncol(train.data)])

## Initialize confusion matrix variables
conf.1.1 = NULL
conf.1.2 = NULL
conf.1.3 = NULL
conf.2.1 = NULL
conf.2.2 = NULL
conf.2.3 = NULL
vi_MDA_01 = NULL
vi_MDA_02 = NULL
vi_MDA_03 = NULL
vi_MDA_04 = NULL
vi_MDA_05 = NULL
vi_MDA_06 = NULL
vi_MDA_07 = NULL
vi_MDA_08 = NULL
vi_MDA_09 = NULL
vi_MDA_10 = NULL
vi_MDA_17 = NULL
vi_MDA_18 = NULL
vi_MDA_19 = NULL
vi_MDA_20 = NULL
vi_MDA_21 = NULL
vi_MDA_22 = NULL
vi_MDA_23 = NULL
vi_MDA_24 = NULL
vi_MDA_25 = NULL
vi_MDA_26 = NULL
vi_MDA_27 = NULL
vi_MDA_28 = NULL
vi_MDA_29 = NULL
vi_MDA_30 = NULL
vi_MDA_31 = NULL
vi_MDA_32 = NULL
vi_MDA_33 = NULL
vi_MDA_34 = NULL
vi_MDA_35 = NULL
vi_MDA_36 = NULL
vi_MDG_01 = NULL
vi_MDG_02 = NULL
vi_MDG_03 = NULL
vi_MDG_04 = NULL
vi_MDG_05 = NULL
vi_MDG_06 = NULL
vi_MDG_07 = NULL
vi_MDG_08 = NULL
vi_MDG_09 = NULL
vi_MDG_10 = NULL
vi_MDG_17 = NULL
vi_MDG_18 = NULL
vi_MDG_19 = NULL
vi_MDG_20 = NULL
vi_MDG_21 = NULL
vi_MDG_22 = NULL
vi_MDG_23 = NULL
vi_MDG_24 = NULL
vi_MDG_25 = NULL
vi_MDG_26 = NULL
vi_MDG_27 = NULL
vi_MDG_28 = NULL
vi_MDG_29 = NULL
vi_MDG_30 = NULL
vi_MDG_31 = NULL
vi_MDG_32 = NULL
vi_MDG_33 = NULL
vi_MDG_34 = NULL
vi_MDG_35 = NULL
vi_MDG_36 = NULL

## Function ##################################################################

foreach(i = seq(1:100)) %do% {
  
  train.rf <- randomForest(x = predictor_modisVAL,
                           y = response_speciesCLASS,
                           importance = TRUE,
                           ntree = 500,
                           #                          mtry = 5,
                           nodesize = 2,
                           type="classification",
                           do.trace = FALSE)
  
  
  variableImportance <- importance(train.rf)
  # confusion.matrix = train.rf$confusion
  
  ### Confusion Matrix ###########################################################
  conf.1.1[i] = train.rf$confusion[1,1]
  conf.1.2[i] = train.rf$confusion[1,2]
  conf.1.3[i] = train.rf$confusion[1,3]
  conf.2.1[i] = train.rf$confusion[2,1]
  conf.2.2[i] = train.rf$confusion[2,2]
  conf.2.3[i] = train.rf$confusion[2,3]
  
  ### Variable Importance - Mean Decrease Accuracy ###############################
  # vi_MeanDecreaseAccuracy <- variableImportance[,3]
  vi_MDA_01[i] <- variableImportance[1,3]
  vi_MDA_02[i] <- variableImportance[2,3]
  vi_MDA_03[i] <- variableImportance[3,3]
  vi_MDA_04[i] <- variableImportance[4,3]
  vi_MDA_05[i] <- variableImportance[5,3]
  vi_MDA_06[i] <- variableImportance[6,3]
  vi_MDA_07[i] <- variableImportance[7,3]
  vi_MDA_08[i] <- variableImportance[8,3]
  vi_MDA_09[i] <- variableImportance[9,3]
  vi_MDA_10[i] <- variableImportance[10,3]
  vi_MDA_17[i] <- variableImportance[11,3]
  vi_MDA_18[i] <- variableImportance[12,3]
  vi_MDA_19[i] <- variableImportance[13,3]
  vi_MDA_20[i] <- variableImportance[14,3]
  vi_MDA_21[i] <- variableImportance[15,3]
  vi_MDA_22[i] <- variableImportance[16,3]
  vi_MDA_23[i] <- variableImportance[17,3]
  vi_MDA_24[i] <- variableImportance[18,3]
  vi_MDA_25[i] <- variableImportance[19,3]
  vi_MDA_26[i] <- variableImportance[20,3]
  vi_MDA_27[i] <- variableImportance[21,3]
  vi_MDA_28[i] <- variableImportance[22,3]
  vi_MDA_29[i] <- variableImportance[23,3]
  vi_MDA_30[i] <- variableImportance[24,3]
  vi_MDA_31[i] <- variableImportance[25,3]
  vi_MDA_32[i] <- variableImportance[26,3]
  vi_MDA_33[i] <- variableImportance[27,3]
  vi_MDA_34[i] <- variableImportance[28,3]
  vi_MDA_35[i] <- variableImportance[29,3]
  vi_MDA_36[i] <- variableImportance[30,3]
  
  ### Variable Importance - Mean Decrease Gini ###################################
  vi_MeanDecreaseGini <- variableImportance[,4]
  vi_MDG_01[i] <- variableImportance[1,4]
  vi_MDG_02[i] <- variableImportance[2,4]
  vi_MDG_03[i] <- variableImportance[3,4]
  vi_MDG_04[i] <- variableImportance[4,4]
  vi_MDG_05[i] <- variableImportance[5,4]
  vi_MDG_06[i] <- variableImportance[6,4]
  vi_MDG_07[i] <- variableImportance[7,4]
  vi_MDG_08[i] <- variableImportance[8,4]
  vi_MDG_09[i] <- variableImportance[9,4]
  vi_MDG_10[i] <- variableImportance[10,4]
  vi_MDG_17[i] <- variableImportance[11,4]
  vi_MDG_18[i] <- variableImportance[12,4]
  vi_MDG_19[i] <- variableImportance[13,4]
  vi_MDG_20[i] <- variableImportance[14,4]
  vi_MDG_21[i] <- variableImportance[15,4]
  vi_MDG_22[i] <- variableImportance[16,4]
  vi_MDG_23[i] <- variableImportance[17,4]
  vi_MDG_24[i] <- variableImportance[18,4]
  vi_MDG_25[i] <- variableImportance[19,4]
  vi_MDG_26[i] <- variableImportance[20,4]
  vi_MDG_27[i] <- variableImportance[21,4]
  vi_MDG_28[i] <- variableImportance[22,4]
  vi_MDG_29[i] <- variableImportance[23,4]
  vi_MDG_30[i] <- variableImportance[24,4]
  vi_MDG_31[i] <- variableImportance[25,4]
  vi_MDG_32[i] <- variableImportance[26,4]
  vi_MDG_33[i] <- variableImportance[27,4]
  vi_MDG_34[i] <- variableImportance[28,4]
  vi_MDG_35[i] <- variableImportance[29,4]
  vi_MDG_36[i] <- variableImportance[30,4]
  
}

## Save mean confusion matrix and variable importance values into a dataframe
df.lvl0600 <- as.data.frame(c(colSums(tmp.species),
                              mean(conf.1.1), 
                              mean(conf.1.2),
                              mean(conf.1.3),
                              mean(conf.2.1), 
                              mean(conf.2.2),
                              mean(conf.2.3),
                              mean(vi_MDA_01),
                              mean(vi_MDA_02),
                              mean(vi_MDA_03),
                              mean(vi_MDA_04),
                              mean(vi_MDA_05),
                              mean(vi_MDA_06),
                              mean(vi_MDA_07),
                              mean(vi_MDA_08),
                              mean(vi_MDA_09),
                              mean(vi_MDA_10),
                              mean(vi_MDA_17),
                              mean(vi_MDA_18),
                              mean(vi_MDA_19),
                              mean(vi_MDA_20),
                              mean(vi_MDA_21),
                              mean(vi_MDA_22),
                              mean(vi_MDA_23),
                              mean(vi_MDA_24),
                              mean(vi_MDA_25),
                              mean(vi_MDA_26),
                              mean(vi_MDA_27),
                              mean(vi_MDA_28),
                              mean(vi_MDA_29),
                              mean(vi_MDA_30),
                              mean(vi_MDA_31),
                              mean(vi_MDA_32),
                              mean(vi_MDA_33),
                              mean(vi_MDA_34),
                              mean(vi_MDA_35),
                              mean(vi_MDA_36),
                              mean(vi_MDG_01),
                              mean(vi_MDG_02),
                              mean(vi_MDG_03),
                              mean(vi_MDG_04),
                              mean(vi_MDG_05),
                              mean(vi_MDG_06),
                              mean(vi_MDG_07),
                              mean(vi_MDG_08),
                              mean(vi_MDG_09),
                              mean(vi_MDG_10),
                              mean(vi_MDG_17),
                              mean(vi_MDG_18),
                              mean(vi_MDG_19),
                              mean(vi_MDG_20),
                              mean(vi_MDG_21),
                              mean(vi_MDG_22),
                              mean(vi_MDG_23),
                              mean(vi_MDG_24),
                              mean(vi_MDG_25),
                              mean(vi_MDG_26),
                              mean(vi_MDG_27),
                              mean(vi_MDG_28),
                              mean(vi_MDG_29),
                              mean(vi_MDG_30),
                              mean(vi_MDG_31),
                              mean(vi_MDG_32),
                              mean(vi_MDG_33),
                              mean(vi_MDG_34),
                              mean(vi_MDG_35),
                              mean(vi_MDG_36)))



## Set colnames (species)
names(df.lvl0600) <- s

# df.lvl0600
# 
# train.rf$confusion    

df.rownames <- data.frame(c("no.of.species",
                               "Actual=0, predict=0",
                               "Actual=0, predict=1",
                               "Class.error 0",
                               "Actual=1, predict=0",
                               "Actual=1, predict=1",
                               "Class.error 1",
                               "MeanDecreaseAccuracy_band01",
                               "MeanDecreaseAccuracy_band02",
                               "MeanDecreaseAccuracy_band03",
                               "MeanDecreaseAccuracy_band04",
                               "MeanDecreaseAccuracy_band05",
                               "MeanDecreaseAccuracy_band06",
                               "MeanDecreaseAccuracy_band07",
                               "MeanDecreaseAccuracy_band08",
                               "MeanDecreaseAccuracy_band09",
                               "MeanDecreaseAccuracy_band10",
                               "MeanDecreaseAccuracy_band17",
                               "MeanDecreaseAccuracy_band18",
                               "MeanDecreaseAccuracy_band19",
                               "MeanDecreaseAccuracy_band20",
                               "MeanDecreaseAccuracy_band21",
                               "MeanDecreaseAccuracy_band22",
                               "MeanDecreaseAccuracy_band23",
                               "MeanDecreaseAccuracy_band24",
                               "MeanDecreaseAccuracy_band25",
                               "MeanDecreaseAccuracy_band26",
                               "MeanDecreaseAccuracy_band27",
                               "MeanDecreaseAccuracy_band28",
                               "MeanDecreaseAccuracy_band29",
                               "MeanDecreaseAccuracy_band30",
                               "MeanDecreaseAccuracy_band31",
                               "MeanDecreaseAccuracy_band32",
                               "MeanDecreaseAccuracy_band33",
                               "MeanDecreaseAccuracy_band34",
                               "MeanDecreaseAccuracy_band35",
                               "MeanDecreaseAccuracy_band36",
                               "MeanDecreaseGini_band01",
                               "MeanDecreaseGini_band02",
                               "MeanDecreaseGini_band03",
                               "MeanDecreaseGini_band04",
                               "MeanDecreaseGini_band05",
                               "MeanDecreaseGini_band06",
                               "MeanDecreaseGini_band07",
                               "MeanDecreaseGini_band08",
                               "MeanDecreaseGini_band09",
                               "MeanDecreaseGini_band10",
                               "MeanDecreaseGini_band17",
                               "MeanDecreaseGini_band18",
                               "MeanDecreaseGini_band19",
                               "MeanDecreaseGini_band20",
                               "MeanDecreaseGini_band21",
                               "MeanDecreaseGini_band22",
                               "MeanDecreaseGini_band23",
                               "MeanDecreaseGini_band24",
                               "MeanDecreaseGini_band25",
                               "MeanDecreaseGini_band26",
                               "MeanDecreaseGini_band27",
                               "MeanDecreaseGini_band28",
                               "MeanDecreaseGini_band29",
                               "MeanDecreaseGini_band30",
                               "MeanDecreaseGini_band31",
                               "MeanDecreaseGini_band32",
                               "MeanDecreaseGini_band33",
                               "MeanDecreaseGini_band34",
                               "MeanDecreaseGini_band35",
                               "MeanDecreaseGini_band36"))
names(df.rownames) <- "confusion.matrix/variable.importance"

## Set rownames for new dataframe
df.lvl0600 <- cbind(df.rownames,
                    df.lvl0600)

file.out.confusion.species.10 <- "lvl0600_rf_prevalence_10_mean100_matrix_test.csv"

write.table(df.lvl0600, 
            file = file.out.confusion.species.10,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
