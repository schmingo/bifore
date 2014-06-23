cat("\014")
################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## FURTHER PREVALENCE CALCULATIONS FOR ALL SPECIES                            ##
## EXECUTE RANDOM FOREST 100 TIMES AND CALCULATE MEAN VALUES FOR              ##
## CONFUSION MATRIX AND VARIABLE IMPORTANCE                                   ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-08                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest", "foreach", "doParallel")
lapply(lib, function(...) require(..., character.only = TRUE))

ncores <- detectCores()
# options(scipen = 10)

## set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/")
setwd("D:/Dropbox/Code/bifore/src/csv/kili/")

## Set filenames
file.in <- "lvl0400_rf_strat_prevalence_all.csv"
file.out.confusion <- "lvl0600_rf_prevalence_species-all_mean100_confusion.csv"
file.out.varimp.MDA <- "lvl0600_rf_prevalence_species-all_mean100_MDA.csv"
file.out.varimp.MDG <- "lvl0600_rf_prevalence_species-all_mean100_MDG.csv"

## Timekeeping
starttime <- Sys.time()


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2(file.in,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)

## Note: 1. Species with less than x observations in different plots removed
##       2. Stratified sampling (only 1 random observation per plot)


################################################################################
### Combining data for randomForest ############################################
################################################################################
set.seed(50)

## List species
names(data.raw)

lst.species <- names(data.raw[(which(names(data.raw)=="coordN")+1):(which(names(data.raw)=="greyval_band_1")-1)])
lst.species

## Remove species without any observations
index <- which(colSums(data.raw[, lst.species]) > 0) + 
  grep("coordN", names(data.raw))

data.raw <- data.frame(data.raw[, 1:grep("coordN", names(data.raw))], 
           data.raw[, index], 
           data.raw[, grep("greyval_band_1", names(data.raw))[1]:ncol(data.raw)])

lst.species <- names(data.raw[(which(names(data.raw)=="coordN")+1):(which(names(data.raw)=="greyval_band_1")-1)])

## Split incoming dataset
df.greyval <- data.raw[(which(names(data.raw)=="greyval_band_1")):(which(names(data.raw)=="greyval_band_36"))]

## Calculate randomForest for all Species
registerDoParallel(cl <- makeCluster(ncores))

df.species.lvl0600 <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %dopar% {
  
  
  s <- lst.species[3]
  set.seed(50)
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
  set.seed(50)
  foreach(i = seq(1)) %do% {
    
    train.rf <- randomForest(x = predictor_modisVAL,
                             y = response_speciesCLASS,
                             importance = TRUE,
                             ntree = 500,
                             # mtry = 5,
                             nodesize = 2,
                             type="classification",
                             do.trace = FALSE)
    
    train.rf$confusion
    train.rf
    plot(train.rf)
    
    variableImportance <- importance(train.rf)
    
    ### Confusion Matrix #######################################################
    conf.1.1[i] = train.rf$confusion[1,1]
    conf.1.2[i] = train.rf$confusion[1,2]
    conf.1.3[i] = train.rf$confusion[1,3]
    conf.2.1[i] = train.rf$confusion[2,1]
    conf.2.2[i] = train.rf$confusion[2,2]
    conf.2.3[i] = train.rf$confusion[2,3]
    
    ### Variable Importance - Mean Decrease Accuracy ###########################
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
    
    ### Variable Importance - Mean Decrease Gini ###############################
    # vi_MeanDecreaseGini <- variableImportance[,4]
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
  
  #   ## Get mean values
  #   mean(conf.1.1)
  #   mean(conf.1.2)
  #   mean(conf.1.3)
  #   mean(conf.2.1)
  #   mean(conf.2.2)
  #   mean(conf.2.3)
  
  ## Save mean confusion matrix and variable importance values into a dataframe
  df.species.lvl0600 <- as.data.frame(c(colSums(tmp.species),
                                        mean(conf.1.1), 
                                        mean(conf.1.2),
                                        mean(conf.2.1), 
                                        mean(conf.2.2),
                                        mean(conf.1.3),
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
  names(df.species.lvl0600) <- s
  return(df.species.lvl0600)
  
}


## Close parallel backend
stopCluster(cl)


## Transpose df
df.species.lvl0600 <- as.data.frame(t(df.species.lvl0600))

## Write species into first row
df.species.lvl0600 <- cbind(rownames(df.species.lvl0600), df.species.lvl0600)

## Remove rownames
row.names(df.species.lvl0600) <- NULL

## Define colnames
names(df.species.lvl0600) <- c("species",
                               "no.of.prevalence",
                               "O0_P0",
                               "O0_P1",
                               "O1_P0",
                               "O1_P1",
                               "Class.error 0",
                               "Class.error 1",
                               "MDA_band01",
                               "MDA_band02",
                               "MDA_band03",
                               "MDA_band04",
                               "MDA_band05",
                               "MDA_band06",
                               "MDA_band07",
                               "MDA_band08",
                               "MDA_band09",
                               "MDA_band10",
                               "MDA_band17",
                               "MDA_band18",
                               "MDA_band19",
                               "MDA_band20",
                               "MDA_band21",
                               "MDA_band22",
                               "MDA_band23",
                               "MDA_band24",
                               "MDA_band25",
                               "MDA_band26",
                               "MDA_band27",
                               "MDA_band28",
                               "MDA_band29",
                               "MDA_band30",
                               "MDA_band31",
                               "MDA_band32",
                               "MDA_band33",
                               "MDA_band34",
                               "MDA_band35",
                               "MDA_band36",
                               "MDG_band01",
                               "MDG_band02",
                               "MDG_band03",
                               "MDG_band04",
                               "MDG_band05",
                               "MDG_band06",
                               "MDG_band07",
                               "MDG_band08",
                               "MDG_band09",
                               "MDG_band10",
                               "MDG_band17",
                               "MDG_band18",
                               "MDG_band19",
                               "MDG_band20",
                               "MDG_band21",
                               "MDG_band22",
                               "MDG_band23",
                               "MDG_band24",
                               "MDG_band25",
                               "MDG_band26",
                               "MDG_band27",
                               "MDG_band28",
                               "MDG_band29",
                               "MDG_band30",
                               "MDG_band31",
                               "MDG_band32",
                               "MDG_band33",
                               "MDG_band34",
                               "MDG_band35",
                               "MDG_band36")


## Order dataset by no.of.prevalence (descending)
attach(df.species.lvl0600)
df.species.lvl0600 <- df.species.lvl0600[order(-no.of.prevalence),]
detach(df.species.lvl0600)

## Split data into 3 separate dataframes - confusion matrix & variable importance (MDA + MDG)

df.out.confusion <- df.species.lvl0600[1:8]
df.out.varimp.MDA <- cbind(df.species.lvl0600[1:2], df.species.lvl0600[9:38])
df.out.varimp.MDG <- cbind(df.species.lvl0600[1:2], df.species.lvl0600[39:68])


################################################################################
### Further Confusion Matrix calculations ######################################
################################################################################
attach(df.out.confusion)

## Sum observed 0
df.out.confusion$sum.O0 <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
  sum.tmp <- df.out.confusion[i,"O0_P0"] + df.out.confusion[i,"O0_P1"]
  return(sum.tmp)
}

## Sum observed 1
df.out.confusion$sum.O1 <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
  sum.tmp <- df.out.confusion[i,"O1_P0"] + df.out.confusion[i,"O1_P1"]
  return(sum.tmp)
}

## Sum predicted 0
df.out.confusion$sum.P0 <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
  sum.tmp <- df.out.confusion[i,"O0_P0"] + df.out.confusion[i,"O1_P0"]
  return(sum.tmp)
}

## Sum predicted 1
df.out.confusion$sum.P1 <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
  sum.tmp <- df.out.confusion[i,"O0_P1"] + df.out.confusion[i,"O1_P1"]
  return(sum.tmp)
}

## POD (Probability of detection)
df.out.confusion$POD <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
  POD.tmp <- (df.out.confusion[i,"O1_P1"]) / (df.out.confusion[i,"sum.P1"])
  return(POD.tmp)
}

## FAR (False alarm ratio)
df.out.confusion$FAR <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
  FAR.tmp <- (df.out.confusion[i,"O0_P1"]) / (df.out.confusion[i,"sum.P1"])
  return(FAR.tmp)
}

## CSI (Critical success index)
df.out.confusion$CSI <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
  CSI.tmp <- (df.out.confusion[i,"O1_P1"]) / (df.out.confusion[i,"O1_P1"] + df.out.confusion[i,"O1_P0"] + df.out.confusion[i,"O0_P1"])
  return(CSI.tmp)
}

## POFD (Probability of false detection)
df.out.confusion$POFD <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
    POFD.tmp <- (df.out.confusion[i,"O0_P1"]) / (df.out.confusion[i,"sum.O0"])
  return(POFD.tmp)
}

## Kappa
df.out.confusion$kappa <- foreach(i=seq(1:nrow(df.out.confusion)), .combine="rbind") %do% {
  kappa.tmp <- ((df.out.confusion[i, "sum.P1"]) *
                  (df.out.confusion[i, "sum.O1"]) + 
                  (df.out.confusion[i, "sum.O0"]) +
                  (df.out.confusion[i, "sum.P0"])) /
    ((df.out.confusion[i, "sum.P0"]) + (df.out.confusion[i, "sum.P1"]))^2
  return(kappa.tmp)
}

detach(df.out.confusion)

################################################################################

## Timekeeping
endtime <- Sys.time()
time <- endtime - starttime
time


write.csv2(df.out.confusion, 
           file = file.out.confusion,
           quote = FALSE,
           row.names = FALSE)

write.csv2(df.out.varimp.MDA, 
           file = file.out.varimp.MDA,
           quote = FALSE,
           row.names = FALSE)

write.csv2(df.out.varimp.MDG, 
           file = file.out.varimp.MDG,
           quote = FALSE,
           row.names = FALSE)



## Get probabilities for ROC-curve 
??predict.randomForest
