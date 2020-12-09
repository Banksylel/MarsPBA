# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#   
#   DATE:     06 December 2020
#   VERSION:  V1.01
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      03/12/2020    Chris Endacott    Initial Version
#   1.01      06/12/2020    Chris Endacott    Modified ROI calculations
# ************************************************


# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4


#  clears all objects in "global environment"
rm(list=ls())

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "caret",
               "stringr",
               "PerformanceAnalytics",
               "ggplot2",
               "reshape")


DATASET_FILENAME  <- "telco-data.csv"          # Name of input dataset file

#In dollars
COST_TO_RETAIN <- 750

# ************************************************
# Name      :   main() :
# Purpose   :   Main entry point for business insight
#
# INPUT     :   None
#
# OUTPUT    :   None
#
# ************************************************

main<-function(){
  
  fullDataset <-  NreadDataset(DATASET_FILENAME)
  ##Run preprocessing
  print("Preprocess the dataset")
  preProcessedDataSet <-  mars_GetPreprocessedDataset(scaleflag=TRUE, printflag=FALSE)
  # ##Run ensemble model evaluation
  print("Print ensemble measures")
  
  measures <-  evaluateEnsembleModel(preProcessedDataSet)
  NprintMeasures(measures, "Ensemble model results")
  ##Load the ensemble model trained on all data
  ensembleModel <- loadModelFromFile("EnsembleModel")
  
  numChurned <- nrow(fullDataset[which(fullDataset$Churn=="Yes"),])
  churnRate <-  numChurned/nrow(fullDataset)
  print(measures$TP)
  print(measures$FP)
  print(measures$TN)
  print(measures$FN)
  
  print("end")
  
} #endof main()




# ************************************************
# This is where R starts execution


# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#This [optionally] sets working directory
#setwd("")

#Load additional R script files 
source("functions/mars/data_pre_processing_pipeline.R")
source("functions/mars/data_pre_processing_functions.R")
source("functions/nick/4labfunctions.R")
source("functions/nick/lab4DataPrepNew.R")
source("functions/nick/lab3DataPrep.R")
source("functions/mars/utility_functions.R")
source("Visualisation_UPDATED.R")
source("functions/mars/logistic_regression_functions.R")
source("functions/mars/random_forest_functions.R")
source("functions/mars/neural_network_functions.R")
source("functions/mars/ensemble.R")
source("functions/mars/clustering_functions.R")

set.seed(123)
# clears the console area
cat("\014")

print("MARS: PBA Project overview file")

# ************************************************
main()


