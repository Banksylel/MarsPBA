# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   LOGISTIC REGRESSION FUNCTIONS
#
#   
#   DATE:     11 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version
#   1.01      15/11/2020    Chris Jennings    Adopt k-folds
#   1.02      16/11/2020    Chris Jennings    Fixed bugs


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
               "PerformanceAnalytics")


DATASET_FILENAME  <- "telco-data.csv"          # Name of input dataset file


# ************************************************
# Name      :   main() :
# Purpose   :   Main entry point for logistic regression script
#
# INPUT     :   None
#
# OUTPUT    :   None
#
# ************************************************

main<-function(){
  ##Run Visualisation
  print("Running visualisations of the Telco dataset")
  visualiseDataset(DATASET_FILENAME)
  
  ##Run preprocessing
  print("Preprocess the dataset")
  fullDataset <-  mars_GetPreprocessedDataset(scaleflag=TRUE, printflag=TRUE)

  ##Split dataset into train/validation set and test set
  trainSamples <- round(nrow(fullDataset)*0.9)
  train <- fullDataset[1:trainSamples,]
  
  testSamples <- nrow(fullDataset)-trainSamples
  test <- fullDataset[(trainSamples+1):nrow(fullDataset),]
  test_expected <- test[,OUTPUT_FIELD]
  
  ##Run logistic regression evaluation
  print("Print logistic regression measures")
  lrResults <-  evaluateLogisticRegressionModel(train)
  NprintMeasures(lrResults, "Logistic Regression model results")
  
  ##Train a logistic regression model on the full train set and output final test measures
  lrModel <- createLogisticRegressionModel(train)
  lrPredictions <- lrPredict(lrModel,test)
  lrTestResults<-NdetermineThreshold(lrPredictions,test_expected,plot=FALSE)
  NprintMeasures(lrTestResults, "Logistic Regression model final test evaluation")
  
  
  ##Run random forest evaluation
  print("Print random forest measures")
  rfResults <-  evaluateRandomForestModel(train)
  NprintMeasures(rfResults, "Random Forest model results")
  
  ##Train a random forest model on the full train set and output final test measures
  rfModel <- createRandomForestModel(train)
  rfPredictions <- rfPredict(rfModel,test)
  rfTestResults<-NdetermineThreshold(rfPredictions,test_expected,plot=FALSE)
  NprintMeasures(rfTestResults, "Random forest model final test evaluation")
  
  
  
  ##Run neural network evaluation
  print("Print neural network measures")
  nnResults <-  evaluateNeuralNetworkModel(train)
  NprintMeasures(nnResults, "Neural Network model results")
  
  ##Train a neural network model on the full train set and output final test measures
  nnModel <- createNeuralNetworkModel(train)
  nnPredictions <- nnPredict(nnModel,test)
  nnTestResults<-NdetermineThreshold(nnPredictions,test_expected,plot=FALSE)
  NprintMeasures(nnTestResults, "Neural network model final test evaluation")
  
  
  
  ##Run ensemble model evaluation
  print("Print ensemble measures")
  ensembleResults <-  evaluateEnsembleModel(train)
  NprintMeasures(ensembleResults, "Ensemble model results")
  
  ##Train an ensemble model on the full train set and output final test measures
  ensembleModel <- createEnsembleModel(train)
  ensemblePredictions <- ensemblePredictMean(ensembleModel$lrModel, ensembleModel$rfModel, ensembleModel$nnModel,test)
  ensembleTestResults<-NdetermineThreshold(ensemblePredictions,test_expected,plot=FALSE)
  NprintMeasures(ensembleTestResults, "Ensemble model final test evaluation")
  
  
  ##Run clustering evaluation
  kMeansModel <-createKmeansModel(fullDataset)
  
  
  print("End")
  
  
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

set.seed(123)
# clears the console area
cat("\014")

print("MARS: PBA Project overview file")

# ************************************************
main()

print("end")
