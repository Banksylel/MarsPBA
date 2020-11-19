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


MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "caret",
               "stringr",
               "PerformanceAnalytics")

# ************************************************
# Name      :   getLRClassifications() :
# Purpose   :   Determine "measures" when using optimal threshold
#
# INPUT     :   glm object    trainedModel
#           :   data frame    testDataset
#           :   Text          Optional title
#           :   Boolean       Enable plots
#
# OUTPUT    :   measures - model performance metrics
#
# ************************************************
# ************************************************
# Name      :   main() :
# Purpose   :   Main entry point for logistic regression script
#
# INPUT     :   None
#
# OUTPUT    :   None
#
# ************************************************



ensemble <- function(train,test){
  
  ensembleModel <- createEnsembleModel(train)
  
  ensemblePredictions <-  ensemblePredictMean(ensembleModel$lrModel, ensembleModel$rfModel, ensembleModel$nnModel, test)
  #ensemblePredictions <-  ensemblePredictVote(ensembleModel$lrModel, ensembleModel$rfModel, ensembleModel$nnModel, test)
  
  test_expected <- test[,OUTPUT_FIELD]
  results<-NcalcConfusion(expectedClass=test_expected,
                          predictedClass=ensemblePredictions)
  results$threshold <- NA
  results$AUC<-auroc(score=ensemblePredictions,bool=test_expected) # Estimate the AUC
  
  return(results)
}




createEnsembleModel <- function(train){
  
  logisticRegressionModel <-  createLogisticRegressionModel(train)
  print("Logistic Regression model trained")

  randomForestModel <-  createRandomForestModel(train)
  print("Random Forest model trained")
  
  neuralNetworkModel <-  createNeuralNetworkModel(train)
  print("Neural Network model trained")
  
  ensembleModel <-  list("lrModel" = logisticRegressionModel, "rfModel" = randomForestModel, "nnModel" = neuralNetworkModel)
  
  return(ensembleModel)
  
}

ensemblePredictVote <- function(lrModel, rfModel, nnModel, test){
  test_expected <- test[,OUTPUT_FIELD]
  
  logrPredictions <- lrPredict(lrModel, test)
  logrThreshold <- NdetermineThreshold(logrPredictions, test_expected)$threshold
  logrVotes<-ifelse(logrPredictions<logrThreshold,0,1)
  
  rfPredictions <- rfPredict(rfModel, test)
  rfThreshold <- NdetermineThreshold(rfPredictions, test_expected)$threshold
  rfVotes<-ifelse(rfPredictions<rfThreshold,0,1)
  
  nnPredictions <-  nnPredict(nnModel,test)
  nnThreshold <- NdetermineThreshold(nnPredictions, test_expected)$threshold
  nnVotes <-ifelse(nnPredictions<nnThreshold,0,1)
  
  ensemblePredictions <- vector()
  
  for(i in 1:nrow(test)){
    #Majority vote needs more than 1
    if(sum(nnVotes[i],rfVotes[i], logrVotes[i])>1.5){
      ensemblePredictions <-  append(ensemblePredictions, 1)
    }else{
      ensemblePredictions <-  append(ensemblePredictions, 0)
      
    }
    
  }
  return(ensemblePredictions)
}


ensemblePredictMean <- function(lrModel, rfModel, nnModel, test){
  test_expected <- test[,OUTPUT_FIELD]

  logrPredictions <- lrPredict(lrModel, test)
  rfPredictions <- rfPredict(rfModel, test)
  nnPredictions <-  nnPredict(nnModel,test)

  
  ensemblePredictions <- vector()
  
  for(i in 1:nrow(test)){
    meanPrediction <-  (logrPredictions[i]+rfPredictions[i]+nnPredictions[i])/3
    ensemblePredictions <-  append(ensemblePredictions,meanPrediction)
    
  }
  
  ensembleThreshold <- NdetermineThreshold(ensemblePredictions, test_expected)$threshold
  ensemblePredictions <-ifelse(ensemblePredictions<ensembleThreshold,0,1)
  
  return(ensemblePredictions)
}


nnPredict <- function(model, test){
  test_expected<-test[,OUTPUT_FIELD]
  test_h2o <- as.h2o(test, destination_frame = "testdata")

  pred <- h2o::h2o.predict(model, test_h2o)

  test_predicted<-as.vector(pred$p1)  #Returns the probabilities of class 1
  return(test_predicted)
}

rfPredict <- function(model, test){
  positionClassOutput=which(names(test)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-test[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(model,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==1)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  return(test_predictedProbs)
  
}

lrPredict <- function(model, test){
  positionClassOutput=which(names(test)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-test[-positionClassOutput]
  
  # Get probabilities of being class 1 from the classifier
  test_predictedProbs<-predict(model,test_inputs, type="response")
  
  return(test_predictedProbs)
}

evaluateEnsembleModel <- function(dataset){

  results <-  kfold(dataset, 5, ensemble)
  
  return(results)
  
}



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
source("functions/mars/logistic_regression_functions.R")
source("functions/mars/random_forest_functions.R")
source("functions/mars/neural_network_functions.R")


