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

print("Sourcing logistic_regression_functions.R")

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



getLRClassifications<-function(trainedModel,
                                 testDataset,
                                 title="",
                                 classLabel=1,
                                 plot=FALSE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(trainedModel,test_inputs, type="response")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs#[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  # if (plot==TRUE)
  #   NprintMeasures(results=measures,title=title)
  
  return(measures)
} #endof getLRClassifications()





logisticRegression <- function(){
  print("Begin logistic regression model")
  
  dataset <- mars_GetPreprocessedDataset(FALSE)

  
  # Create a TRAINING dataset using first HOLDOUT% of the records
  # and the remaining 30% is used as TEST
  # use ALL fields (columns)
  dataset<-dataset[sample(nrow(dataset)),]
  training_records<-round(nrow(dataset)*(70/100))
  training_data <- dataset[1:training_records,]
  testing_data = dataset[-(1:training_records),]
  
  # First pass - determine which features impact prediction
  formular<-myModelFormula(dataset = training_data, fieldNameOutput = OUTPUT_FIELD)
  logr<-stats::glm(formular,data=training_data,family=quasibinomial)
  
  # Exclude "NA" coefficients - avoids rank deficiency
  coefs<-names(which(!is.na(logr$coefficients)))
  coefs<-coefs[2:length(coefs)]
  
  # Reconstruct model formula
  formular<-paste(OUTPUT_FIELD, "~", paste(coefs, collapse = "+"))
  
  # Re-train model with redcued feature list
  logr<-stats::glm(formular,data=training_data,family=quasibinomial)
  
  
  # ************************************************
  # Use the trained model with the test dataset
  measures<-getLRClassifications(trainedModel = logr,
                                   testDataset = testing_data,
                                   title=myTitle,
                                   plot=FALSE)
  
  # if (plot==TRUE){
  #   # Get importance of the input fields
  #   importance<-randomForest::importance(rf,scale=TRUE,type=1)
  #   importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
  #   
  #   colnames(importance)<-"Strength"
  #   
  #   barplot(t(importance),las=2, border = 0,
  #           cex.names =0.7,
  #           main=myTitle)
  #   
  #   print(formattable::formattable(data.frame(importance)))
  # }
  print(measures)
  return(measures)
  
  print("End logistic regression model")  
}








# ************************************************
# This is where R starts execution

# clears the console area
cat("\014")

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

set.seed(123)

print("MARS: LOGISTIC REGRESSION MODEL")

# ************************************************
logisticRegression()

print("end")

