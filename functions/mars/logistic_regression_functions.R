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

# ************************************************
# Name      :   getLRClassifications() :
# Purpose   :   Determine "measures" when using optimal threshold
#
# INPUT     :   glm object - trainedModel
#           :   data frame - testDataset
#           :   Text - Optional title
#           :   Boolean - Enable plots
#
# OUTPUT    :   measures - model performance metrics
#
# ************************************************

getLRClassifications<-function(trainedModel,
                                 testDataset,
                                 title="",
                                 plot=FALSE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Get probabilities of being class 1 from the classifier
  test_predictedProbs<-predict(trainedModel,test_inputs, type="response")


  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)

  return(measures)
} #endof getLRClassifications()


# ************************************************
# Name      :   logisticRegression() :
# Purpose   :   Train logistic regression model
#
# INPUT     :   data frame - training_data
#           :   data frame - testing_data
#           :   Boolean - plot - Enable plots
#
# OUTPUT    :   measures - model performance metrics
#
# ************************************************

logisticRegression <- function(training_data,testing_data, formular=formular, plot=TRUE, ...){
  print("Begin logistic regression model")

  # Train model with reduced feature list
  logr<-stats::glm(formular,data=training_data,family=quasibinomial)

  
  # ************************************************
  # Use the trained model with the test dataset
  measures<-getLRClassifications(trainedModel = logr,
                                   testDataset = testing_data,
                                   title=myTitle,
                                   plot=FALSE)
  
  
  
  
  print("End logistic regression model") 
  return(measures)
} #endof logisticRegression()



# ************************************************
# Name      :   reduceFeatures() :
# Purpose   :   Train logistic regression model on entire dataset to
#           :   determine feature importance.
#               Remove non-contributing features and generate model formula.
#               Plot importance chart.
#
# INPUT     :   data frame - dataset
#
# OUTPUT    :   Model formula with reduced feature set.
#
# ************************************************

reduceFeatures<-function(dataset) {
  
  # Determine importance of features
  formular<-myModelFormula(dataset = dataset, fieldNameOutput = OUTPUT_FIELD)
  logr<-stats::glm(formular,data=dataset,family=quasibinomial)
  
  # Plot feature importance chart
  importance<-as.data.frame(caret::varImp(logr, scale = TRUE))
  row.names(importance)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance))
  barplot(t(importance[order(-importance$Overall),,drop=FALSE]),
          las=2, 
          border = 3, 
          cex.names = 0.8, 
          legend.text = "Logistic regression feature importance")
  
  # Exclude features of no importance - avoids rank deficiency issues
  importance<-as.data.frame(caret::varImp(logr, scale = TRUE))
  features<-data.frame(gsub("[[:blank:]]+", "", row.names(importance)), importance$Overall)
  colnames(features)<-c("Feature", "Overall")
  features<-features[order(-importance$Overall),]
  
  # Reconstruct model formula
  formular<-paste(OUTPUT_FIELD, "~", paste(features$Feature, collapse = "+"))
  
  return(formular)
  
}


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
  
  # Acquire pre-processed data
  dataset <- mars_GetPreprocessedDataset(TRUE)
  
  # Remove redundant features from model
  formular<-reduceFeatures(dataset)
  
  # RUn k-folds
  results <-  kfold(dataset, 5, logisticRegression, formular)

  # Print k-folds measures means
  NprintMeasures(results)

} #endof main()




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
main()

print("end")

