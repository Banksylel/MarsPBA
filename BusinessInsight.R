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

KFOLDS           <- 5 # Number of folds to use in k-Fold validation

CUSTOMER_TENURE_FIELD <- 'tenure'            #Tenure field in dataset
MONTHLY_CHARGE_FIELD <- 'MonthlyCharges'     #Charge field by month


#Business assumptions
COST_TO_RETAIN <- 750                   #Cost to retain customers in dollars 
ENTICEMENT_PERCENTAGE <- 0.1            #Enticement percentage to offer (as decimal)
MINIMUM_ENTICEMENT_THRESHOLD <-  25     #Minimum monthly spend to offer enticement to


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
  #We need a scaled dataset for training and an unscaled dataset for cost calculations
  scaledDataset <-  mars_GetPreprocessedDataset(scaleflag=TRUE, printflag=FALSE)
  unscaledDataset <-  mars_GetPreprocessedDataset(scaleflag=FALSE, printflag=FALSE)
  
  #Check that tenure field exists in the loaded data
  if( !(CUSTOMER_TENURE_FIELD %in% colnames(scaledDataset)) || !(CUSTOMER_TENURE_FIELD %in% colnames(unscaledDataset)))
  {
    stop("The loaded dataset does not contain the specified tenure field, have you specified this correctly?");
  }
  
  #Check that the monthly charge field exists in the loaded data
  if( !(MONTHLY_CHARGE_FIELD %in% colnames(scaledDataset)) || !(MONTHLY_CHARGE_FIELD %in% colnames(unscaledDataset)))
  {
    stop("The loaded dataset does not contain the specified monthly charges field, have you specified this correctly?");
  }
  
  #Check that all assumptions are defined
  if(is.null(COST_TO_RETAIN) || is.null(ENTICEMENT_PERCENTAGE) ||is.null(MINIMUM_ENTICEMENT_THRESHOLD)){
    stop("One or more assumptions have not been set");
  }
  
  
  #Evalutate the ensemble model
  evaluateEnsembleModelROI(scaledDataset, unscaledDataset, COST_TO_RETAIN,ENTICEMENT_PERCENTAGE,MINIMUM_ENTICEMENT_THRESHOLD)
  
  
} #endof main()



# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

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
source("functions/mars/ensemble.R")
source("functions/mars/roi_functions.R")

set.seed(123)
# clears the console area
cat("\014")

print("MARS: PBA Project business insight file")

# ************************************************
main()


