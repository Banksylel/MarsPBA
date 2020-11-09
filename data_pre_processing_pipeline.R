# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#   DATA PRE-PROCESSING PIPELINE
#
#   
#   DATE:     08 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      08/11/2020    Chris Jennings    Initial Version
# *************************************************************

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************
#GLOBALS

DATASET_FILENAME  <- "telco-data.csv"     # Name of input dataset file
OUTPUT_FIELD      <- "Churn"              # Field name of the output class to predict

SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.9                  # Confidence p-value for outlier detection
                                          # Set to negative means analyse but do not replace outliers

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

DISCREET_BINS     <- 5                    # Number of empty bins to determine discreet
MAX_LITERALS      <- 55                   # Maximum number of hotcoding new fields

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

# User defined functions



# ************************************************
# main() :
# Main entry point
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# ************************************************
main<-function(){

  print("Inside main function")

  # ************************************************
  # Read data from file
  telco<-NreadDataset(DATASET_FILENAME)
  
  #*************************************************
  #Return all rows that have churned. There are 1869 rows
  y <-churn[churn$Churn == "Yes", ]
  #Return rows that not churned.
  n <- churn[churn$Churn == "No", ]
  #Shuffle these rows and select 1869 rows.
  set.seed(42)
  n_shuffled <- n[sample(1:nrow(n)), ]
  n_final <- n_shuffled[1:1869, ]  
  #Return balanced dataset
  yandn <- rbind(y, n_final)
  telco <- yandn[sample(1:nrow(yandn)), ]
  
  # ************************************************
  # Remove customerID field because it is irrelevant
  telco<-subset(telco, select = -customerID)
  
  # ************************************************
  # Cast SeniorCitizen to string so that it is interpreted as categorical
  telco$SeniorCitizen<-as.character(telco$SeniorCitizen)

  # ************************************************
  # Set TotalCharges to zero where tenure is zero
  telco[which(telco$tenure==0),"TotalCharges"]<--0
  
  # ************************************************  
  # One-hot encoding special cases
  telco<-PREPROCESSING_one_hot_special(telco)
  

  # ************************************************
  #
  NPREPROCESSING_prettyDataset(telco)

  # ************************************************
  # Determine if the field appears numeric or symbolic
  field_types<-NPREPROCESSING_initialFieldType(telco)

  # ************************************************
  # View the field types on the console

  numeric_fields<-names(telco)[field_types=="NUMERIC"]
  print(paste("NUMERIC FIELDS=",length(numeric_fields)))
  print(numeric_fields)

  symbolic_fields<-names(telco)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)

  # ************************************************
  # Determine if the numeric fields might be discreet numeric

  field_types1<-NPREPROCESSING_discreetNumeric(dataset=telco,
                                               field_types=field_types,
                                               cutoff=DISCREET_BINS)

  results<-data.frame(field=names(telco),initial=field_types,types1=field_types1)
  print(formattable::formattable(results))

  # ************************************************
  # This is a sub-set frame of just the ordinal fields
  ordinals<-telco[,which(field_types1==TYPE_ORDINAL)]

  # Replace outlying ordinals with mean values
  ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)

  # ************************************************
  # z-scale
  zscaled<-as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))

  # Scaled numeric input fields to [0.0,1.0]
  ordinalReadyforML<-Nrescaleentireframe(zscaled)

  # ************************************************
  # Process the categorical (symbolic/discreet) fields using 1-hot-encoding
  catagoricalReadyforML<-NPREPROCESSING_categorical(dataset=telco,field_types=field_types1)

  print(formattable::formattable(data.frame(fields=names(catagoricalReadyforML))))

  # Number of non-numeric fields before transformation
  # Which fields are either SYMBOLIC or DISCREET
  nonNumericbefore<-length(which(field_types1!=TYPE_ORDINAL))

  # How many fields have be generated through the 1-hot-encoding process
  nonNumerictranformed<-ncol(catagoricalReadyforML)
  print(paste("Symbolic fields. Before encoding=",nonNumericbefore,"After",nonNumerictranformed))

  # Output the names of the encoded fields (literals)
  print(formattable::formattable(data.frame(field=1:nonNumerictranformed,encoded=names(catagoricalReadyforML))))

  # ************************************************
  # Combine the two sets of data that are read for ML
  combinedML<-cbind(ordinalReadyforML,catagoricalReadyforML)

  # Remove redundant fields
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=OUTLIER_CONF)
  
  # The dataset for ML information
  print(paste("Fields=",ncol(combinedML)))
  
  print("End")

} #endof main()

# ************************************************
# This is where R starts execution

# Automatically release memory
gc()

# Clear plots and other graphics in RStudio output
if(!is.null(dev.list())) dev.off()
graphics.off()

# Clear all warning messages
assign("last.warning", NULL, envir = baseenv())

# Clears the RStudio console area
cat("\014")

# If library not already on your computer this will download and
# install the library. Each library is activated.
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


#Source functions
source("lab3dataPrep.R")    # From Prof Nick's lab
source("lab2functions.R")   # From Prof Nick's lab
source("data_pre_processing_functions.R")

# Reset the pseudo-random number generator to start at the same point
set.seed(123)

print("PBA TEAM MARS: DATA PRE-PROCESSING PIPELINE")

# ************************************************
# Call the main function
main()

print("end")

