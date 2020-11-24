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
#   1.01      08/11/2020    Brian Nyathi      + Balancing of dataset
#   1.02      09/11/2020    Ryan Banks        Called functions for splitting the dataset with k-fold cross validation
# *************************************************************

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

KFOLDS           <- 5

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

# Write ML Models here




# ************************************************
# mars_GetPreprocessedDataset() :
#
# INPUT       :   printflag - optionally display results
#
# OUTPUT      :   Pre-processed dataset
#
# ************************************************
mars_GetPreprocessedDataset<-function(scaleflag = TRUE, printflag = FALSE){
  
  if(printflag){
    print("Inside main function")
  }

  # ************************************************
  # Read data from file
  dataset<-NreadDataset(DATASET_FILENAME)
  
  print(getMode(dataset$tenure))
  
  
  #*************************************************
  #Return all rows that have churned. There are 1869 rows
  y <-dataset[dataset$Churn == "Yes", ]
  #Return rows that not churned.
  n <- dataset[dataset$Churn == "No", ]
  #Shuffle these rows and select 1869 rows.
  set.seed(42)
  n_shuffled <- n[sample(1:nrow(n)), ]
  n_final <- n_shuffled[1: (nrow(y)), ] 
  #Return balanced and shuffled dataset
  yandn <- rbind(y, n_final)
  dataset <- yandn[sample(1:nrow(yandn)), ]
  #*************************************************  
  p<-ggplot(dataset, aes(x=tenure, y = TotalCharges)) + geom_point()
  print(p)
  
  
  # ************************************************
  # Remove customerID field because it is irrelevant
  dataset<-subset(dataset, select = -customerID)
  
  # ************************************************
  # Cast SeniorCitizen to string so that it is interpreted as categorical
  dataset$SeniorCitizen<-as.character(dataset$SeniorCitizen)
  
  # ************************************************
  # Set TotalCharges to zero where tenure is zero
  dataset[which(dataset$tenure==0),"TotalCharges"]<--0
  

  
  # ************************************************  
  # One-hot encoding special cases
  dataset<-PREPROCESSING_one_hot_special(dataset)
  
  
  # ************************************************
  #
  NPREPROCESSING_prettyDataset(dataset)
  
  # ************************************************
  # Determine if the field appears numeric or symbolic
  field_types<-NPREPROCESSING_initialFieldType(dataset)
  
  # ************************************************
  # View the field types on the console
  
  numeric_fields<-names(dataset)[field_types=="NUMERIC"]
  if(printflag){
    print(paste("NUMERIC FIELDS=",length(numeric_fields)))
    print(numeric_fields)
  }

  
  symbolic_fields<-names(dataset)[field_types=="SYMBOLIC"]
  
  if(printflag){
    print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
    print(symbolic_fields)
  }

  
  # ************************************************
  # Determine if the numeric fields might be discreet numeric
  
  field_types1<-NPREPROCESSING_discreetNumeric(dataset=dataset,
                                               field_types=field_types,
                                               cutoff=DISCREET_BINS)
  
  results<-data.frame(field=names(dataset),initial=field_types,types1=field_types1)
  
  if(printflag){
    print(formattable::formattable(results))
    
  }

  # ************************************************
  # This is a sub-set frame of just the ordinal fields
  ordinals<-dataset[,which(field_types1==TYPE_ORDINAL)]

  # Replace outlying ordinals with mean values
  ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)
  if(scaleflag==TRUE){
    # ************************************************
    # z-scale
    zscaled<-as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))
    
    # Scaled numeric input fields to [0.0,1.0]
    ordinalReadyforML<-Nrescaleentireframe(zscaled)
  }else{
    ordinalReadyforML<-ordinals
    
  }
  

  
  # ************************************************
  # Process the categorical (symbolic/discreet) fields using 1-hot-encoding
  catagoricalReadyforML<-NPREPROCESSING_categorical(dataset=dataset,field_types=field_types1)
  
  if(printflag){
    print(formattable::formattable(data.frame(fields=names(catagoricalReadyforML))))
    
  }

  # Number of non-numeric fields before transformation
  # Which fields are either SYMBOLIC or DISCREET
  nonNumericbefore<-length(which(field_types1!=TYPE_ORDINAL))
  
  # How many fields have be generated through the 1-hot-encoding process
  nonNumerictranformed<-ncol(catagoricalReadyforML)
  
  if(printflag){
    print(paste("Symbolic fields. Before encoding=",nonNumericbefore,"After",nonNumerictranformed))
    print(formattable::formattable(data.frame(field=1:nonNumerictranformed,encoded=names(catagoricalReadyforML))))
    
    
  }

  # ************************************************
  # Combine the two sets of data that are read for ML
  combinedML<-cbind(ordinalReadyforML,catagoricalReadyforML)
  
  # Remove redundant fields
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=OUTLIER_CONF)
  
  
  #**************** after the ml model is run, the mean of the results for each kfold should be returned **************
  
  
  # The dataset for ML information
  print(paste("Fields=",ncol(combinedML)))
  print("End")
  
  

  

  
  return(combinedML)
  
} #endof main()

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


