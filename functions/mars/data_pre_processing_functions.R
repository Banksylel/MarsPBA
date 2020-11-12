# ************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#   DATA PRE-PROCESSING FUNCTIONS
#
#   
#   DATE:     08 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      08/11/2020    Chris Jennings    Initial Version
#   1.10      09/11/2020    Ryan Banks        Added PREPROCESSING_stratDataset, PREPROCESSING_trainTestSplit, PREPROCESSING_stratSplit, and  PREPROCESSING_FoldID functions 
# # ***********************************************************

# ************************************************
# PREPROCESSING_one_hot_special() :
#
# One-hot encoding special cases
#
# INPUT: data Frame - dataset
#
# OUTPUT : data Frame - Encoded dataset
# ************************************************
PREPROCESSING_one_hot_special<-function(dataset){
  
  # Create new columns with all values initially 0
  dataset<-cbind(dataset, data.frame("PhoneService_SingleLine" = "No",
                                     "PhoneService_MultipleLines" = "No",
                                     "PhoneService_NoPhoneService" = "No",
                                     "InternetService_NoInternetService" = "No",
                                     "InternetService_DSL" = "No",
                                     "InternetService_Fiber" = "No",
                                     "InternetService_OnlineSecurity" = "No",
                                     "InternetService_OnlineBackup"= "No",
                                     "InternetService_DeviceProtection"= "No",
                                     "InternetService_TechSupport" = "No",
                                     "InternetService_StreamingTV" = "No",
                                     "InternetService_StreamingMovies" = "No",
                                     "PaymentMethod_Automatic" = "No"))
  
  
  # Customers with phone service have PhoneService_SingleLine=1 xor PhoneService_MultipleLines=1.
  # Both fields set to zero when customer has no phone service.
  dataset[which(dataset$MultipleLines=="No"),"PhoneService_SingleLine"]<-"Yes"
  dataset[which(dataset$MultipleLines=="Yes"),"PhoneService_MultipleLines"]<-"Yes"
  dataset[which(dataset$MultipleLines=="No phone service"),"PhoneService_NoPhoneService"]<-"Yes"
  
  # Customers with internet have InternetService_DSL=1 xor InternetService_Fiber=1.
  # Both fields set to zero when customer has no internet service.
  dataset[which(dataset$InternetService=="No"),"InternetService_NoInternetService"]<-"Yes"
  dataset[which(dataset$InternetService=="DSL"),"InternetService_DSL"]<-"Yes"
  dataset[which(dataset$InternetService=="Fiber optic"),"InternetService_Fiber"]<-"Yes"
  
  # One-hot encode internet service fields and rename them using standard convention
  dataset[which(dataset$OnlineSecurity=="Yes"),"InternetService_OnlineSecurity"]<-"Yes"
  dataset[which(dataset$OnlineBackup=="Yes"),"InternetService_OnlineBackup"]<-"Yes"
  dataset[which(dataset$DeviceProtection=="Yes"),"InternetService_DeviceProtection"]<-"Yes"
  dataset[which(dataset$TechSupport=="Yes"),"InternetService_TechSupport"]<-"Yes"
  dataset[which(dataset$StreamingTV=="Yes"),"InternetService_StreamingTV"]<-"Yes"
  dataset[which(dataset$StreamingMovies=="Yes"),"InternetService_StreamingMovies"]<-"Yes"
  
  # PaymentMethod values encoded as automatic or manual
  dataset[which(str_detect(dataset$PaymentMethod, "automatic")),"PaymentMethod_Automatic"]<-"Yes"
  
  # Remove now-redundant fields
  dataset<-subset(dataset, select = -c(PhoneService, 
                                       MultipleLines, 
                                       InternetService,
                                       OnlineSecurity,
                                       OnlineBackup,
                                       DeviceProtection,
                                       TechSupport,
                                       StreamingTV,
                                       StreamingMovies,
                                       PaymentMethod
  ))
  
  
  # Return output field to last column
  idx <- grep(OUTPUT_FIELD, names(dataset))
  dataset <- dataset[, c((1:ncol(dataset))[-idx], idx)]
  
  return (dataset)
}




# ************************************************
# PREPROCESSING_splitDataset() :
#
# A function that randomizes the dataset and splits it into train and test using k fold cross validation
#
# INPUT: data Frame - dataset
#
# OUTPUT : data Frame - Encoded dataset
# ************************************************
PREPROCESSING_stratDataset <- function(dataset, KFOLDS){
  
  
  classOutput=which(names(dataset)==OUTPUT_FIELD)               
  classes <- unique(dataset[,classOutput])                                      #this gets the unique class values
  #this Splits the dataset into 2 classes to keep the class balance the same as in the dataset
  indexClass1<-which(dataset[,classOutput]==classes[1])
  split1 <- dataset[indexClass1,]
  split2 <- dataset[-indexClass1,]
  #this calls the PREPROCESSING_FoldID function below
  split1 <- PREPROCESSING_FoldID(split1, KFOLDS)
  split2 <- PREPROCESSING_FoldID(split2, KFOLDS)
  
  newDataset <- rbind(split1,split2)                                            #this Combines the two datasets
  newDataset <- newDataset[order(runif(nrow(newDataset))),]                     #this randomises the classes
  
  return(newDataset)
  
}


# ************************************************
# PREPROCESSING_FoldID() :
#
# adds a column called "foldID" to the dataset that indicates the fold number
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
PREPROCESSING_FoldID <- function(dataset, KFOLDS){
  
  kRecords <- ceiling(nrow(dataset)/KFOLDS)                                       #this devides the number of rows in the dataset by the pre dertermined kFold number, then rounds up to the nearest int
  foldIds <- rep(seq(1:KFOLDS),kRecords)                                          #this creates an Id numbers for each k fold for the size of the records that appear in each fold
  foldIds <- foldIds[1:nrow(dataset)]                                             #this cuts foldIds so it will fit into the dataset dataframe, incase the rounding earier made it too large
  dataset$foldId<-foldIds                                                       #This assignes the fold column to the dataset
  
  return(dataset)
  
  
}


# ************************************************
# PREPROCESSING_stratSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************
PREPROCESSING_stratSplit <- function(dataset, kfold){
  
  test<-subset(dataset, subset= foldId==kfold, select=-foldId)
  train<-subset(dataset, subset= foldId!=kfold,select=-foldId)
  
  return(list(
    train=train,
    test=test))
}



