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
  dataset[which(dataset$PhoneService=="Yes"),"PhoneService_SingleLine"]<-"Yes"
  dataset[which(dataset$MultipleLines=="Yes"),"PhoneService_MultipleLines"]<-"Yes"
  dataset[which(dataset$MultipleLines=="Yes"),"PhoneService_SingleLine"]<-"No"
  
  # Customers with internet have InternetService_DSL=1 xor InternetService_Fiber=1.
  # Both fields set to zero when customer has no internet service.
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

