# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   NEURAL NETWORK FUNCTIONS
#
#   
#   DATE:     11 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version

DEEP_HIDDEN       <- c(5,5)               # Number of neurons in each layer
DEEP_STOPPING     <- 3                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.015                 # Error threshold
DEEP_ACTIVATION   <- "TanhWithDropout"    # Non-linear activation function
DEEP_REPRODUCABLE <- TRUE                 # Set to TRUE to test training is same for each run
BASICNN_EPOCHS = 350

HIDDEN_TEST = "Hidden Nodes"
STOPPING_TEST = "Stopping threshold"
ACTIVATION_TEST = "Activation function"
TOLERANCE_TEST = "Error Threshold"




findOptimalNetworkParameter <- function(dataset, testName, testSet, kfolds){
  
  hiddenNodes <-  DEEP_HIDDEN
  stopping  <- DEEP_STOPPING   
  tolerance <- DEEP_TOLERANCE
  activation <-  DEEP_ACTIVATION

  
  print(paste("Running tests to determine optimal value for",testName))
  for(i in 1:length(testSet)){
    print(paste("Test",i,"of",length(testSet)))
    print(paste("Testing", testName, "=", testSet[i]))
    
    if(testName == HIDDEN_TEST){
      hiddenNodes <- testSet[[i]]
    }else if(testName==STOPPING_TEST){
      stopping <- testSet[i]
    }else if(testName==ACTIVATION_TEST){
      activation <- testSet[i]
    }else if(testName==TOLERANCE_TEST){
      tolerance <- testSet[i]
    }

    results <-  kfold(dataset, 5, deepNeural, hidden=hiddenNodes, stopping_rounds=stopping, activation=activation, stopping_tolerance=tolerance)
    
    
    results <-  c(hidden=hiddenNodes, stopping_rounds=stopping, stopping_tolerance=tolerance , activation=activation, results)
    
    if(i==1){
      allResults<-data.frame(ParamTest=unlist(results))
      
    }else{
      allResults<-cbind(allResults,data.frame(ParamTest=unlist(results)))
    }
    
  }
  allResults<-data.frame(t(allResults))

  print(formattable::formattable(allResults))

  
  
}

findAllOptimalNetworkParameters <- function(dataset, k){
  hiddenNodesTests <-  list()
  for(i in 1:5){
    hiddenNodesTests[[i]] <- c(i+2,i+2)
  }
  
  stoppingTests     <- 2:5   
  toleranceTests <- c(0.005, 0.08, 0.01, 0.12, 0.15, 0.18, 0.2)
  activationTests <- c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
  
  findOptimalNetworkParameter(dataset, HIDDEN_TEST, hiddenNodesTests, 5)
  findOptimalNetworkParameter(dataset, ACTIVATION_TEST, activationTests, 5)
  findOptimalNetworkParameter(dataset, STOPPING_TEST, stoppingTests, 5)
  findOptimalNetworkParameter(dataset, TOLERANCE_TEST, toleranceTests, 5)
  
  
}

# ************************************************
# deepNeural() :
#
# DEEP LEARNING EXAMPLE USING H2O library
#
# INPUT   :
#         :   Data Frame     - rawDataset  - original dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
deepNeural<-function(train,test,hidden=DEEP_HIDDEN, stopping_rounds=DEEP_STOPPING,stopping_tolerance=DEEP_TOLERANCE 
                     , activation=DEEP_ACTIVATION, plot=TRUE,...){
  
  myTitle<-"Preprocessed Dataset. Deep NN"

  deep_classifier<-N_DEEP_TrainClassifier(train=train,
                                          fieldNameOutput=OUTPUT_FIELD,
                                          hidden=hidden,
                                          stopping_rounds=stopping_rounds,
                                          stopping_tolerance=stopping_tolerance,
                                          activation=activation,
                                          reproducible=DEEP_REPRODUCABLE)
  
  # Evaluate the deep NN as we have done previously
  measures<-N_EVALUATE_DeepNeural(test=test,
                                  fieldNameOutput=OUTPUT_FIELD,
                                  deep=deep_classifier,
                                  plot=plot,
                                  myTitle = myTitle)
  
  if (plot==TRUE){
    # ************************************************
    # TELL ME SOMETHING INTERESTING...
    summary(deep_classifier)
    plot(deep_classifier)  # plots the scoring history
    
    # variable importance from the deep neural network
    importance = as.data.frame(h2o::h2o.varimp(deep_classifier))
    
    row.names(importance)<-importance$variable
    importanceScaled<-subset(importance, select=scaled_importance)*100
    colnames(importanceScaled)<-"Strength"
    
    barplot(t(importanceScaled),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importanceScaled)))
  }
  
  # ************************************************
  
  return(measures)
} #endof deepNeural()


createNeuralNetworkModel <- function(dataset,print=FALSE){
  N_DEEP_Initialise()
  
  model <-  N_DEEP_TrainClassifier(train=dataset,
                                   fieldNameOutput=OUTPUT_FIELD,
                                   hidden=DEEP_HIDDEN,
                                   stopping_rounds=DEEP_STOPPING,
                                   stopping_tolerance=DEEP_TOLERANCE,
                                   activation=DEEP_ACTIVATION,
                                   reproducible=DEEP_REPRODUCABLE)
  return(model)
  
  
}

evaluateNeuralNetworkModel <- function(dataset, printflag=FALSE){
  N_DEEP_Initialise()
  #keeps <-  c("TotalCharges", "MonthlyCharges", "tenure", "Contract_Monthtomonth", "InternetService_Fiber", "InternetService_TechSupport", "Contract_Twoyear", "PaymentMethod_Automatic", "InternetService_NoInternetService", "InternetService_TechSupport","InternetService_OnlineSecurity","Churn")
  
  #dataset <-  keepFields(dataset, keeps)
  
  ##UNCOMMENT OUT TO RUN, TAKES A LONG TIME. 
  #optimals <-  findAllOptimalNetworkParameters(dataset,5)
  
  
  results <-  kfold(dataset, 5, deepNeural, plot=printflag)
  return(results)


}

source("functions/mars/data_pre_processing_pipeline.R")
source("functions/mars/data_pre_processing_functions.R")
source("functions/nick/4labfunctions.R")
source("functions/nick/lab4DataPrepNew.R")
source("functions/mars/utility_functions.R")











print("Sourcing neural_network_functions.R ")