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
DEEP_STOPPING     <- 2                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.01                 # Error threshold
DEEP_ACTIVATION   <- "Tanh"    # Non-linear activation function
DEEP_REPRODUCABLE <- TRUE                 # Set to TRUE to test training is same for each run
BASICNN_EPOCHS = 100

HIDDEN_TEST = "Hidden Nodes"
STOPPING_TEST = "Stopping threshold"
ACTIVATION_TEST = "Activation function"
TOLERANCE_TEST = "Error Threshold"




findOptimalNetworkParameter <- function(dataset, testName, testSet, kfolds){
  
  hiddenNodes <-  DEEP_HIDDEN
  stopping  <- DEEP_STOPPING   
  tolerance <- DEEP_TOLERANCE
  activation <-  DEEP_ACTIVATION
  
  
  #Assign the folds for testing and shuffle
  dataset <- PREPROCESSING_stratDataset(dataset, kfolds)
  dataset <- dataset[order(runif(nrow(dataset))),]

  

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

    results <-  data.frame()
    #Kfold validate for neural specific
    for(k in 1:kfolds){
      train <-  subset(dataset, (dataset$foldId!=k))
      test <-  dataset[dataset$foldId == k,]
      train <-  dropFields(train, c("foldId"))
      test <-  dropFields(test, c("foldId"))
      result <-  deepNeural(train, test, hidden=hiddenNodes, stopping_rounds=stopping, stopping_tolerance=tolerance , activation=activation, plot=TRUE)
      results <-rbind(results, data.frame(result))
    }
    
    avgs <-  colMeans(results)
    avgs[1:4] <-  as.integer(round(avgs[1:4]))
    avgs[5:13] <-  round(avgs[5:13], digits=3)
    
    
    results <-  c(hidden=hiddenNodes, stopping_rounds=stopping, stopping_tolerance=tolerance , activation=activation, avgs)
    
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
  
  print(hiddenNodesTests)
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
deepNeural<-function(train,test,hidden=DEEP_HIDDEN, stopping_rounds=DEEP_STOPPING,stopping_tolerance=DEEP_TOLERANCE , activation=DEEP_ACTIVATION, plot=TRUE){
  
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


main <- function(){
  N_DEEP_Initialise()
  
  keeps <-  c("TotalCharges", "MonthlyCharges", "tenure", "Contract_Monthtomonth","PaymentMethod_Automatic" ,"Churn")
  
  dataset <- mars_GetPreprocessedDataset(FALSE)
  
  #dataset <-  keepFields(dataset, keeps)
  
  ##UNCOMMENT OUT TO RUN, TAKES A LONG TIME. 
  optimals <-  findAllOptimalNetworkParameters(dataset,5)
  
  
  #results <-  kfold(dataset, 5, deepNeural)
  
  

  


}

source("functions/mars/data_pre_processing_pipeline.R")
source("functions/mars/data_pre_processing_functions.R")
source("functions/nick/4labfunctions.R")
source("functions/nick/lab4DataPrepNew.R")
source("functions/mars/utility_functions.R")

main()











print("Sourcing neural_network_functions.R ")