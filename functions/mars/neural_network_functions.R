# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   NEURAL NETWORK FUNCTIONS
#
#   
#   DATE:     14 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version
#   1.01      12/11/2020    Chris Endacott + Ryan Banks + Adlan Elias    Implemented Neural Net
#   1.02      14/11/2020    Chris Endacott + Ryan Banks + Adlan Elias    Parameter tuning
# ************************************************

DEEP_HIDDEN       <- c(6,6)               # Number of neurons in each layer
DEEP_STOPPING     <- 4                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.1                 # Error threshold
DEEP_ACTIVATION   <- "TanhWithDropout"    # Non-linear activation function
DEEP_REPRODUCABLE <- TRUE                 # Set to TRUE to test training is same for each run

HIDDEN_TEST = "Hidden Nodes"
STOPPING_TEST = "Stopping threshold"
ACTIVATION_TEST = "Activation function"
TOLERANCE_TEST = "Error Threshold"
BASICNN_EPOCHS=350


# ************************************************
# Name      :   findOptimalNetworkParameter() :
# Purpose   :   Find the optimal network parameter for a specific test
#
# INPUT     :   data frame            - dataset     - the train dataset
#           :   string                - testName    - constant name of the parameter test
#           :   vector                - testSet     - parameter values to test
#
# OUTPUT    :   None
#
# ************************************************
findOptimalNetworkParameter <- function(dataset, testName, testSet, kfolds){
  
  #Set parameters to defaults
  hiddenNodes <-  DEEP_HIDDEN
  stopping  <- DEEP_STOPPING   
  tolerance <- DEEP_TOLERANCE
  activation <-  DEEP_ACTIVATION

  
  print(paste("Running tests to determine optimal value for",testName))
  for(i in 1:length(testSet)){
    print(paste("Test",i,"of",length(testSet)))
    print(paste("Testing", testName, "=", testSet[i]))
    
    #Change the parameter we're testing for to the value in the test set
    if(testName == HIDDEN_TEST){
      hiddenNodes <- testSet[[i]]
    }else if(testName==STOPPING_TEST){
      stopping <- testSet[i]
    }else if(testName==ACTIVATION_TEST){
      activation <- testSet[i]
    }else if(testName==TOLERANCE_TEST){
      tolerance <- testSet[i]
    }

    #Evaluate the results of the new parameter set
    results <-  kfold(dataset, kfolds, deepNeural, hidden=hiddenNodes, stopping_rounds=stopping, activation=activation, stopping_tolerance=tolerance, plot=FALSE)
    
    #Include used parameters in the results record
    results <-  c(hidden=hiddenNodes, stopping_rounds=stopping, stopping_tolerance=tolerance , activation=activation, results)
    
    #Create a new dataframe if one doesn't exist yet or add to the existing one
    if(i==1){
      allResults<-data.frame(ParamTest=unlist(results))
      
    }else{
      allResults<-cbind(allResults,data.frame(ParamTest=unlist(results)))
    }
    
  }
  
  #Transpose the data frame to make the metrics fields
  allResults<-data.frame(t(allResults))
  
  #Print the resulting overview for the parameter tests
  print(formattable::formattable(allResults))

  
  
}

# ************************************************
# Name      :   findAllOptimalNetworkParameters() :
# Purpose   :   Run all of our neural network parameter tests
#
# INPUT     :   dataset     dataset
#
# OUTPUT    :   None
#
# ************************************************
findAllOptimalNetworkParameters <- function(dataset, k){
  
  #The hidden nodes parameter needs a special format
  hiddenNodesTests <-  list()
  for(i in 1:5){
    hiddenNodesTests[[i]] <- c(i+2,i+2)
  }
  
  stoppingTests     <- 2:5   
  toleranceTests <- c(0.005, 0.08, 0.1, 0.12, 0.15, 0.18, 0.2)
  activationTests <- c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
  
  #Run all parameter tests
  findOptimalNetworkParameter(dataset, HIDDEN_TEST, hiddenNodesTests, k)
  findOptimalNetworkParameter(dataset, ACTIVATION_TEST, activationTests, k)
  findOptimalNetworkParameter(dataset, STOPPING_TEST, stoppingTests, k)
  findOptimalNetworkParameter(dataset, TOLERANCE_TEST, toleranceTests, k)
  
  
}

# ************************************************
# deepNeural() :
# Purpose   :   Evaluate the neural network model
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


# ************************************************
# Name      :   nnPredict() :
# Purpose   :   Calculates class predictions using a trained neural network model
#
# INPUT     :   object           - nnModel              - the trained neural network
#           :   data frame       - test                 - the dataset to predict
#
# OUTPUT    :   vector double    - test_predicted       - the class predictions for the input dataset
#
# ************************************************
nnPredict <- function(model, test){
  test_expected<-test[,OUTPUT_FIELD]
  test_h2o <- as.h2o(test, destination_frame = "testdata")
  
  pred <- h2o::h2o.predict(model, test_h2o)
  
  test_predicted<-as.vector(pred$p1)  #Returns the probabilities of class 1
  return(test_predicted)
}

# ************************************************
# Name      :   createNeuralNetworkModel() :
# Purpose   :   Train NN model
#
# INPUT     :   data frame        - dataset      - train dataset
#           :   boolean           - print        - Output intermediate values?

#
# OUTPUT    :   object            - model        - Trained model
#
# ************************************************
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


# ************************************************
# Name      :   evaluateNeuralNetworkModel() :
# Purpose   :   Evaluate NN model
#
# INPUT     :   data frame                          - dataset           - the train/validation dataset
#           :   boolean                             - printflag         - output intermediate values to the console?
#
# OUTPUT    :   Model performance measures
#
# ************************************************
evaluateNeuralNetworkModel <- function(dataset, printflag=FALSE){
  N_DEEP_Initialise()
  #keeps <-  c("TotalCharges", "MonthlyCharges", "tenure", "Contract_Monthtomonth", "InternetService_Fiber", "InternetService_TechSupport", "Contract_Twoyear", "PaymentMethod_Automatic", "InternetService_NoInternetService", "InternetService_TechSupport","InternetService_OnlineSecurity","Churn")
  
  #dataset <-  keepFields(dataset, keeps)
  
  ##UNCOMMENT OUT TO RUN, TAKES A LONG TIME. 
  #optimals <-  findAllOptimalNetworkParameters(dataset,5)
  
  
  results <-  kfold(dataset, KFOLDS, deepNeural, plot=printflag)
  return(results)


}
