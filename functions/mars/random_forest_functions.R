# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   RANDOM FOREST FUNCTIONS
#
#   
#   DATE:     15 NOVEMBER 2020
#   VERSION:  V1.03
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version
#   1.01      12/11/2020    Chris Endacott + Brian Nyathi + Adlan Elias    Implemented Random Forest
#   1.02      14/11/2020    Chris Endacott + Brian Nyathi + Adlan Elias    Implemented K FOLD
#   1.03      15/11/2020    Chris Endacott + Brian Nyathi + Adlan Elias    Implemented parameter tuning
# ************************************************

FOREST_SIZE <- 800                 # Number of trees in the forest
MAX_NODES <-  20                   # Max nodes each tree can have
MTRY <- 5                          # Number of variables availible at each split


#Names of tests as constants to reduce potential input errors
FOREST_SIZE_TEST = "Forest Size"
MAX_NODES_TEST = "Max Nodes"
MTRY_TEST = "MTRY"




# ************************************************
# Name      :   testForestParameter() :
# Purpose   :   Run tests to determine an optimal parameter
#
# INPUT     :   data frame      - dataset       - dataset to evaluate
#           :   string          - testName      - Name of test
#           :   vector          - testSet       - Vector containing variants of the parameter to be tested
#           :   integer         - kfolds        - the number of folds to evaluate with
#
# OUTPUT    :   None
#
# ************************************************
testForestParameter <- function(dataset, testName, testSet, kfolds){
  
  #Set parameters to defaults
  forestSize <- FOREST_SIZE
  maxNodes <- MAX_NODES
  mtry <- MTRY
  
  
  print(paste("Running tests to determine optimal value for:",testName))
  for(i in 1:length(testSet)){
    print(paste("Test",i,"of",length(testSet)))
    print(paste("Testing", testName, "=", testSet[i]))
    
    #Change the parameter we're testing for to the value in the test set
    if(testName == FOREST_SIZE_TEST){
      forestSize <- testSet[[i]]
    }else if(testName==MAX_NODES_TEST){
      maxNodes <- testSet[i]
    }else if(testName==MTRY_TEST){
      mtry <- testSet[i]
    }
    
    #Evaluate the results of the new parameter set
    results <-  kfold(dataset, kfolds, randomForest, forestSize=forestSize, maxNodes=maxNodes, mtry=mtry, plot=FALSE)
    
    #Include used parameters in the results record
    results <-  c(forestSize=forestSize, maxNodes=maxNodes, mtry=mtry ,results)
    
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
# Name      :   testAllForestParameters() :
# Purpose   :   Runs all of our parameter tests that we wish to perform
#
# INPUT     :   data frame      - dataset       - dataset to evaluate
#           :   integer         - k             - the number of folds to evaluate with
#
# OUTPUT    :   None
#
# ************************************************
testAllForestParameters <- function(dataset, k){

  #Parameter sets to test for
  forestSizeTests <-  c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000, 3000, 4000)
  maxNodesTests <- c(5: 25, NULL)
  mtryTests <- c(1:10)
  
  #Run each test
  testForestParameter(dataset, MAX_NODES_TEST, maxNodesTests, k)
  testForestParameter(dataset, FOREST_SIZE_TEST, forestSizeTests, k)
  testForestParameter(dataset, MTRY_TEST, mtryTests, k)
}


# ************************************************
# Name      :   getTreeClassifications() :
# Purpose   :   Get forest measures
#
# INPUT     :   object                  - myTree                    - Trained forest model
#           :   data frame              - testDataset               - Test dataset
#           :   string                  - title                     - Title for plots
#           :   integer                 - classLabel                - Column id with class label
#           :   boolean                 - plot                      - Output intermediate steps?
#
# OUTPUT    :   list                    - measures                  - List of measures
#
# ************************************************
getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE)
    NprintMeasures(results=measures,title=title)
  
  return(measures)
} #endof getTreeClassifications()


# ************************************************
# randomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForest<-function(train,test,plot=TRUE, forestSize=FOREST_SIZE, maxNodes=MAX_NODES, mtry=MTRY,...){
  myTitle<-(paste("Preprocessed Dataset. Random Forest=",FOREST_SIZE,"trees"))
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=forestSize ,
                                 importance=TRUE,
                                 maxnodes=maxNodes,
                                 mtry=mtry)
  
  if(plot==TRUE){
    #Visualise some trees
    print("Example tree")
    tree1 <- randomForest::getTree(rf, k=1, labelVar=TRUE)
    #tree2 <- getTree(rf, 42) 
    #tree3 <- getTree(rf, 100) 
    print(formattable::formattable(tree1))
    #reprtree:::plot.getTree(rf)
    
    
  }
  # ************************************************
  # Use the created decision tree with the test dataset
  measures<-getTreeClassifications(myTree = rf,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importance)))
  }
  
  return(measures)
} #endof randomForest()


# ************************************************
# Name      :   createRandomForestModel() :
# Purpose   :   Create random forest model
#
# INPUT     :   data frame       - dataset     - Train dataset
#           :   boolean          - print       - Output intermediate values?
#
# OUTPUT    :   Trained random forest model
#
# ************************************************

createRandomForestModel <- function(dataset,print=FALSE){
  positionClassOutput<-which(names(dataset)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-dataset[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-dataset[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE ,
                                 importance=TRUE,
                                 maxnodes=MAX_NODES,
                                 mtry=MTRY)
  
  return(rf)
  
}


# ************************************************
# Name      :   rfPredict() :
# Purpose   :   Calculates class predictions using a trained random forest model
#
# INPUT     :   object           - rfModel              - the trained random forest model
#           :   data frame       - test                 - the dataset to predict
#
# OUTPUT    :   vector double    - test_predictedProbs  - the class predictions for the input dataset
#
# ************************************************
rfPredict <- function(model, test){
  positionClassOutput=which(names(test)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-test[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(model,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==1)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  return(test_predictedProbs)
  
}

# ************************************************
# Name      :   evaluateRandomForestModel() :
# Purpose   :   Evaluate the random forest model with a dataset
#
# INPUT     :   data frame           - dataset              - the dataset
#
# OUTPUT    :   list                 - results              - the evaluation metrics
#
# ************************************************
evaluateRandomForestModel<-function(dataset){
  
  #Keep these fields only
  keeps <-  c("TotalCharges", "MonthlyCharges", "tenure", "Contract_Monthtomonth", "InternetService_Fiber", "InternetService_TechSupport", "Contract_Twoyear", "Churn")
  dataset <-  keepFields(dataset, keeps)

  #Train using k fold cross val
  results <-  kfold(dataset, 5, randomForest, plot=FALSE)

  return(results)
  
} 


