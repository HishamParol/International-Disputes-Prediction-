# ************************************************
# Coursework - Preparing data for machine learning
# based on real-world dataset
#
# DATE: 25th November 2019
# VERSION: v1.00
# AUTHOR: Jupiter
#
# UPDATE
# 1.00 25/11/2019 Initial Version
# ************************************************
# Derived from 4labSolution.R by 
# Prof. Nick F Ryman-Tubb
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
# ************************************************



# ************************************************
# allocateFoldID() :
#
# Append a column called "foldID" that indicates the fold number
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
allocateFoldID<-function(dataset){
  recordsPerFold<-ceiling(nrow(dataset)/KFOLDS)
  
  foldIds<-rep(seq(1:KFOLDS),recordsPerFold)
  
  foldIds<-foldIds[1:nrow(dataset)]
  
  dataset$foldId<-foldIds
  
  return(dataset)
} #endof allocateFoldID()

# ************************************************
# stratifiedDataset() :
#
# Split dataset by the class (assume 2-class)
# Calculate the number of records that will appear in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# The dataset now has a foldID from which we can select the data
# for the experiments
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
stratifiedDataset<-function(originalDataset){
  
  positionClassOutput=which(names(originalDataset)==OUTPUT_FIELD)
  
  # Get the unique class values
  classes<-unique(originalDataset[,positionClassOutput])
  
  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1<-which(originalDataset[,positionClassOutput]==classes[1])
  split1<-originalDataset[indexClass1,]
  split2<-originalDataset[-indexClass1,]
  
  # Append a column that indicates the fold number for each class
  split1<-allocateFoldID(split1)
  split2<-allocateFoldID(split2)
  
  # Combine the two datasets
  
  newDataset<-rbind(split1,split2)
  
  #Randomise the classes
  newDataset<-newDataset[order(runif(nrow(newDataset))),]
  
  return(newDataset)
}

# ************************************************
# stratifiedSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit<-function(newDataset,fold){
  
  test<-subset(newDataset, subset= foldId==fold, select=-foldId)
  train<-subset(newDataset, subset= foldId!=fold,select=-foldId)
  
  return(list(
    train=train,
    test=test))
}

# ************************************************
# runExperiment() :
#
#
# INPUT   :   data frame         - dataset        - dataset
#             object function    - FUN            - name of function
#             ...                - optional        - parameters are passed on
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
runExperiment<-function(dataset,FUN, ...){
  
  allResults<-data.frame()
  
  for (k in 1:KFOLDS){
    
    splitData<-stratifiedSplit(newDataset=dataset,fold=k)
    
    measures<-FUN(train=splitData$train,
                  test=splitData$test,
                  plot=(k==KFOLDS),...)
    
    allResults<-rbind(allResults,data.frame(measures))
  } #endof for()
  
  # Return the means from all the experiments back as a list
  getMeans<-colMeans(allResults)
  #getMeans[1:4]<-as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints
  getMeans$varGood<-var(allResults$pgood)
  getMeans$varAccuracy<-var(allResults$accuracy)
  getMeans$varMCC<-var(allResults$MCC)
  
  #return(as.list(colMeans(allResults)))
  return(as.list(getMeans))
} #endof runExperiment()

# ************************************************
# getTreeClassifications() :
#
# Put in test dataset and get out class predictions of the decision tree
# Determine the threshold, plot the results and calculate metrics
#
# INPUT   :   object         - myTree        - tree
#         :   Data Frame     - testDataset - dataset to evaluate
#         :   string         - title        - string to plot as the chart title
#         :   int            - classLabel   - lable given to the positive (TRUE) class
#         :   boolean        - plot         - TRUE to output results/charts
#
# OUTPUT  :   List       - Named evaluation measures
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
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
 
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
# simpleDT() :
#
# Create C5 Decision Tree on the raw dataset
# A decision tree may not need the dataset to be pre-processed
#
# INPUT   :
#             Data Frame     - train       - original train dataset
#             Data Frame     - test        - original test dataset
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
simpleDT<-function(train,test,plot=TRUE){
  
  # In original dataset, $Status is the classification label
  # We need to convert this to give the minority class a value of 0
  # this just makes it easiert to define the confusioon matrix!
  # for the UCI-G this is a class of {0,1} being {bad, good}
  #  train<-NConvertClass(train) - this is already handled in JDataPreparation
  #  test<-NConvertClass(test)  - this is already handled in JDataPreparation
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)

    # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  myTitle<-"DT C5.0 - Hold Out"
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  tree<-C50::C5.0(x=train[-positionClassOutput],
                  y=factor(train[,positionClassOutput]),
                  rules=TRUE,
                  trials=1)
  
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title=myTitle)
  
  measures$varGood<-0.0
  measures$varAccuracy<-0.0
  measures$varMCC<-0.0
  
  if (plot==TRUE){
  
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    sink(paste(myTitle, SUMMARY_FILENAME))
    print("Decision Tree - Summary")
    print(summary(tree))
    sink()
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=myTitle)
    
    # ************************************************
    # We can visualise the tree
    
    #Function to output the tree as rules to a file
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))

    NprintDTRules(dftreerules, paste(myTitle, RULES_FILENAME))
    
    # ************************************************
    # Creates the same  C5.0 decision tree & output as a tree structure, plot it
    # The "partykit" library requires the variables (wrongly) to be global
    print("Plot decision tree to file.")
    
    Global_train_inputs<<-train_inputs
    Global_train_expected<<-train_expected
    
    # :: is used to specify a function within the named package to avoid confusion
    tree<-C50::C5.0(x=Global_train_inputs,
                    factor(Global_train_expected),
                    trials=1)
    
    # ::: is used to directly access a member of a package that is internal
    graphtree<-C50:::as.party.C5.0(tree)
    
    # The plot is large - so print to a big PDF file
    pdf(paste(myTitle,PDF_FILENAME), width=100, height=50, paper="special", onefile=F)
    
    # The number is the node level of the tree to print
    plot(graphtree[NODE_LEVEL])
    
    #This closes the PDF file
    dev.off()
  }
  return(measures)
} #endof simpleDT()


# ************************************************
# fullDT() :
#
# Create C5 Decision Tree on pre-processed dataset
#
# INPUT   :
#             Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             int            - boost       - number of trees to boost
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
fullDT<-function(train,test,boost=1,plot=TRUE){
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  # ************************************************
  # Create a standard Decision Tree using the C5.0 algorithm
  # Uses library C50
  # Outputs the tree in the format of rules
  
  myTitle<-"DT C5.0 - K-fold"
  
  if (boost>1)
    myTitle<-paste(myTitle,"BOOSTED=",boost)
  
  print(myTitle)
  
  tree<-C50::C5.0(x=train_inputs,
                  factor(train_expected),
                  rules=TRUE,
                  trials=boost)
  
  # Use the created decision tree with the test dataset
  # to determine best classification threshold & calculate metrics
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    sink(paste(myTitle, SUMMARY_FILENAME))
    print("Decision Tree - Summary")
    print(summary(tree))
    sink()
    
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=myTitle)
    
    # ************************************************
    # We can visualise the tree
    
    #Function to output the tree as rules to 
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
    
    NprintDTRules(dftreerules, paste(myTitle, RULES_FILENAME))

        # ************************************************
    # Creates the same  C5.0 decision tree & output as a tree structure, plot it
    # The "partykit" library requires the variables (wrongly) to be global
    print("Plot decision tree to file.")
    
    Global_train_inputs<<-train_inputs
    Global_train_expected<<-train_expected
    
    # :: is used to specify a function within the named package to avoid confusion
    tree<-C50::C5.0(x=Global_train_inputs,
                    factor(Global_train_expected),
                    trials=boost)
    
    # ::: is used to directly access a member of a package that is internal
    graphtree<-C50:::as.party.C5.0(tree)
    
    # The plot is large - so print to a big PDF file
    pdf(paste(myTitle,PDF_FILENAME), width=100, height=50, paper="special", onefile=F)
    
    # The number is the node level of the tree to print
    plot(graphtree[NODE_LEVEL])
    
    #This closes the PDF file
    dev.off()
  }
  return(measures)
} #endof fullDT()


# ************************************************
# randomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#             boolean        - save        - TRUE = output the model object
#                                                   to file for deployment
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForest<-function(train,test,plot=TRUE, save=FALSE){
  
  myTitle<-(paste("Random Forest=",FOREST_SIZE,"trees"))
  print(myTitle)
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  
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
    
    sink(paste(myTitle, SUMMARY_FILENAME))
    print("Random Forest - Importance")
    print(importance)
    rf1<-randomForest::getTree(rf, k=1, labelVar = TRUE)
    print("Tree 1:")
    print(summary(rf1))
    rfn<-randomForest::getTree(rf, k=FOREST_SIZE, labelVar = TRUE)
    print("Tree N:")
    print(summary(rfn))
    sink()
    
    # PDF in A4
    pdf(paste(myTitle, VARBAR_FILENAME), paper="a4", onefile=F)
    
    barplot(t(importance),las=2,
            cex.names = 0.7,
            main=myTitle)

    dev.off()
    
    # PDF in A4 landscape
    pdf(paste(myTitle, VARIMP_FILENAME), paper="a4r", onefile=F)
    
    varImpPlot(rf, border=0.5, main=myTitle)
    
    dev.off()
    
    
    print(formattable::formattable(data.frame(importance)))
    
  }
  
  if (save==TRUE)
  {
    #Save the Random Forest for deployment
    saveRDS(rf, file=ROBJECT_FILENAME)
  }
  
  return(measures)
} #endof randomForest()


# ************************************************
# mlpNeural() :
#
# SHALLOW BASIC MLP TRAINING
# Uses Keras or h2o library to create a basic 3 layer MLP
#
# INPUT   :
#         :   Data Frame     - rawDataset  - original dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# 311019NRTY Updated to use either h2o or Keras library
# ************************************************
mlpNeural<-function(train,test, plot=TRUE){
  
  myTitle<-paste("Preprocessed Dataset. MLP. Hidden=",BASICNN_HIDDEN,sep="")
  print(myTitle)
  
  # Set to TRUE to use the h2o library
  # otheriwse FALSE to try to use the Keras library
  
  if (TRUE) {
    N_DEEP_Initialise()
    
    mlp_classifier<-N_DEEP_TrainClassifier(train=train,
                                           fieldNameOutput=OUTPUT_FIELD,
                                           hidden=BASICNN_HIDDEN,
                                           stopping_rounds=DEEP_STOPPING,
                                           stopping_tolerance=DEEP_TOLERANCE,
                                           activation=DEEP_ACTIVATION,
                                           reproducible=DEEP_REPRODUCABLE)
    
    if (plot==TRUE)
    {
      plot(mlp_classifier,metric="classification_error")
      
      # variable importance from the deep neural network
      importance = as.data.frame(h2o::h2o.varimp(mlp_classifier))
      
      row.names(importance)<-importance$variable
      importanceScaled<-subset(importance, select=scaled_importance)*100
      colnames(importanceScaled)<-"Strength"
      
      # PDF in A4 landscape
      pdf(paste(myTitle, VARBAR_FILENAME), paper="a4r", onefile=F)
      
      barplot(t(importanceScaled),las=2, border = 0,
              cex.names =0.7,
              main=myTitle)
      
      dev.off()
      
      print(formattable::formattable(data.frame(importanceScaled)))
      
    }
    
    # Evaluate the deep NN as we have done previously
    measures<-N_EVALUATE_DeepNeural(test=test,
                                    fieldNameOutput=OUTPUT_FIELD,
                                    deep=mlp_classifier,
                                    plot=plot,
                                    myTitle = myTitle)
  } else {
    
    mlp_classifier<-N_MLP_TrainClassifier(train=train,
                                          fieldNameOutput=OUTPUT_FIELD,
                                          hidden=BASICNN_HIDDEN,
                                          plot=plot)
    
    measures<-N_evaluate_MLP(test=test,
                             fieldNameOutput=OUTPUT_FIELD,
                             mlp_classifier=mlp_classifier,
                             plot=plot,
                             myTitle=myTitle)
  } #endof if()
  
  return(measures)
} #endof mlpNeural()

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
deepNeural<-function(train,test,plot=TRUE){
  
  myTitle<-"Preprocessed Dataset. Deep NN"
  
  N_DEEP_Initialise()
  
  deep_classifier<-N_DEEP_TrainClassifier(train=train,
                                          fieldNameOutput=OUTPUT_FIELD,
                                          hidden=DEEP_HIDDEN,
                                          stopping_rounds=DEEP_STOPPING,
                                          stopping_tolerance=DEEP_TOLERANCE,
                                          activation=DEEP_ACTIVATION,
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
    
    # PDF in A4 landscape
    pdf(paste(myTitle, VARBAR_FILENAME), paper="a4r", onefile=F)

        barplot(t(importanceScaled),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    dev.off()
        
    print(formattable::formattable(data.frame(importanceScaled)))
  }
  
  # ************************************************
  
  return(measures)
} #endof deepNeural()

# ************************************************
# modelling() :
#
# Run a range of models and save the metrics
# using K-fold cross validation.
#
# Run the chosen model again on the full data set
# and save the model for deployment.
#
# INPUT: None
#
# OUTPUT :None
# ************************************************
modelling<-function(){
  
  # ************************************************
  # This reads in a CSV file 
  # The first row of this file has the field names of each column
  
  rawCases<-NreadDataset(DATASET_FILENAME)

  dataset<-JdataPreparation(rawCases)
  

  # Hold Out method
  original<-NPREPROCESSING_splitdataset(dataset)
  measures<-simpleDT(original$train,original$test, plot = TRUE)

  # Create a data frame to compare results from different experiments
  allResults<-data.frame(DT_Holdout=unlist(measures))
  
  # Stratified K-fold cross-validation
  dataset<-stratifiedDataset(dataset)
  
  # ************************************************************
  # Experiment with C5 decision tree and K-fold cross validation
  measures<-runExperiment(dataset = dataset,FUN = fullDT)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(DT_Kfold=unlist(measures)))
  
  # ************************************************
  # Creates a boosted tree
  # The algorithm allows for "boosting"
  # Many trees are built using all the input fields and these then "vote"
  #measures<-fullDT(train=splitData$train,test=splitData$test,boost=BOOST,plot=FALSE)
  measures<-runExperiment(dataset = dataset,FUN = fullDT, boost=BOOST)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(DT_boost=unlist(measures)))
  
  # ************************************************
  # Random forest model
  # measures<-randomForest(train=splitData$train, test=splitData$test)
  measures<-runExperiment(dataset = dataset,FUN = randomForest)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(RandomForest=unlist(measures)))
  
  # ************************************************
  # Now a MLP neural network
  # measures<-mlpNeural(train=splitData$train, test=splitData$test)
  measures<-runExperiment(dataset = dataset,FUN = mlpNeural)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(MLP=unlist(measures)))
  
  # ************************************************
  # Now a deep neural network
  # measures<-deepNeural(train=splitData$train, test=splitData$test)
  measures<-runExperiment(dataset = dataset,FUN = deepNeural)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(Deep_Neural=unlist(measures)))
  
  allResults<-data.frame(t(allResults))
  
  # Sort by highest MCC
  allResults<-allResults[order(allResults$MCC,decreasing = TRUE),]
  
  # Output results to compare all classifiers
  allResults[,1:4]<-sapply(allResults[,1:4], as.integer)
  allResults$folds<-KFOLDS
  print(formattable::formattable(allResults))
  
  # Write frame to a CSV file
  write.csv(allResults,file=RESULTS_FILENAME)
  
  # Random Forest is the chosen model
  # Train on the full dataset
  measures<-randomForest(train=dataset,
                         test=dataset,
                         plot=FALSE,
                         save=TRUE)
  
  # Write measures to a CSV file
  write.csv(measures,file=paste("RandomForest",RESULTS_FILENAME), row.names = FALSE)
  
} #endof modelling()

# ************************************************
# loadModel() :
#
# Load a Random Forest model
# that has been saved for deployment
# together with the saved Results file
# that contains the computed threshold.
loadModel<-function(){
  
  rf<-readRDS(file=ROBJECT_FILENAME)
  print(summary(rf))
  
  return(rf)
} #endof loadModel

