# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRATICAL BUSINESS ANALYTICS
#  COM3018 / COMM053
#
# Prof. Nick F Ryman-Tubb
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 15th February 2018
#
#
# UPDATE
# 1.00      1/2/2017    Initial Version
# 1.01      18/2/2018   Pass datafile as variable
# 1.02      10/2/2019   Updated for 2019 lab / simplify
# 1.03      20/2/2019   nrmse removed unneeded code
# 1.04      10/10/2019  Added Nmae(), updated mewtrics in NscatterPlotMultiLinearRegression()
# 1.05      13/10/2019  NscatterPlotNonLinearRegression() evaluate on test
#
# ************************************************
# Support functions prefixed by "N"

# ************************************************
# Calculate the Mean Abosolute Error MAE metric
#
# INPUT:      vector - float values for expected values
#             vector - float values of predicted values
# OUTPUT :    float - calculated RMSE
# ************************************************
Nrmse<-function(actual_y,y_predicted){

  return(sqrt(mean((actual_y-y_predicted)^2)))
}

# ************************************************
# Calculate the Root Mean Squared Error RMSE metric
#
# INPUT:      vector - float values for expected values
#             vector - float values of predicted values
# OUTPUT :    float - calculated RMSE
# ************************************************
Nmae<-function(actual_y,y_predicted){

  return((mean(abs(actual_y-y_predicted))))
}

# ************************************************
# Calculate the r2 metric (adjusted to allow for multiple inputs)
# REMEMBER this is a measure of the fit of the training data to the model
# it is NOT a measure of how good a predictor the model is on unseen data
#
# INPUT:      object - trained linear model
# OUTPUT :    float - calculated RMSE
# ************************************************
Nr2<-function(linearModel){

  rsq<-summary(linearModel)$adj.r.squared

  return(rsq)
}

# ************************************************
# Nrescale() :
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - values to scale
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(d){

  minv<-min(d)
  maxv<-max(d)
  return((d-minv)/(maxv-minv))
}

# ************************************************
# Nrescaleentireframe() :
# INPUT: text - filename
# OUTPUT : Frame - dataset
# ************************************************
# Rescle the entire dataframe to [0.0,1.0]
Nrescaleentireframe<-function(dataset){
  for(field in 1:(ncol(dataset))){
    dataset[,field]<-Nrescale(dataset[,field])
  }
  return(dataset)
}

# ************************************************
# Visulise the decisions made by the linear model
# INPUT:
#               String - name of the field to predict
#               Vector - values of the output field
#               Frame -  values of the predictor field
#               Float - value of the threshold over which is "YES"
#               String - title of the graph
# OUTPUT :      None
# ************************************************
NplotDecisions<-function(fieldNameOutput, y_predicted, topredict, threshold, title){

  xname<-names(topredict)
  y_decisions<-ifelse(y_predicted>threshold,1.0,0.0)

  #Create a frame with all values, then two frames with the CLASS 0/CLASS 1
  scatter<-data.frame(topredict,y_decisions)
  class1<-scatter[which(scatter$y_decisions==1),]
  class0<-scatter[which(scatter$y_decisions==0),]

  plot(class1,pch=4,ylab=fieldNameOutput,xlab=xname,main=paste(title,fieldNameOutput),col="green",ylim=c(0, 1),sub="2 Classes. Threshold shown.",xlim=c(min(topredict),max(topredict)))
  points(class0,pch=6,ylab=fieldNameOutput,xlab=xname,main=paste(title,fieldNameOutput),col="red",ylim=c(0, 1))

}

# ************************************************
# Creates a multiple linear regression model on the two named fields
# Visulises on 3d graph
# Evaluation is output to the console
#
# INPUT:        frame - dataset
#               String - name of the field to predict
#               String - x predictor field
#               String - y predictor field
#               Boolean - TRUE if log of the name in xname
# OUTPUT :      None
# ************************************************
# Uses library(scatterplot3d)
NscatterPlotMultiLinearRegression<-function(datasetTrain, datasetTest, fieldNameOutput,xname,zname){

  formular<-paste(fieldNameOutput,"~",xname,"+",zname)
  linearModel<-lm(formular,data=datasetTrain)
  #linearModel<-lm(medv~.-lstat+log(lstat),data=datasetTrain) # this selects ALL fields and log

  x<-datasetTrain[,xname]
  y<-datasetTrain[,fieldNameOutput]
  z<-datasetTrain[,zname]

  # Get the predicted values from the model in TEST dataset
  dframe<-datasetTest[,c(fieldNameOutput,xname,zname)]
  #dframe<-datasetTest
  y_actual<-dframe[,zname]
  y_predicted<-as.vector(predict(linearModel,dframe))

  # Calculate metrics
  RMSE<-round(Nrmse(y_actual,y_predicted),digits=2)
  mae<-round(Nmae(y_actual,y_predicted),digits=2)
  r2<-round(Nr2(linearModel),digits=2)

  s3d<-scatterplot3d(x,y,z, main="2 Predictor Multi-linear Regression Model",
                     xlab=xname,ylab=fieldNameOutput,zlab=zname,
                     pch=16,highlight.3d = TRUE,
                     sub=paste("MAE=",mae,"RMSE=",RMSE," R2=",r2))
  s3d$plane3d(linearModel)
}

# ************************************************
# Creates a multiple linear regression model on the two named fields
# Visulises on 3d graph
# Evaluation is output to the console
#
# INPUT:        frame - dataset
#               String - name of the field to predict
#               String - x predictor field
#               String - y predictor field
#               Boolean - TRUE if log of the name in xname
# OUTPUT :      None
# ************************************************
# Uses library(scatterplot3d)
NscatterPlotLogMultiLinearRegression<-function(datasetTrain, datasetTest, fieldNameOutput,xname,yname){
  
  formular<-paste("log10(",fieldNameOutput,")~",xname,"+",yname)
  linearModel<-lm(formular,data=datasetTrain)

#  x<-datasetTrain[,xname]
#  y<-datasetTrain[,yname]
#  z<-datasetTrain[,fieldNameOutput]
  
  # Get the predicted values from the model in TEST dataset
  dframe<-datasetTest[,c(fieldNameOutput,xname,yname)]
  #dframe<-datasetTest
  z_actual<-dframe[,fieldNameOutput]
  z_predicted<-as.vector(predict(linearModel,dframe))
  
  # Calculate metrics
  RMSE<-round(Nrmse(z_actual,z_predicted),digits=2)
  mae<-round(Nmae(z_actual,z_predicted),digits=2)
  r2<-round(Nr2(linearModel),digits=2)
 
  x<-dframe[,xname] 
  y<-dframe[,yname]
  s3d<-scatterplot3d(x,y,z_predicted, main="2 Predictor Multi-linear Regression Model",
                     xlab=xname,ylab=yname,zlab=fieldNameOutput,
                     pch=16,highlight.3d = TRUE,
                     sub=paste("MAE=",mae,"RMSE=",RMSE," R2=",r2))
  s3d$points3d(x,y,z_actual,col="blue",pch=16)
  #s3d$plane3d(linearModel)
  
  return(
    list(MAE=mae,
         RMSE=RMSE,
         r2=r2))  
}


# ************************************************
# Non-linear regression model using log predictor field against outcome
# INPUT:      data frame - datasetTrain - create model
#             data frame - datasetTest - test model
#             data frame - datasetTest - test model
#             String - name of field to predict
#             String - name of single predictor field
# OUTPUT :    None
# ************************************************
NscatterPlotLogLinearRegression<-function(datasetTrain, datasetTest, fieldNameOutput, predictorName1, predictorName2){
  formular<-paste("log10(",fieldNameOutput,")","~",predictorName1,"+",predictorName2)
  linearModel<-lm(formular,data=datasetTrain)
  
  predictorInput<-data.frame(datasetTest[,predictorName1])
  cbind(predictorInput<-data.frame(datasetTest[,predictorName2]))
  names(predictorInput)<-c(predictorName1,predictorName2)
  
  y_actual<-datasetTest[,fieldNameOutput]
  y_predicted<-predict(linearModel, predictorInput)
  
  RMSE<-round(Nrmse(y_actual,y_predicted),digits=2)
  mae<-round(Nmae(y_actual,y_predicted),digits=2)
  r2<-round(Nr2(linearModel),digits=2)
  
  scatter<-data.frame(predictorInput,y_actual,y_predicted)
  scatter<-scatter[order(predictorInput),]
  
  plot(scatter[,1],scatter[,2],pch=4,
       ylab=fieldNameOutput,xlab=predictorName,
       main=paste("Log-Linear Regression:",fieldNameOutput,
                  sub=paste("Log:","MAE=",mae,"RMSE=",RMSE," R2=",r2)))
  
  # Plot the model prediction line on the chart
  topredict<-data.frame(seq(min(scatter[,1]),max(scatter[,1]),by=.1))
  names(topredict)<-predictorName
  #y_predicted<-predict(linearModel, topredict)
  #lines(topredict[,predictorName],y_predicted,col="red",lwd=4)
  #y_predicted<-predict(linearModel, predictorInput)
  #lines(predictorInput,y_predicted,col="red",lwd=4)
  lines(scatter[,1], scatter[,3],col="red",lwd=4)
  
  return(
    list(MAE=mae,
         RMSE=RMSE,
         r2=r2))
}




# ************************************************
# Non-linear regression model using one predictor field against outcome
# INPUT:      data frame - datasetTrain - create model
#             data frame - datasetTest - test model
#             data frame - datasetTest - test model
#             String - name of field to predict
#             String - name of single predictor field
#             int - polynominal order
# OUTPUT :    None
# ************************************************
NscatterPlotNonLinearRegression<-function(datasetTrain, datasetTest, fieldNameOutput,predictorName,cpoly){

  formular<-paste(fieldNameOutput,"~","poly(",predictorName,",",cpoly,")")
  linearModel<-lm(formular,data=datasetTrain)

  predictorInput<-data.frame(datasetTest[,predictorName])
  names(predictorInput)<-predictorName

  y_actual<-datasetTest[,fieldNameOutput]
  y_predicted<-predict(linearModel, predictorInput)

  RMSE<-round(Nrmse(y_actual,y_predicted),digits=2)
  mae<-round(Nmae(y_actual,y_predicted),digits=2)
  r2<-round(Nr2(linearModel),digits=2)

  scatter<-data.frame(predictorInput,y_actual)
  scatter<-scatter[order(predictorInput),]

  par(mar=c(5.1,4.1,4.1,2.1))
  plot(scatter[,1],scatter[,2],pch=4,
       ylab=fieldNameOutput,xlab=predictorName,
       main=paste("Non-Linear Regression:",fieldNameOutput,
                  sub=paste("Polynominal order:",cpoly,"MAE=",mae,"RMSE=",RMSE," R2=",r2)))

  # Plot the model prediction line on the chart
  topredict<-data.frame(seq(min(scatter[,1]),max(scatter[,1]),by=.1))
  names(topredict)<-predictorName
  y_predicted<-predict(linearModel, topredict)
  lines(topredict[,predictorName],y_predicted,col="red",lwd=4)

}

# ************************************************
# Using simple linear classifier
# Make a "decision" based on threshold
#
# INPUT:        Frame - dataset
#               String - name of the field to predict
#               String - name of the predictor field
#               Float - value of the threshold over which is "YES". Scaled 0-1
# OUTPUT : Frame - dataset
# ************************************************
NscatterPlotLinearDecisions<-function(dataset, fieldNameOutput,xname, threshold)
{

  #Creates a frame with a single column of the predictor input field
  topredict<-data.frame(dataset[,xname])
  names(topredict)<-xname

  #These are the real values, that we scale between 0-1
  # x-min / max-min
  v<-dataset[,fieldNameOutput]
  minv<-min(v)
  maxv<-max(v)

  known_values<-(v-minv)/(maxv-minv)

  scaleThreshold<-(threshold-minv)/(maxv-minv)

  #Actual classes, split by the threshold
  NplotDecisions(fieldNameOutput, known_values, topredict, scaleThreshold,"Linear Classifier")

  #Show the threshold, over this dotted line should classify into class 1
  abline(h=scaleThreshold,col="blue",lty=2)

  #Create small data frame with just the y and x values
  dframe<-data.frame(known_values,topredict)
  names(dframe)[1]=fieldNameOutput

  #Build the linear model
  formular<-paste(fieldNameOutput,"~",xname)
  linearModel<-lm(formular,data=dframe)

  #Show model line on graph - we need to rescale to the decision scale
  topredict<-data.frame(seq(min(dataset[,xname]),max(dataset[,xname]),by=.1))
  names(topredict)<-xname
  y_predicted<-predict(linearModel, topredict)

  #y_predicted_01<-rescale(y_predicted,range(0,1))

  #This shows the line that the model will use to seperate the classes
  lines(topredict[,xname],y_predicted,col="blue",lwd=4)
  lines(topredict[,xname],y_predicted>scaleThreshold,pch=4,col="black",lty=2,lwd=2)

}

# ************************************************
# Logistic classifier - single predictor
# INPUT:      Frame - dataset
#             String - name of field to predict
#             String - name of single predictor field
# OUTPUT :    None
# ************************************************
NscatterPlotLogisticRegression<-function(dataset,fieldNameOutput,xname,threshold)
{

  #Scale between 0-1
  # x-min / max-min
  v<-dataset[,xname]
  minv<-min(v)
  maxv<-max(v)
  x<-(v-minv)/(maxv-minv)

  v<-dataset[,fieldNameOutput]
  minv<-min(v)
  maxv<-max(v)
  y<-(v-minv)/(maxv-minv)

  scaleThreshold<-(threshold-minv)/(maxv-minv)

  topredict<-data.frame(x)

  #These show the classification of either HIGH or LOW house prices
  NplotDecisions("x", y, topredict, 0.5, "Logistic Classifier")
  abline(h=scaleThreshold,col="blue",lty=2)

  #Build a logistic regression classifier
  dframe<-data.frame(x,y)
  formular<-paste("y~x")
  logisticModel<-glm(formular,data=dframe,family=binomial)

  #creates prediction for an entire x range
  topredict<-data.frame(seq(0,1,.01))
  names(topredict)<-"x"

  #Now generate predictions on  range and plot to visulise the fitted model
  y_predicted<-predict(logisticModel, topredict,type="response")
  scatter<-data.frame(topredict,y_predicted)
  lines(scatter[,"x",],scatter[,"y_predicted"],pch=4,col="blue",lwd=4 )
  lines(scatter[,"x",],scatter[,"y_predicted"]>scaleThreshold,pch=4,col="black",lty=2,lwd=2)

  return(logisticModel)
}


# ***************************************************
# Various metrics : Calculate various confusion matrix metrics
# INPUT: TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
# OUTPUT : float - calculated results
# ***************************************************

NcalcAccuracy<-function(TP,FP,TN,FN){return(100.0*((TP+TN)/(TP+FP+FN+TN)))}
NcalcPgood<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FP)))}
NcalcPbad<-function(TP,FP,TN,FN){return(100.0*(TN/(FN+TN)))}
NcalcFPR<-function(TP,FP,TN,FN){return(100.0*(FP/(FP+TN)))}
NcalcTPR<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FN)))}
NcalcMCC<-function(TP,FP,TN,FN){return( ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))}

# ***************************************************
# NcalcConfusion() : Calculate a confusion matrix for 2-class classifier
# INPUT: vector - output from table()
#
# OUTPUT: A list with the following entries:
#        TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
#        accuracy - float - accuracy metric
#        pgood - float - precision for "good" (values are 1) metric
#        pbad - float - precision for "bad" (values are 1) metric
#        FPR - float - FPR metric
#        TPR - float - FPR metric
# ***************************************************

NcalcConfusion<-function(confusion){

  TP<-confusion[2,2]
  FN<-confusion[1,2]
  FP<-confusion[2,1]
  TN<-confusion[1,1]

  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=NcalcAccuracy(TP,FP,TN,FN),
                  "pgood"=NcalcPgood(TP,FP,TN,FN),
                  "pbad"=NcalcPbad(TP,FP,TN,FN),
                  "FPR"=NcalcFPR(TP,FP,TN,FN),
                  "TPR"=NcalcTPR(TP,FP,TN,FN),
                  "mcc"=NcalcMCC(TP,FP,TN,FN)
  )
  return(retList)
}
