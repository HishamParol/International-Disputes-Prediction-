#  clears all objects in "global environment"
rm(list=ls())

# Global Environment variables - i.e. available to all functions

Global_train_inputs <- NA
Global_train_inputs_expected <- NA

# ************************************************
# Good practice to place constants in variables
# I use UPPERCASE to identify these in my code

DAMAGES_FILENAME  <- "Damages.csv" # Name of damages raw csv file
HDI_FILENAME      <- "HDI.csv" # HDI index per state
DATASET_FILENAME  <- "Damages_Clean.csv"  #Name of input dataset file
OUTPUT_FIELD      <- "success"            # Field name of the output class to predict

# These are the data preparation values

HOLDOUT           <- 80                   # % split to create TRAIN dataset

# Cutoff values - you can experiment with these

CUTOFF_OUTLIER    <- 0.99                 # Confidence p-value for outlier detection
# Set to negative means analyse but do not replace outliers
CUTOFF_DISCREET   <- 5                    # Number of empty bins to determine discreet
CUTOFF_REDUNDANT  <- 0.95                 # Linear correlation coefficient cut-off

# Indicates the type of each field

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

MAX_LITERALS      <- 55                    # Maximum numner of 1-hot-encoding fields

# These are the supervised model constants

SUMMARY_FILENAME  <- "Summary.txt"        # Name of text file containing summary
VARBAR_FILENAME   <- "ImportanceBar.pdf"  # Name of PDF file containing Importance Bar chart
VARIMP_FILENAME   <- "ImportancePlot.pdf" # Name of text file containing Importance Plot
PDF_FILENAME      <- "Tree.pdf"           # Name of PDF with graphical tree diagram
ROBJECT_FILENAME  <- "Model.rds"          # Name of saved R model
RULES_FILENAME    <- "Rules.txt"          # Name of text file with rules saved
RESULTS_FILENAME  <- "Results.csv"        # Name of the CSV results file
NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 1000                 # Number of trees in the forest
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage

BASICNN_HIDDEN    <- 10                   # 10 hidden layer neurons
BASICNN_EPOCHS    <- 100                  # Maximum number of training epocs


DEEP_HIDDEN       <- c(5,5)               # Number of neurons in each layer
DEEP_STOPPING     <- 2                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.01                 # Error threshold
DEEP_ACTIVATION   <- "TanhWithDropout"    # Non-linear activation function
DEEP_REPRODUCABLE <- TRUE                 # Set to TRUE to test training is same for each run


MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "pROC",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "stringi",
               "stringr",
               "partykit",
               "C50",
               "randomForest",
               "keras",
               "h2o",
               "dplyr")

KFOLDS          <- 6                 # Number of folded experiments

main<-function(){
  
  # Data cleaning function
  Jdatacleaning()
  
  # Run the models
  modelling()
  
  # Load the chosen model
  rf<-loadModel()
  
}


# ************************************************
# This is where R starts execution

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

print("START Supervised Machine Learning")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("lab4DataPrep.R")
source("lab4Functions.R")
source("JData_Preparation.R")
source("JFunctions.R")
source("JModels.R")

set.seed(123)

# ************************************************
main()

print("end")
