# ***************************
# Data Pre Processing

# Loading the damages and hdi csv

# ************
# Jdatacleaning Reads the damages and hdi csv files and:
#  - Standaradised the amounts field
#  - Splitting the arbitrators to seperate column
#  - Splitting the state field and taking the first country name
#  - Updated the HDI values 
# ************
Jdatacleaning<-function(){
  print("Data Cleaning")
  damages<-read.csv(DAMAGES_FILENAME, check.names = FALSE, stringsAsFactors = FALSE)
  hdi<-read.csv(HDI_FILENAME, check.names = FALSE, stringsAsFactors = FALSE)
  damages <- amount_standardised(damages, fieldName = 'Amount claimed')
  damages <- amount_standardised(damages, fieldName = "Amount awarded (or settled for)" )
  damages <- arbitrator_split(damages, fieldName = 'Arbitrators')
  damages <- CountrySplit(damages, hdi, fieldName1 = "Respondent State", fieldName2 = "Country")
  damages <- CountrySplit(damages, hdi, fieldName1 = "Home State of investor", fieldName2 = "Country")
  damages <- update_hdi_value(damages, hdi, fieldName1 = "Home State of investor", fieldName2 = "Claimant HDI")
  damages <- update_hdi_value(damages, hdi, fieldName1 = "Respondent State", fieldName2 = "Respondent HDI")
  write.csv(damages, file=DATASET_FILENAME, row.names = FALSE, fileEncoding="UTF-8")
}

# Standardising the structure of amount feild
amount_standardised<-function(dataFrame, fieldName) {
  for (i in 1:nrow(dataFrame)){
    if (str_detect(dataFrame[[fieldName]][i],"\\(")){
      new_amount = gsub("[\\(\\)]","",regmatches(dataFrame[[fieldName]][i], gregexpr("\\(.*?\\)",dataFrame[[fieldName]][i]))[[1]])
      dataFrame[[fieldName]][i]<-new_amount
    }
    if(dataFrame[[fieldName]][i] == "Data not available") {
      dataFrame[[fieldName]][i]<-"Null"
    }
    dataFrame[[fieldName]][i]<-word(dataFrame[[fieldName]][i], 1)
  }
  return(dataFrame)
}

# This function returns the string preceeding the keyword
string_split <- function(stringToSplit, keyword) {
  return(unlist(strsplit(stringToSplit, keyword)))
}

# add separator
add_separator <- function(stringToConsider, conditionsToCheck, replaceText, replaceCompletely = TRUE) {
  for (ctc in conditionsToCheck) {
    UpdatedReplaceText = ifelse(replaceCompletely, replaceText, paste(ctc, replaceText, sep=""))
    stringToConsider = gsub(ctc, UpdatedReplaceText, stringToConsider, ignore.case = TRUE)
  }
  return(stringToConsider)
}

# Reverse a string character by character
rev_char <- function(stringToReverse) {
  splitString = strsplit(as.character(stringToReverse), split = "")
  reversed_chr = splitString[[1]][nchar(stringToReverse):1]
  reversed_str = paste(reversed_chr, collapse = "")
  return(reversed_str)
}

# Delete a set of text following a pattern in reverse order
delete_text_follows_pattern <- function(stringToConsider, patternBegin, patternEnd) {
  patternToDelete = paste(patternEnd, "(.*?)", patternBegin, sep="")
  editedString = sub(patternToDelete, "111", rev_char(stringToConsider))
  return(rev_char(editedString))
}

# Function to split the Arbitrators President, Claimant and Respondent to separte columns
arbitrator_split <- function(dataFrame, fieldName) {
  aPresidentC = c()
  aRespondentC = c()
  aClaimantC = c()
  for (i in 1:nrow(dataFrame)){
    toClean = c("claimant \\(replaced\\)", "president \\(replaced\\)", "respondent \\(replaced\\)")
    cleanedArbitrators = add_separator(dataFrame[[fieldName]][i], toClean, "999")
    toSeperate = c("claimant", "president", "respondent")
    cleanedArbitrators = add_separator(cleanedArbitrators, toSeperate, "111", replaceCompletely = FALSE)
    cleanedArbitrators = paste("111", cleanedArbitrators, sep = "")
    cleanedArbitrators = delete_text_follows_pattern(cleanedArbitrators, "111", "999")
    # President
    aPresident <- str_match(rev_char(cleanedArbitrators), "111tnediserp - (.*?)111")[,2]
    aPresidentC = c(aPresidentC, ifelse(is.na(aPresident), "NA", rev_char(aPresident)))
    # Respondent
    aRespondent <- str_match(rev_char(cleanedArbitrators), "111tnednopser - (.*?)111")[,2]
    aRespondentC = c(aRespondentC, ifelse(is.na(aRespondent), "NA", rev_char(aRespondent)))
    # Claimant
    aClaimant <- str_match(rev_char(cleanedArbitrators), "111tnamialc - (.*?)111")[,2]
    aClaimantC = c(aClaimantC, ifelse(is.na(aClaimant), "NA", rev_char(aClaimant)))
  }
  dataFrame<-cbind(dataFrame, aPresidentC, aRespondentC, aClaimantC)
  return(dataFrame)
}

# Function to clean the country names.
# change the united states of america to united states and
# country name display pattern update
cleanedCountryName <- function(dataFrame1, dataFrame2, fieldName1, fieldName2){
  for(i in 1:nrow(dataFrame1)) {
    flag = 0
    for(j in 1:nrow(dataFrame2)) {
      if(dataFrame1[[fieldName1]][i] == dataFrame2[[fieldName2]][j]) {
        flag = 1 
      }
    }
    if(flag==0) {
      if (dataFrame1[[fieldName1]][i]=="United States of America") {
        dataFrame1[[fieldName1]][i] <- "United States" 
      }
      else {
        if(str_detect(dataFrame1[[fieldName1]][i], '\\,')) {
          dataFrame1[[fieldName1]][i] <- paste(gsub(", ", " (", dataFrame1[[fieldName1]][i]), ")", sep="")
        }
      }
    }
  }
  return(dataFrame1)
}

# Function to compare a coloumn in 2 csv
CountrySplit <- function(dataFrame1, dataFrame2, fieldName1, fieldName2){
  #damages<-read.csv("Damages.csv", stringsAsFactors = FALSE)
  #hdi<-read.csv("HDI.csv", stringsAsFactors = FALSE)
  for(i in 1:nrow(dataFrame1)) {
    flag = 0
    for(j in 1:nrow(dataFrame2)) {
      if(dataFrame1[[fieldName1]][i] == dataFrame2[[fieldName2]][j]) {
        flag = 1 
      }
    }
    if(flag==0) {
      upperCaseChrLoc = str_locate_all(pattern='([[:upper:]])', dataFrame1[[fieldName1]][i])
      len = length(upperCaseChrLoc[[1]])/2
      for (j in 1:len){
        chrloc = upperCaseChrLoc[[1]][[j]]
        splitStr = strsplit(dataFrame1[[fieldName1]][i], "")
        if(chrloc == 1) {
          next()
        }
        else {
          if (splitStr[[1]][chrloc-1] == " "){
            next()
          }
          else {
            firstCountry = substr(dataFrame1[[fieldName1]][i], 1, chrloc-1)
          }
          dataFrame1[[fieldName1]][i] <- firstCountry
          break()
        }
      }
    }
  }
  dataFrame1 <- cleanedCountryName(dataFrame1, dataFrame2, fieldName1, fieldName2)
  return(dataFrame1)
}

# Function to update HDI values
update_hdi_value <- function(dataFrame1, dataFrame2, fieldName1, fieldName2) {
  for(i in 1:nrow(dataFrame1)) {
    countryHdiIndex <- ifelse(is.element(dataFrame1[[fieldName1]][i], dataFrame2$Country), 
                         match(dataFrame1[[fieldName1]][i], dataFrame2$Country), 
                         FALSE)
    countryHdiValue <- ifelse(countryHdiIndex, dataFrame2[['HDI value 2017']][countryHdiIndex], "NA")
    dataFrame1[[fieldName2]][i] <- countryHdiValue
  }
  return(dataFrame1)
}
# ************************ Data Preprocessing End