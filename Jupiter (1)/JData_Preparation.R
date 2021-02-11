# ************************************************
# Coursework - Preparing data for machine learning
# based on real-world dataset
#
# DATE: 24th November 2019
# VERSION: v1.00
# AUTHOR: Graham Hay
#
# UPDATE
# 1.00 06/11/2019 Initial Version
# ************************************************

# ************************************************
# Perfom data preparation operations on a dataframe
# containing damages data from the UNCTAD web site.
# INPUT:   rawCases - data frame
# OUTPUT : prepared data - data frame
# ************************************************

MAX_LITERALS      <- 55                    # Maximum numner of hotcoding new fields

# ************************************************
# Create a dataframe of case data with 
# the required fields  encoded as necessary
# INPUT:   rawCases - data frame
# OUTPUT:  caseData - prepared data frame
# ************************************************

JdataPreparation<-function(rawCases) {
  
  # ************************************************
  # Exclude data points by Outcome

  filteredCases<-rawCases[which(rawCases[,"Outcomeoforiginalproceedings"] != "Data not available"),]
  filteredCases<-filteredCases[which(filteredCases[,"Outcomeoforiginalproceedings"] != "Pending"),]
  
  
  # ************************************************
  # Initially include Ordinal fields
  
  cases<-filteredCases[, (names(filteredCases) %in% c("Arbitralrules", "Breachesalleged",
                                                      "Economicsector", "aPresidentC",
                                                      "aRespondentC", "aClaimantC",
                                                      "RespondentHDI", "ClaimantHDI",
                                                      "Amountclaimed", "Amountawardedorsettledfor",
                                                      "Outcomeoforiginalproceedings"))]
 
  
  # ************************************************
  # Fill empty values
  
  cases<-JfillNullValues(cases, "Amountclaimed", 0.0)
  cases<-JfillNullValues(cases, "Amountawardedorsettledfor", 0.0)
  
  cases<-JfillNaValues(cases, "ClaimantHDI", mean(cases[,"ClaimantHDI"], na.rm=TRUE))
  cases<-JfillNaValues(cases, "RespondentHDI", mean(cases[,"RespondentHDI"], na.rm=TRUE))
  
  cases<-JfillNaValues(cases, "aPresidentC", "")
  cases<-JfillNaValues(cases, "aClaimantC", "")
  cases<-JfillNaValues(cases, "aRespondentC", "")
  
  # ************************************************
  # 1-hot-encode fields
  

  rules<-JOneHotEncode(cases, "Arbitralrules")
  breaches<-JOneHotEncodeValues(cases, "Breachesalleged", c("Direct expropriation"))
  sector<-JOneHotEncodeValues(cases, "Economicsector", c("Primary: B - Mining", "Tertiary: D - Electricity", "Tertiary: F - Construction"))
  presidents<-JOneHotEncodeValues(cases, "aPresidentC", c("Kaufmann-Kohler, G.", "Armesto, J.", "Veeder, V. V.", "Fortier, L. Y.", "Tercier, P."))
  claimants<-JOneHotEncodeValues(cases, "aClaimantC", c("Brower, C. N.","Alexandrov, S. A.","Fortier, L. Y.","Orrego Vic", "Grigera","Beechey, J","Hanotiau, B."))
  respondents<-JOneHotEncodeValues(cases, "aRespondentC", c("Stern, B.","Thomas, J. C","Sands, P","Douglas, Z.","Landau, T."))

  # ************************************************
  # Output field - Success
  success<-ifelse ( (stri_count_fixed(cases[,"Outcomeoforiginalproceedings"],"Decided in favour of investor") > 0) |
                    (stri_count_fixed(cases[,"Outcomeoforiginalproceedings"],"Settled") > 0), 1, 0)
  caseData<-as.data.frame(success)
  caseData<-cbind(caseData, rules)
  caseData<-cbind(caseData, breaches)
  caseData<-cbind(caseData, sector)
  caseData<-cbind(caseData, presidents)
  caseData<-cbind(caseData, claimants)
  caseData<-cbind(caseData, respondents)
  caseData<-cbind(caseData, cases[,"ClaimantHDI"])
  colnames(caseData)[ncol(caseData)]<-"ClaimantHDI"
  caseData<-cbind(caseData, cases[,"RespondentHDI"])
  colnames(caseData)[ncol(caseData)]<-"RespondentHDI"
  
  return(caseData)
}

# ************************************************
# Fill null values in a field of a dataframe 
# with the specified value.
# INPUT:   df - data frame
#          fieldName - field name
#          value - replacement value
# ************************************************
JfillNullValues<-function(df, fieldName, value) {
  df[,fieldName] <- as.numeric(as.character(df[,fieldName]))
  df[,fieldName][is.na(df[,fieldName])] <- value
  return(df)
}

# ************************************************
# Fill na values in a field of a dataframe 
# with the specified value.
# INPUT:   df - data frame
#          fieldName - field name
#          value - replacement value
# ************************************************
JfillNaValues<-function(df, fieldName, value) {
  df[,fieldName][is.na(df[,fieldName])] <- value
  return(df)
}



# ************************************************
# 1-hot-encode a field in a dataframe
# INPUT:   dataset - data frame
#          field - field name
# Derived from NPREPROCESSING_categorical by 
# Prof. Nick F Ryman-Tubb
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
# ************************************************
JOneHotEncodeValues<-function(dataset, field, literals) {
  
  #This is a dataframe of the transformed categorical fields
  catagorical<-data.frame(first=rep(NA,nrow(dataset)),stringsAsFactors=FALSE)
  
  # Literals passed in
  numberLiterals<-length(literals)
  
  #if there are just two literals in the field we can convert to 0 and 1
  if (length(literals)==1){
    transformed<-ifelse ((stri_count_fixed(dataset[,field],literals[1]) > 0), 1.0,0.0)
    catagorical<-cbind(catagorical,transformed)
    colnames(catagorical)[ncol(catagorical)]<-paste(field,
                                                    "_",
                                                    NPREPROCESSING_removePunctuation(literals[1]),
                                                    sep="")
    
  } else
  {
    #We have now to one-hot encoding FOR SMALL NUMBER of literals
    if (numberLiterals<=MAX_LITERALS){
      for(num in 1:numberLiterals){
        nameOfLiteral<-literals[num]
        hotEncoding<-ifelse ((stri_count_fixed(dataset[,field],nameOfLiteral) > 0), 1.0,0.0)
        
        # 5/3/2018 - do not convert the field if their are too few literals
        # Use log of number of recrods as the measure
        literalsActive<-sum(hotEncoding==1)
        if (literalsActive>log(length(hotEncoding))) {
          catagorical<-cbind(catagorical,hotEncoding)
          #060819 field name has the "_" seperator to make easier to read
          colnames(catagorical)[ncol(catagorical)]<-paste(field,
                                                          "_",
                                                          NPREPROCESSING_removePunctuation(nameOfLiteral),
                                                          sep="")
        }
        else {
          print(paste("Ignoring in field:",names(dataset)[field],
                      "Literal:",nameOfLiteral,
                      "Too few=",literalsActive))
        }
      }
    } else {
      stop(paste("Error - too many literals in:",names(dataset)[field], numberLiterals))
    }
  }
  return(catagorical[,-1]) #Remove that first column that was full of NA due to R
}

# ************************************************
# 1-hot-encode a field in a dataframe
# INPUT:   dataset - data frame
#          field - field name
# Derived from NPREPROCESSING_categorical by 
# Prof. Nick F Ryman-Tubb
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
# ************************************************
JOneHotEncode<-function(dataset, field) {
  
  #This is a dataframe of the transformed categorical fields
  catagorical<-data.frame(first=rep(NA,nrow(dataset)),stringsAsFactors=FALSE)

  
  #Create a list of unique values in the field (each is a literal)
  literals<-as.vector(unique(dataset[,field]))
  numberLiterals<-length(literals)
  
  #if there are just two literals in the field we can convert to 0 and 1
  if (numberLiterals==2){
    transformed<-ifelse (dataset[,field]==literals[1],0.0,1.0)
    catagorical<-cbind(catagorical,transformed)
    colnames(catagorical)[ncol(catagorical)]<-paste(field,
                                                    "_",
                                                    NPREPROCESSING_removePunctuation(literal[1]),
                                                    sep="")
    
  } else
  {
    #We have now to one-hot encoding FOR SMALL NUMBER of literals
    if (numberLiterals<=MAX_LITERALS){
      for(num in 1:numberLiterals){
        nameOfLiteral<-literals[num]
        hotEncoding<-ifelse (dataset[,field]==nameOfLiteral,1.0,0.0)
        
        # 5/3/2018 - do not convert the field if their are too few literals
        # Use log of number of recrods as the measure
        literalsActive<-sum(hotEncoding==1)
        if (literalsActive>log(length(hotEncoding))) {
          catagorical<-cbind(catagorical,hotEncoding)
          #060819 field name has the "_" seperator to make easier to read
          colnames(catagorical)[ncol(catagorical)]<-paste(field,
                                                          "_",
                                                          NPREPROCESSING_removePunctuation(nameOfLiteral),
                                                          sep="")
        }
        else {
          print(paste("Ignoring in field:",field,
                      "Literal:",nameOfLiteral,
                      "Too few=",literalsActive))
        }
      }
    } else {
      stop(paste("Error - too many literals in:",field, numberLiterals))
    }
  }
  return(catagorical[,-1]) #Remove that first column that was full of NA due to R
}
