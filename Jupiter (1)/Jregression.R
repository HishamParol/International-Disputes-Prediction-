rm(list=ls())

MYLIBRARIES<-c("corrplot",
               "RColorBrewer",
               "ggplot2",
               "ggpubr")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#------------------------------------------------------------------------------
CSV_DATA <-"Regression_data.csv"
Jupiter<- read.csv(CSV_DATA)
#------------------------------------------------------------------------------
#HISTOGRAM PLOTS
#------------------------------------------------------------------------------
#HDI CLAIMANT 
hist(Jupiter$hDIClaim, main="Histogram of HDI Claimant", xlab="HDI Claimant", breaks=10,border="red",col="navy blue") 

#HDI RESPONDENT
hist(Jupiter$hDIResp , main="Histogram of HDI Respondent", xlab="HDI Respondent", breaks=10,border="red",col="navy blue") 
#------------------------------------------------------------------------------
#Scatter Plots
#------------------------------------------------------------------------------
#Amount Claimed VS Amount Awarded
plot(Jupiter$amountClaimed, Jupiter$amountAwarded,main = "Scatter Plot", xlab = "Amount Claimed", ylab = "Amount Awarded", type="p", col = "blue") 
#HDI RESPONDENT VS AMOUNT AWARDED
plot(Jupiter$hDIResp, Jupiter$amountAwarded,main = "Scatter Plot", xlab = "Amount Claimed", ylab = "HDI Respondent", type="p", col = "blue") 
#------------------------------------------------------------------------------
#Correlation
#------------------------------------------------------------------------------
pearson_correlation_function <- function(field1, field2){
  cor.test(field1, field2, use="complete.obs", method ="pearson")
}

#Amount Claimed VS HDI Claim
pearson_correlation_function(Jupiter$amountClaimed,Jupiter$hDIClaim)
 

#Amount Awarded VS Amount Claimed
pearson_correlation_function(Jupiter$amountAwarded,Jupiter$amountClaimed)


#Damages Ratio VS HDI Claim
pearson_correlation_function(Jupiter$damagesRatio,Jupiter$hDIClaim)


#Damages Ratio VS HDI Respondent
pearson_correlation_function(Jupiter$damagesRatio,Jupiter$hDIResp)


#----------------------------------------------------------------------------------
#CORRELATION PLOT
#----------------------------------------------------------------------------------
#Amount Claimed vs HDI Claim
ggscatter(Jupiter, x = "amountClaimed", y = "hDIClaim", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "AMOUNT CLAIMED", ylab = "HDI CLAIMANT")

#HDI Respondent vs Damages ratio
ggscatter(Jupiter, x = "hDIResp", y = "damagesRatio", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "HDI RESPONDENT", ylab = "DAMAGES RATIO")

#HDI claim vs Damages ratio
ggscatter(Jupiter, x = "hDIClaim", y = "damagesRatio", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "HDI CLAIMANT", ylab = "DAMAGES RATIO")

#HDI claim vs Damages ratio
ggscatter(Jupiter, x = "amountClaimed", y = "amountAwarded", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "AMOUNT CLAIMED", ylab = "AMOUNT AWARDED")

#----------------------------------------------------------------------------------
#Linear Regression Plots
#----------------------------------------------------------------------------------
AMOUNT_CLAIMED<-Jupiter$amountClaimed
AMOUNT_AWARDED<-Jupiter$amountAwarded
HDI_CLAIMED<-Jupiter$hDIClaim
HDI_RESPONDENT<-Jupiter$hDIResp
PREDICTED<-Jupiter$predicted
RESIDUALS<-Jupiter$residuals
DAMAGESRATIO<-Jupiter$damagesRatio

#Amountawarded vs HDI Claimant
linearregression<-lm(AMOUNT_AWARDED~HDI_CLAIMED)
summary(linearregression)
plot(HDI_CLAIMED, AMOUNT_AWARDED,type= "p",main= "Fitted Line Plot",xlab="HDI Respondent",ylab="Amount Awarded",col = "dark red")
abline(linearregression, col = 'blue')

PREDICTED <- predict(linearregression)  
RESIDUALS <- residuals(linearregression)

#Residual Plot
ggplot(Jupiter, aes(x = (HDI_CLAIMED), y = (AMOUNT_AWARDED))) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Plot regression slope
  geom_segment(aes(xend = HDI_CLAIMED, yend = (PREDICTED)), alpha = .2, color="red") +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = (PREDICTED)), shape = 1, color="brown") +
  theme_bw()
#---------------------------------------------------------------------------------------

#Amountawarded vs HDI Respondent
linearregression<-lm(AMOUNT_AWARDED~HDI_RESPONDENT)
summary(linearregression)
plot(HDI_RESPONDENT, AMOUNT_AWARDED,type= "p",main= "Fitted Line Plot",xlab="HDI Respondent",ylab="Amount Awarded",col = "dark red")
abline(linearregression, col = 'blue')

PREDICTED <- predict(linearregression)  
RESIDUALS <- residuals(linearregression)

#Residual Plot
ggplot(Jupiter, aes(x = (HDI_RESPONDENT), y = (AMOUNT_AWARDED))) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Plot regression slope
  geom_segment(aes(xend = HDI_RESPONDENT, yend = (PREDICTED)), alpha = .2, color="red") +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = (PREDICTED)), shape = 1, color="brown") +
  theme_bw()
#---------------------------------------------------------------------------------------
#Amountawarded vs Amount Claimed
linearregression<-lm(AMOUNT_AWARDED~AMOUNT_CLAIMED)
summary(linearregression)
plot(AMOUNT_CLAIMED, AMOUNT_AWARDED,type= "p",main= "Fitted Line Plot",xlab="Amount Claimed",ylab="Amount Awarded",col = "dark red")
abline(linearregression, col = 'blue')

PREDICTED <- predict(linearregression)  
RESIDUALS <- residuals(linearregression)

#Residual Plot
ggplot(Jupiter, aes(x = (AMOUNT_CLAIMED), y = (AMOUNT_AWARDED))) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Plot regression slope
  geom_segment(aes(xend = AMOUNT_CLAIMED, yend = (PREDICTED)), alpha = .2, color="red") +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = (PREDICTED)), shape = 1, color="brown") +
  theme_bw()
#-------------------------------------------------------------------------------
#HDI CLAIM vs DAMAGESRATIO
linearregression<-lm(DAMAGESRATIO~HDI_CLAIMED)
summary(linearregression)
plot(HDI_CLAIMED, DAMAGESRATIO,type= "p",main= "Fitted Line Plot",xlab="Amount Claimed",ylab="Amount Awarded",col = "dark red")
abline(linearregression, col = 'blue')

PREDICTED <- predict(linearregression)  
RESIDUALS <- residuals(linearregression)

#Residual Plot
ggplot(Jupiter, aes(x = (HDI_CLAIMED), y = (DAMAGESRATIO))) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Plot regression slope
  geom_segment(aes(xend = HDI_CLAIMED, yend = (PREDICTED)), alpha = .2, color="red") +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = (PREDICTED)), shape = 1, color="brown") +
  theme_bw() 
#------------------------------------------------------------------------------------
#HDI RESPONDENT vs DAMAGESRATIO
linearregression<-lm(DAMAGESRATIO~HDI_RESPONDENT)
summary(linearregression)
plot(HDI_RESPONDENT, DAMAGESRATIO,type= "p",main= "Fitted Line Plot",xlab="Amount Claimed",ylab="Amount Awarded",col = "dark red")
abline(linearregression, col = 'blue')

PREDICTED <- predict(linearregression)  
RESIDUALS <- residuals(linearregression)

#Residual Plot
ggplot(Jupiter, aes(x = (HDI_RESPONDENT), y = (DAMAGESRATIO))) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Plot regression slope
  geom_segment(aes(xend = HDI_RESPONDENT, yend = (PREDICTED)), alpha = .2, color="red") +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = (PREDICTED)), shape = 1, color="brown") +
  theme_bw()

# Outlier Removal
# referred https://youtu.be/6hRKlZ8D_mk
# boxplot(field) # box plot the field
# summary(field) # Print the summary
# bench = Q3 + 1.5 * IQR(field) # Q3 is the 3rd quartile field and stores the values to bench
# field <- field[field < bench] # Updated the field and removed the outliers.
#----------------------------------------------------------------------------------------

