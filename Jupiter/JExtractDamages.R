# ************************************************
# Coursework - Preparing data for machine learning
# basedased on real-world dataset
#
# DATE: 6th November 2019
# VERSION: v1.00
# AUTHOR: Graham Hay
#
# UPDATE
# 1.00 06/11/2019 Initial Version
# ************************************************

library(rvest)

# ************************************************
# Parse and extract the data table from a file
# containing the html scraped from the UNCTAD web site.
# INPUT: htmlFile - html file assumed to be in the project folder
# OUTPUT : CSV file - The raw Damages data as a UTF-8 CVS file
# ************************************************

JextractDamages<-function(htmlFile) {
	html <- read_html(htmlFile)
	resultsTable <- html_nodes(html, 'table[data-ajax-url="/investment-dispute-settlement/ajax/cases"]')
	tableDF <- html_table(resultsTable)[[1]]
	dropColumns <- c("NO.","Full case name")
	tableDF <- tableDF[, !(names(tableDF) %in% dropColumns)]
	print(colnames(tableDF))
	write.csv(tableDF, file = "Damages.csv", row.names = FALSE, fileEncoding = "UTF-8")
}
