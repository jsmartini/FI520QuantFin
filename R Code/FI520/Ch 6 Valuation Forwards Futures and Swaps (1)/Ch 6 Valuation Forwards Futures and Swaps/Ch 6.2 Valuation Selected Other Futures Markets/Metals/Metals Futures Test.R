# Metals Futures Test.R
# read.xlsx, plot, lines, legend
# Working with futures prices
# rmarkdown::render("Metals Futures Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
defaultpar <- par() # plot global parameters
# Libraries
#  stats: 
#  date:
#  openxlsx: MS Excel file management
#  PerformanceAnalytics: Higher moments
Packages <- c("stats", "date", "openxlsx", "PerformanceAnalytics") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Examine relationship between term premiums and 3M Libor
#
NumberOfNearbys = 3
# FileName = 'Gold2.xlsx'
# Title1 = as.character('Gold')
# # Files have dates in strange format
# StartDate = 19931231
# EndDate = 20181231
# sTitle = expression(paste("12/31/1993 through 12/31/2018"))
# source('Futures Term Premiums 2.R')
# FileName = 'Silver.xlsx'
# Title1 = as.character('Silver')
# # Files have dates in strange format
# StartDate = 19931231
# EndDate = 20181231
# sTitle = expression(paste("12/31/1993 through 12/31/2018"))
# source('Futures Term Premiums 2.R')
# FileName = 'Copper.xlsx'
# Title1 = as.character('Copper')
# # Files have dates in strange format
# StartDate = 19931231
# EndDate = 20181231
# sTitle = expression(paste("12/31/1993 through 12/31/2018"))
# source('Futures Term Premiums 2.R')
#
# Last five years
#
FixYRangeTP <- FALSE
if(FixYRangeTP == TRUE){
  MaxYValueTP <- 10
  MinYValueTP <- -20
}
# Whole period
StartDate = 19870401
EndDate = 20220801
sTitle = expression(paste("04/01/1987 through 10/09/2020"))
# Gold
FileName = 'Gold2.xlsx'
Title1 = as.character('Gold')
source('Futures Term Premiums 2.R')
# Silver
FileName = 'Silver.xlsx'
Title1 = as.character('Silver')
source('Futures Term Premiums 2.R')
# Copper
FileName = 'Copper.xlsx'
Title1 = as.character('Copper')
source('Futures Term Premiums 2.R')
# # Five year subperiod
# StartDate = 19891231
# EndDate = 19941231
# sTitle = expression(paste("12/31/1989 through 12/31/1994"))
# # Gold
# FileName = 'Gold2.xlsx'
# Title1 = as.character('Gold')
# source('Futures Term Premiums 2.R')
# # Silver
# FileName = 'Silver.xlsx'
# Title1 = as.character('Silver')
# source('Futures Term Premiums 2.R')
# # Copper
# FileName = 'Copper.xlsx'
# Title1 = as.character('Copper')
# source('Futures Term Premiums 2.R')
# # Five year subperiod
# StartDate = 19941231
# EndDate = 19991231
# sTitle = expression(paste("12/31/1994 through 12/31/1999"))
# # Gold
# FileName = 'Gold2.xlsx'
# Title1 = as.character('Gold')
# source('Futures Term Premiums 2.R')
# # Silver
# FileName = 'Silver.xlsx'
# Title1 = as.character('Silver')
# source('Futures Term Premiums 2.R')
# # Copper
# FileName = 'Copper.xlsx'
# Title1 = as.character('Copper')
# source('Futures Term Premiums 2.R')
# # Five year subperiod
# StartDate = 19991231
# EndDate = 20041231
# sTitle = expression(paste("12/31/1999 through 12/31/2004"))
# # Gold
# FileName = 'Gold2.xlsx'
# Title1 = as.character('Gold')
# source('Futures Term Premiums 2.R')
# # Silver
# FileName = 'Silver.xlsx'
# Title1 = as.character('Silver')
# source('Futures Term Premiums 2.R')
# # Copper
# FileName = 'Copper.xlsx'
# Title1 = as.character('Copper')
# source('Futures Term Premiums 2.R')
# # Five year subperiod
# StartDate = 20041231
# EndDate = 20091231
# sTitle = expression(paste("12/31/2004 through 12/31/2009"))
# # Gold
# FileName = 'Gold2.xlsx'
# Title1 = as.character('Gold')
# source('Futures Term Premiums 2.R')
# # Silver
# FileName = 'Silver.xlsx'
# Title1 = as.character('Silver')
# source('Futures Term Premiums 2.R')
# # Copper
# FileName = 'Copper.xlsx'
# Title1 = as.character('Copper')
# source('Futures Term Premiums 2.R')
# # Five year subperiod
# StartDate = 20091231
# EndDate = 20141231
# sTitle = expression(paste("12/31/2009 through 12/31/2014"))
# # Gold
# FileName = 'Gold2.xlsx'
# Title1 = as.character('Gold')
# source('Futures Term Premiums 2.R')
# # Silver
# FileName = 'Silver.xlsx'
# Title1 = as.character('Silver')
# source('Futures Term Premiums 2.R')
# # Copper
# FileName = 'Copper.xlsx'
# Title1 = as.character('Copper')
# source('Futures Term Premiums 2.R')
# # Last Five Years
# FixYRangeTP <- TRUE
# if(FixYRangeTP == TRUE){
#   MaxYValueTP <- 5
#   MinYValueTP <- -5
# }
# 
# StartDate = 20141231
# EndDate = 20191231
# sTitle = expression(paste("12/31/2014 through 12/31/2019"))
# # Gold
# FileName = 'Gold2.xlsx'
# Title1 = as.character('Gold')
# source('Futures Term Premiums 2.R')
# # Silver
# FileName = 'Silver.xlsx'
# Title1 = as.character('Silver')
# source('Futures Term Premiums 2.R')
# # Copper
# FileName = 'Copper.xlsx'
# Title1 = as.character('Copper')
# source('Futures Term Premiums 2.R')

