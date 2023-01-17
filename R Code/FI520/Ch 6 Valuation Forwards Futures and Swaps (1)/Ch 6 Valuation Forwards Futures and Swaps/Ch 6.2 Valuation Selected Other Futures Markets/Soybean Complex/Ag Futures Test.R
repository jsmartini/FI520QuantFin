# Ag Futures Test.R
# read.xlsx, plot, lines, legend
# Working with futures prices
# rmarkdown::render("Ag Futures Test.R", "word_document")
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
FixYRangeTP <- TRUE
FixYRangeTP <- TRUE
if(FixYRangeTP == TRUE){
  MaxYValueTP <- 100
  MinYValueTP <- -100
}
NumberOfNearbys = 3
StartDate = 19941231
EndDate = 20220220
sTitle = expression(paste("12/31/1994 through 12/31/2019"))
FileName = 'Soybeans.xlsx'
Title1 = as.character('Soybeans')
source('Futures Term Premiums 2.R')
FileName = 'SoybeanMeal.xlsx'
Title1 = as.character('Soybean Meal')
source('Futures Term Premiums 2.R')
FileName = 'SoybeanOil.xlsx'
Title1 = as.character('Soybean Oil')
source('Futures Term Premiums 2.R')
FileName = 'Wheat.xlsx'
Title1 = as.character('Wheat')
source('Futures Term Premiums 2.R')
# #
# # Wheat shortage and abundance
# #
# StartDate = 20070401
# EndDate = 20090401
# sTitle = expression(paste("04/01/2007 through 04/01/2009"))
# FileName = 'Soybeans.xlsx'
# Title1 = as.character('Soybeans')
# source('Futures Term Premiums 2.R')
# FileName = 'SoybeanMeal.xlsx'
# Title1 = as.character('Soybean Meal')
# source('Futures Term Premiums 2.R')
# FileName = 'SoybeanOil.xlsx'
# Title1 = as.character('Soybean Oil')
# source('Futures Term Premiums 2.R')
# FileName = 'Wheat.xlsx'
# Title1 = as.character('Wheat')
# source('Futures Term Premiums 2.R')
# 

