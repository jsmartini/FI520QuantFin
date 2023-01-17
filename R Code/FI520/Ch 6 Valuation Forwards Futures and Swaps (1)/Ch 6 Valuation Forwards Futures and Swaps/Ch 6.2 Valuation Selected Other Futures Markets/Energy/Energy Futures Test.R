# Energy Futures Test.R
# read.xlsx, plot, lines, legend
# Working with futures prices
# rmarkdown::render("Energy Futures Test.R", "word_document")
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
StartDate = 19941231
EndDate = 20220220
sTitle = expression(paste("12/31/1994 through 12/31/2019"))
FixYRangeTP <- TRUE
MaxYValueTP = 100
MinYValueTP = -100
# Brent
FileName = 'Brent.xlsx'
Title1 = as.character('Brent')
source('Futures Term Premiums 2.R')
# WTI
FileName = 'WTIComb.xlsx'
Title1 = as.character('WTI Crude Oil')
source('Futures Term Premiums 2.R')
# Diesel
FileName = 'GasOil.xlsx'
Title1 = as.character('Diesel')
source('Futures Term Premiums 2.R')
# Unleaded Gas
FileName = 'UnleadedGasComb.xlsx'
Title1 = as.character('Unleaded Gas')
source('Futures Term Premiums 2.R')
# Natural Gas
FileName = 'NGComb.xlsx'
Title1 = as.character('Natural Gas')
source('Futures Term Premiums 2.R')
# Heating Oil
FileName = 'HeatingOilComb.xlsx'
Title1 = as.character('Heating Oil')
source('Futures Term Premiums 2.R')
