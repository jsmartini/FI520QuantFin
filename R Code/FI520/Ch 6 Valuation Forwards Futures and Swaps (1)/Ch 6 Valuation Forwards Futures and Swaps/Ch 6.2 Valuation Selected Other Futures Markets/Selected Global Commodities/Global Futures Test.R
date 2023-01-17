# Global Futures Test.R
# read.xlsx, plot, lines, legend
# Working with futures prices
# rmarkdown::render("Global Futures Test.R", "word_document")
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
if(FixYRangeTP == TRUE){
  MaxYValueTP = 25
  MinYValueTP = -25
}
NumberOfNearbys = 3

# Lumber
if(FixYRangeTP == TRUE){
  MaxYValueTP = 50
  MinYValueTP = -50
}

StartDate = 19870401 #20010310
EndDate = 20220220
sTitle = expression(paste("04/01/1987 through 03/10/2021"))
Currency <- 'USD'
FileName = 'Lumber.xlsx'
Title1 = as.character('U.S. Lumber')
source('Futures Term Premiums 2.R')



# Gold
StartDate = 20141231
EndDate = 20220220
sTitle = expression(paste("12/31/2014 through 02/20/2022"))
Currency <- 'CNY'
FileName = 'CNYGold.xlsx'
Title1 = as.character('China Gold')
source('Futures Term Premiums 2.R')
Currency <- 'USD'
FileName = 'Gold2.xlsx'
Title1 = as.character('U.S. Gold')
source('Futures Term Premiums 2.R')
# Silver
# StartDate = 20131231
# EndDate = 20181231
# sTitle = expression(paste("12/31/2013 through 12/31/2018"))
Currency <- 'CNY'
FileName = 'CNYSilver.xlsx'
Title1 = as.character('China Silver')
source('Futures Term Premiums 2.R')
Currency <- 'USD'
FileName = 'Silver.xlsx'
Title1 = as.character('U.S. Silver')
source('Futures Term Premiums 2.R')
# StartDate = 20031231
# EndDate = 20181231
# sTitle = expression(paste("12/31/12003 through 12/31/2018"))
Currency <- 'CNY'
FileName = 'CNYCopper.xlsx'
Title1 = as.character('China Copper')
source('Futures Term Premiums 2.R')
Currency <- 'USD'
FileName = 'Copper.xlsx'
Title1 = as.character('U.S. Copper')
source('Futures Term Premiums 2.R')


