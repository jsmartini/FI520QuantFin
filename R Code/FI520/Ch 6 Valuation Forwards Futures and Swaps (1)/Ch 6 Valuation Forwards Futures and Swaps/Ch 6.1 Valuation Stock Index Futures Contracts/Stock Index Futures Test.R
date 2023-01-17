# Stock Index Futures Test.R
# read.xlsx, plot, lines, legend
# Working with futures prices
# rmarkdown::render("Stock Index Futures Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Technical plot manipulations require resetting back to default
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
NumberOfNearbys = 3 # Spot, nearby, and third nearby
FileName = 'MiniSPX.xlsx'
Title1 = as.character('S&P 500')
# # Files have dates in strange format
# StartDate = 19941231
# EndDate = 20210303
# sTitle = expression(paste("12/31/1994 through 12/31/2019"))
# source('Futures Term Premiums.R')

StartDate = 20191231
EndDate = 202202022
sTitle = expression(paste("12/31/2019 through 03/03/2021"))
FileName = 'SPX.xlsx'
source('Futures Term Premiums.R')
# sTitle = expression(paste("12/31/2019 through 03/03/2021, Mini SPX"))
# FileName = 'MiniSPX.xlsx'
# source('Futures Term Premiums.R')



# StartDate = 19941231
# EndDate = 19991231
# sTitle = expression(paste("12/31/1994 through 12/31/1999"))
# source('Futures Term Premiums.R')
# StartDate = 19991231
# EndDate = 20041231
# sTitle = expression(paste("12/31/1999 through 12/31/2004"))
# source('Futures Term Premiums.R')
# StartDate = 20041231
# EndDate = 20091231
# sTitle = expression(paste("12/31/2004 through 12/31/2009"))
# source('Futures Term Premiums.R')
# StartDate = 20091231
# EndDate = 20141231
# sTitle = expression(paste("12/31/2009 through 12/31/2014"))
# source('Futures Term Premiums.R')
# StartDate = 20141231
# EndDate = 20191231
# sTitle = expression(paste("12/31/2014 through 12/31/2019"))
# source('Futures Term Premiums.R')
