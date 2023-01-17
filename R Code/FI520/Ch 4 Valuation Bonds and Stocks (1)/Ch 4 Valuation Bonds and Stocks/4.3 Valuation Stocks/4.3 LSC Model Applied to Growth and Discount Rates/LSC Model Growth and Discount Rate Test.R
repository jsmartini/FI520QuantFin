# LSC Model Growth and Discount Rate Test.R
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  optimx - general purpose optimization
#  openxlsx - manipulate spreadsheet files
#  stats - general statistical functions
#  expss - vlookup (not used now)
#  dplyr - rounding in dataframes
Packages <- c("date", "optimx", "openxlsx", "stats", "expss", "dplyr") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Identify spreadsheet containing inputs
InputFileName <- 'LSC Input File.xlsx'
#
# Core modeling inputs
#
NumberOfMaturities <- 500
# SPY: SP 500 ETF
# XLK: SP 500 Sector ETF: Technology
# XLF: SP 500 Sector ETF: Financial
# XLI: SP 500 Sector ETF: Industrial
# XLY: SP 500 Sector ETF: Consumer Discretionary
# XLB: SP 500 Sector ETF: Materials
# XLV: SP 500 Sector ETF: Healthcare
# XLU: SP 500 Sector ETF: Utilities
# XLP: SP 500 Sector ETF: Consumer Staples
# XLE: SP 500 Sector ETF: Energy
#
# Calibrate using LSC model
#
source("LSC Model Calibration.R")
source("LSC Model Calibration Plots.R")
source("LSC Model Calibration Plots V2.R")

