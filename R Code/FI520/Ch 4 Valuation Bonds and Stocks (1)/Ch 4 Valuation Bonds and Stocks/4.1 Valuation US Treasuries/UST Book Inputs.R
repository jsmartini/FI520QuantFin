# UST Book Inputs.R
#  Contains both individual UST and CMT curve information
# Read UST bonds from spreadsheet
UST <- read.xlsx(xlsxFile = USTFileName, sheet = 1, skipEmptyRows = FALSE)
# is.data.frame(UST)
UST$BID <- NULL # Remove bid price
UST$CHG <- NULL # Remove price change from previous day
# Create Julian date and then convert to integer so it displays correctly
# in the Files window
UST$JMaturityDate = as.integer(as.date(UST$MATURITY - 21916)) 
UST$MATURITY <- NULL # Remove spreadsheet maturity
UST$MaturityDate <- date.mmddyyyy(UST$JMaturityDate) # Create R date
# Work on creating quoted bond price in decimal form
APrice0 <- UST$ASKED
APrice <- trunc(UST$ASKED) # Truncate price
AFrac1 <- round((APrice0 - APrice)*100) # Number of 32nds
AFrac2 <- APrice0*1000 - APrice*1000 - AFrac1*10 # Number of 8ths
UST$APrice <- APrice + (AFrac1 + AFrac2/8)/32 # Price in decimal form, % of par


