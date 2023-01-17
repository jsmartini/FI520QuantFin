# 3.4 LSC Curve Fitting Test.R
# stats: lm()
# read.table(), head(), as.character(), matrix(), plot(), lines()
# Illustrating a simple OLS regression: a modified Nelson and Siegel model
# rmarkdown::render("2.5 LSC Curve Fitting Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
Packages <- c("stats") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Inputs
NumberOfFactors <- 3
N <- NumberOfFactors - 2
Scalars <- c(1:N)
Scalars[1] <- 2.0
NumberOfDates <- 5
# Read in data from file
# NumberOfMaturities <- 9
DataSource <- 'UST'
# LSCData <- read.table("LSCInputData.dat",header = FALSE)
NumberOfMaturities <- 8
LSCData <- read.table("USTInputData.dat",header = FALSE)
NumberOfDates <- length(LSCData$V1) - 1
head(LSCData,10) # Examine first few lines
Maturity <- c(1:NumberOfMaturities)
Maturity[1] <- LSCData$V2[1]
Maturity[2] <- LSCData$V3[1]
Maturity[3] <- LSCData$V4[1]
Maturity[4] <- LSCData$V5[1]
Maturity[5] <- LSCData$V6[1]
Maturity[6] <- LSCData$V7[1]
Maturity[7] <- LSCData$V8[1]
Maturity[8] <- LSCData$V9[1]
if(DataSource == 'IRS')Maturity[9] <- LSCData$V10[1]
Dates <- c(1:NumberOfDates)
for(i in 1:NumberOfDates){
  Dates[i] = as.character(LSCData$V1[i+1])
}
LSCData # Review data management thus far
Dates
Maturity
# Place input rates in matrix
Rates <- matrix(nrow = NumberOfDates, ncol = NumberOfMaturities)
for(i in 1:NumberOfDates){
  Rates[i,1] <- LSCData$V2[i+1]
  Rates[i,2] <- LSCData$V3[i+1]
  Rates[i,3] <- LSCData$V4[i+1]
  Rates[i,4] <- LSCData$V5[i+1]
  Rates[i,5] <- LSCData$V6[i+1]
  Rates[i,6] <- LSCData$V7[i+1]
  Rates[i,7] <- LSCData$V8[i+1]
  Rates[i,8] <- LSCData$V9[i+1]
  if(DataSource == 'IRS')Rates[i,9] <- LSCData$V10[i+1]
}
Rates
Factors <- matrix(nrow = NumberOfFactors - 1, ncol = NumberOfMaturities)
Factors # Note Factors is filled with NAs
for (j in 1:NumberOfFactors-1) {
  for (i in 1:NumberOfMaturities) {
    if (j == 1) Factors[j,i] = (1.0 - exp(-Maturity[i]/Scalars[j])) / 
        (Maturity[i]/Scalars[j])
    else Factors[j, i] = (1.0 - exp(-Maturity[i]/Scalars[j-1])) / 
      (Maturity[i]/Scalars[j-1]) - exp(-Maturity[i]/Scalars[j-1])
  }
}
Factors # Now Factors has x values for regression
Intercept <- c(1:NumberOfDates) # Vectors for output
Slope <- c(1:NumberOfDates)
Curvature <- c(1:NumberOfDates)
rsquared <- c(1:NumberOfDates)
# OLS regressions for each date
for (i in 1:NumberOfDates){ # Cross-section analysis of each date
  LSC <- lm(formula = Rates[i,]~Factors[1,]+Factors[2,])
  Betas <- LSC$coefficients # Make clear grabbing beta coefficients
  Intercept[i] <- Betas[1]
  Slope[i] <- Betas[2]
  Curvature[i] <- Betas[3]
  rsquared[i] <- summary(LSC)$r.square
}

# Work on fitted data for plots
FittedRates <- matrix(nrow = NumberOfDates, ncol = NumberOfMaturities)
for (i in 1:NumberOfDates) { # Fitted values for each date
  for (j in 1:NumberOfMaturities){
    FittedRates[i,j] = Intercept[i] + Slope[i] * 
      ((1.0 - exp(-Maturity[j]/Scalars[1]))/(Maturity[j]/Scalars[1])) + 
      Curvature[i]*((1.0 - exp(-Maturity[j]/Scalars[1])) / 
      (Maturity[j]/Scalars[1]) - exp(-Maturity[j]/Scalars[1]))
  }
}
Rates        # Check output
FittedRates
# Plots
x <- Maturity
y1 <- Rates        # Helps to check output
y2 <- FittedRates
MinXValue = 0; MaxXValue = max(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MinYValue = min(y1, y2,na.rm = TRUE); MaxYValue = max(y1, y2,na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
legtxt = c("Actual Rates","Fitted Rates")
if(DataSource == 'IRS')mTitle = "Swap Rates"
if(DataSource == 'UST')mTitle = "UST Rates"
xTitle = "Maturity"
yTitle = "Rates"
lTitle <- "Parameter"
legtxt = c("Actual","Fitted")
for (i in 1:NumberOfDates) {
  sTitle <- paste0(Dates[i], ", Scalar = ", Scalars[1], 
    ", NFactors = ", NumberOfFactors,  ", R^2 = ", round(rsquared[i],3))
  plot(x, y1[i,], type="b", main=mTitle, sub=sTitle, xlab=xTitle, 
    ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 1.0)
  lines(Maturity, FittedRates[i,], type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 2, cex = 1.0)
  legend("bottomright", legtxt, cex = 1.0, lwd = c(1, 1), lty = c(1, 1),
    col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
}
