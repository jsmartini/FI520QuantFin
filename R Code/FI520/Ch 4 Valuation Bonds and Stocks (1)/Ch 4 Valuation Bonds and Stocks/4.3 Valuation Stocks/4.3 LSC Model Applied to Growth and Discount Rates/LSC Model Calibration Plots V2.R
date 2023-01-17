# LSC Model Calibration Plots.R
#
# Need to plot G and DR over maturities
#
Maturity <- c(1:NumberOfMaturities)
N <- LengthII
FDR <- matrix(nrow = N, ncol = NumberOfMaturities)
GrowthRates <- matrix(nrow = N, ncol = NumberOfMaturities)
for (i in 1:N) { # Fitted values for each curve
  for (j in 1:NumberOfMaturities){
    GrowthRates[i,j] <- II$GLevel[i] + 
      II$GSlope[i] * ((1.0 - exp(-Maturity[j]/II$ScalarG[i]))/(Maturity[j]/II$ScalarG[i]))
    FDR[i,j] <- II$DRLevel[i] + 
      II$DRSlope[i] * ((1.0 - exp(-Maturity[j]/II$ScalarDR[i]))/(Maturity[j]/II$ScalarDR[i]))
  }
}

defaultpar <- par() # plot global parameters
par(defaultpar) # Reset to default parameters
LineType <- "b" # "p" points; "l" lines; "b" both
LineSize <- 0.75
# Set x variable here:
isxDate <- FALSE
xT <- Maturity
FixAxisX <- TRUE
MinValueX <- 0
MaxValueX <- 30
if(isxDate){
  xIT <- as.integer(xT) # Dates are tricky: Convert to integer only (Julian)
} else {
  xIT <- xT
}
# Number of y variables and definitions
Ny <- LengthII
FixAxisY <- TRUE
MinValueY <- 0.0
MaxValueY <- 25.0
xTitle = "Maturity"
lTitle = "Sectors"
legtxt <- format(II$Ticker)
sTitle <- "Sectors:"
cpch <- seq(1,N,1)
clwd <- c(rep(1, N))
clty <- c(rep(2,N))
mTitle <- "LSC Model: Growth Rates by Sector"
yTitle = "Growth Rates (%)"
yT <- GrowthRates
source("Generic Plots.R")
mTitle <- "LSC Model: Forward Discount Rate by Sector"
yTitle = "Forward Discount Rates (%)"
yT <- FDR
source("Generic Plots.R")
