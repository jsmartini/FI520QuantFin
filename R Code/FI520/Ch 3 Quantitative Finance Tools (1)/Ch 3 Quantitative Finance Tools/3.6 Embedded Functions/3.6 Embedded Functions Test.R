# 3.6 Embedded Functions Test.R
# function, optimize
# rmarkdown::render("3.6 Embedded Functions Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  stats: optimize, solving for yield to maturity
Packages <- c("stats") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Bond inputs
CouponRate = 5.0
ParValue = 100.0
YearsToMaturity = 30
YieldToMaturity = 6.0
ActualPrice = 80.0
NumberOfObservations = 21
YieldLowerBound = 0.0
YieldUpperBound = 10.0
YieldStepSize = (YieldUpperBound - YieldLowerBound)/(NumberOfObservations-1)
#
# Bond value function
#
FRMBondValue <- function(tempYieldToMaturity, tempCouponRate, tempParValue, 
  tempYearsToMaturity){
  PV = 0.0 # Present value variable
  for (i in 1:tempYearsToMaturity){
    PV = PV + ((tempCouponRate/100.0)*tempParValue) / 
      ((1.0 + (tempYieldToMaturity/100.0))^i)
  }
  return(PV + tempParValue / 
    ((1.0 + (tempYieldToMaturity/100.0))^tempYearsToMaturity))
}
# Test the function
BondValue = FRMBondValue(YieldToMaturity, CouponRate, ParValue, 
  YearsToMaturity)
BondValue
#
# Yield to maturity function with bond function (Version 1)
#
# Function that finds the difference between market and model bond prices
BondValue <- 90
FRMPriceDifferenceWFunction <- function(tempYieldToMaturity, tempCouponRate, 
  tempParValue, tempYearsToMaturity, tempActualPrice){
  tempBV <- FRMBondValue(tempYieldToMaturity, tempCouponRate, tempParValue, 
    tempYearsToMaturity)
  return(abs(tempActualPrice - tempBV))
}
TestDifference1 = FRMPriceDifferenceWFunction(YieldToMaturity, CouponRate, 
  ParValue, YearsToMaturity, BondValue)
TestDifference1
#
# Yield to maturity function with embedded bond value calculation (Version 2)
#
# Function that finds the difference between market and model bond prices
FRMPriceDifference <- function(tempYieldToMaturity, tempCouponRate, 
  tempParValue, tempYearsToMaturity, tempActualPrice){
  PV = 0.0 # Present value variable
  for (i in 1:tempYearsToMaturity){
    PV = PV + ((tempCouponRate/100.0)*tempParValue) / 
      ((1.0 + (tempYieldToMaturity/100.0))^i)
  }
  return(abs(tempActualPrice - (PV + tempParValue 
    / ((1.0 + (tempYieldToMaturity/100.0))^tempYearsToMaturity))))
}
# Test the function -- should be 0 if using BondValue from calculation above
BondValue = 90
TestDifference2 = FRMPriceDifference(YieldToMaturity, CouponRate, ParValue, 
  YearsToMaturity, BondValue)
TestDifference2
TestDifference2 - TestDifference1
# Minimize the objective function (FRMPriceDifference) by changing 
#  YieldToMaturity
# ?'optimize' # Help on optimize function
# Note using ActualPrice and not BondValue
# optimize will solve for the first parameter in the function 
# (tempYieldToMatuirity in FRMPriceDifference here)
solution = optimize(FRMPriceDifferenceWFunction, tempCouponRate = CouponRate, 
  tempParValue = ParValue, tempYearsToMaturity = YearsToMaturity, 
  tempActualPrice = ActualPrice, interval = c(0,1000), 
  tol = .Machine$double.eps^0.25)
solution
# Print YieldToMaturity that equates actual and model bond prices
BondYieldToMaturity = solution$minimum
BondYieldToMaturity
# Data for plots
YieldToMaturity = c(1:NumberOfObservations)
BondValue30 <- c(1:NumberOfObservations)
BondValue15 <- c(1:NumberOfObservations)
BondValue1 <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  YieldToMaturity[i] <- as.double(YieldLowerBound + (i-1)*YieldStepSize)
  YearsToMaturity = 1
  BondValue1[i] = FRMBondValue(YieldToMaturity[i], CouponRate, ParValue, 
    YearsToMaturity)
  YearsToMaturity = 15
  BondValue15[i] = FRMBondValue(YieldToMaturity[i], CouponRate, ParValue,
    YearsToMaturity)
  YearsToMaturity = 30
  BondValue30[i] = FRMBondValue(YieldToMaturity[i], CouponRate, ParValue, 
    YearsToMaturity)
}
# Simple plot
MaxXValue = max(YieldToMaturity)
MinXValue = min(YieldToMaturity)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MaxYValue = max(BondValue1, BondValue15, BondValue30)
MinYValue = min(BondValue1, BondValue15, BondValue30)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
legtxt = c("30 Year","15 Year","1 Year")
Title1 = "Bond Price-Yield Relation"
xTitle = "Yield To Maturity"
yTitle = "Bond Value"
# Plot footers
TC = paste0('Coupon = ', CouponRate, '%')
TPar = paste0(', Par = $', ParValue)
sTitle = paste0(TC, TPar)
lTitle = "Maturity"
plot(YieldToMaturity, BondValue30, type="b", main=Title1,
  sub=sTitle, xlab=xTitle, ylab=yTitle, col="black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5, lty = 1)
lines(YieldToMaturity,BondValue15, type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5, lty = 2)
lines(YieldToMaturity,BondValue1, type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 0.5, lty = 3)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(1,2,3), 
  col = c("black","black","black"), pch = c(1, 2, 3), bty = "n", 
  title = lTitle)

