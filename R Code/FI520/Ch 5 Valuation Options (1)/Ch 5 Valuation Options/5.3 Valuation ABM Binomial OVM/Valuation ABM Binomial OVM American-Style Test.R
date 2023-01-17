# Valuation ABM Binomial OVM American-Style Test.R
# Arithmetic Brownian Motion
# Illustrating American-style binomial option valuation and related functions
# rmarkdown::render("Valuation ABM Binomial OVM American-Style Test.R", 
#  "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
PackagesToLibrary <- c("beepr") # Libraries
if (length(setdiff(PackagesToLibrary, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(PackagesToLibrary, rownames(installed.packages())))  
} # Make sure libraries are installed on this computer
lapply(PackagesToLibrary,library,character.only = TRUE) # Load and attach libraries
rm(PackagesToLibrary)
source('ASABMBINOVM Backward Recursion Function.R')
# Test inputs
inputStockPrice = 100.0          # Need "input" as using variable names below
inputStrikePrice = 100.0         # In currency units, numeric
inputInterestRate = 5.0          # In percent
inputDividendYield = 5.0         # In percent
inputVolatility = 29.88476829    # 29.88476829   # In dollars, annualized
inputTimeToMaturity = 1.0        # In fraction of year
inputType = 1L                   # 1 for call, -1 for put
inputNumberOfSteps = as.integer(500)   # Or use L: 1000L
inputPayoutType = 1L             # 1 Plain vanilla, 2 digital
inputEMMProbability = 50.0       # In percent
inputDigitalPayout = 100.0
LowerBoundSP = 50 # Analysis wrt stock price
UpperBoundSP = 150
NumberOfObservations = 101
# Plot footers
TX = paste0('X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
Td = paste0(',d=', inputDividendYield, ',')
TV = paste0('Vol=', round(inputVolatility,4))
TT = paste0(',T=',inputTimeToMaturity)
TN = paste0(',N=',inputNumberOfSteps)
TDP = paste0(',DP=', inputDigitalPayout)
sTitleBIN = paste0(TX, TR, Td, TV, TT, TN)
sTitleBINDP = paste0(TX, TR, Td, TV, TT, TN, TDP)
#  BINInputData - list of inputs with associated names
BINInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType, 
  inputNumberOfSteps, inputPayoutType, inputEMMProbability, 
  inputDigitalPayout)
names(BINInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
  "DividendYield", "Volatility", "TimeToMaturity", "Type", "NumberOfSteps", 
  "PayoutType", "EMMProbability", "DigitalPayout")
# Values
BINInputData$Type <- 1
ESPVCallLB = ESOptionLowerBound(BINInputData)
ESPVCallUB = ESOptionUpperBound(BINInputData)
ASPVCallLB = ASOptionLowerBound(BINInputData)
ASPVCallUB = ASOptionUpperBound(BINInputData)
BINInputData$Type <- -1
ESPVPutLB = ESOptionLowerBound(BINInputData)
ESPVPutUB = ESOptionUpperBound(BINInputData)
ASPVPutLB = ASOptionLowerBound(BINInputData)
ASPVPutUB = ASOptionUpperBound(BINInputData)
ESPVCallLB; ESPVCallUB
ASPVCallLB; ASPVCallUB
ESPVPutLB; ESPVPutUB
ASPVPutLB; ASPVPutUB
# American-style
Values = ABMASOptionValue(BINInputData)
ASPVCallValue <- Values$CallValue
ASPVPutValue <- Values$PutValue
ASDigitalCallValue <- Values$DigitalCallValue
ASDigitalPutValue <- Values$DigitalPutValue
ASPVCallValue; ASPVPutValue 
ASDigitalCallValue; ASDigitalPutValue
# European-style
Values = ABMESOptionValue(BINInputData)
ESPVCallValue <- Values$CallValue
ESPVPutValue <- Values$PutValue
ESDigitalCallValue <- Values$DigitalCallValue
ESDigitalPutValue <- Values$DigitalPutValue
ESPVCallValue; ESPVPutValue 
ESDigitalCallValue; ESDigitalPutValue

PVCallDiff <- ASPVCallValue - ESPVCallValue
PVPutDiff <- ASPVPutValue - ESPVPutValue
DigCallDiff <- ASDigitalCallValue - ESDigitalCallValue 
DigPutDiff <- ASDigitalPutValue - ESDigitalPutValue 
PVCallDiff; PVPutDiff; DigCallDiff; DigPutDiff
#
# StockPrice Plots
#
StepSizeSP = (UpperBoundSP - LowerBoundSP)/(NumberOfObservations - 1)
StockPrice <- numeric(NumberOfObservations)
ESPVCallLB <- numeric(NumberOfObservations)
ESPVCallUB <- numeric(NumberOfObservations)
ESPVPutLB <- numeric(NumberOfObservations)
ESPVPutUB <- numeric(NumberOfObservations)
ESPVCallValue <- numeric(NumberOfObservations)
ESPVPutValue <- numeric(NumberOfObservations)
ESDigitalCallValue <- numeric(NumberOfObservations)
ESDigitalPutValue <- numeric(NumberOfObservations)
ASPVCallLB <- numeric(NumberOfObservations)
ASPVCallUB <- numeric(NumberOfObservations)
ASPVPutLB <- numeric(NumberOfObservations)
ASPVPutUB <- numeric(NumberOfObservations)
ASPVCallValue <- numeric(NumberOfObservations)
ASPVPutValue <- numeric(NumberOfObservations)
ASDigitalCallValue <- numeric(NumberOfObservations)
ASDigitalPutValue <- numeric(NumberOfObservations)
for(i in 1:NumberOfObservations){
  StockPrice[i] <-LowerBoundSP + (i - 1)*StepSizeSP
  BINInputData$StockPrice = StockPrice[i]
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  ESPVCallLB[i] = ESOptionLowerBound(BINInputData)
  ESPVCallUB[i] = ESOptionUpperBound(BINInputData)
  ASPVCallLB[i] = ASOptionLowerBound(BINInputData)
  ASPVCallUB[i] = ASOptionUpperBound(BINInputData)
  BINInputData$Type = -1L
  ESPVPutLB[i] = ESOptionLowerBound(BINInputData)
  ESPVPutUB[i] = ESOptionUpperBound(BINInputData)
  ASPVPutLB[i] = ASOptionLowerBound(BINInputData)
  ASPVPutUB[i] = ASOptionUpperBound(BINInputData)
  Value <- ABMESOptionValue(BINInputData)
  ESPVCallValue[i] = Value$CallValue
  ESPVPutValue[i] = Value$PutValue
  ESDigitalCallValue[i] = Value$DigitalCallValue
  ESDigitalPutValue[i] = Value$DigitalPutValue
  Value <- ABMASOptionValue(BINInputData)
  ASPVCallValue[i] = Value$CallValue
  ASPVPutValue[i] = Value$PutValue
  ASDigitalCallValue[i] = Value$DigitalCallValue
  ASDigitalPutValue[i] = Value$DigitalPutValue
}
BINInputData$StockPrice = inputStockPrice # Reset from previous analysis
# Plots: Plain Vanilla Options with respect to the Stock Price
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
#  Call with boundaries
MaxYValue = max(ESPVCallValue, ASPVCallValue, ESPVCallLB, ESPVCallUB,
  ASPVCallLB, ASPVCallUB)
MinYValue = min(ESPVCallValue, ASPVCallValue, ESPVCallLB, ESPVCallUB,
  ASPVCallLB, ASPVCallUB)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Value"
legtxt = c("ES Call Value", "AS Call Value", 
  "ES Call Lower Bound", "ES Call Upper Bound",
  "AS Call Lower Bound", "AS Call Upper Bound")
lTitle = "Parameter"
plot(StockPrice, ESPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVCallValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPVCallLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ESPVCallUB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
lines(StockPrice, ASPVCallLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 5, cex = 0.5)
lines(StockPrice, ASPVCallUB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 6, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1,1,1), 
  lty = c(1,1,1,1,1,1),
  col = c("black", "black", "black", "black", "black", "black"), 
  pch = c(1,2,3,4,5,6), bty = "n", title = lTitle)
#  Call time value
ESPVCallTV <- ESPVCallValue - ESPVCallLB
ASPVCallTV <- ASPVCallValue - ASPVCallLB
MaxValue = max(ESPVCallTV, ASPVCallTV)
MinValue = min(ESPVCallTV, ASPVCallTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "ES and AS Binomial Option Time Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Time Value"
legtxt = c("ES Call Time Value", "AS Call Time Value")
lTitle = "Parameter"
plot(StockPrice, ESPVCallTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVCallTV, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1),
  col = c("black", "black"), pch = c(1,2), bty = "n", title = lTitle)
# Put with boundaries
MaxYValue = max(ESPVPutValue, ASPVPutValue, 
  ESPVPutLB, ESPVPutUB,
  ASPVPutLB, ASPVPutUB)
MinYValue = min(PVPutValue, ASPVPutValue,  
  PVPutLB, PVPutUB)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
legtxt = c("ES Put Value", "AS Put Value", 
  "ES Put Lower Bound", "ES Put Upper Bound",
  "AS Put Lower Bound", "AS Put Upper Bound")
mTitle = "Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(StockPrice, ESPVPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPVPutLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ESPVPutUB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
lines(StockPrice, ASPVPutLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 5, cex = 0.5)
lines(StockPrice, ASPVPutUB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 6, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1,1,1,1,1,1), 
  lty = c(1,1,1,1,1,1),
  col = c("black", "black", "black", "black", "black", "black"), 
  pch = c(1,2,3,4,5,6), bty = "n", title = lTitle)
#  Put time value
ESPVPutTV <- ESPVPutValue - ESPVPutLB
ASPVPutTV <- ASPVPutValue - ASPVPutLB
MaxValue = max(ESPVPutTV, ASPVPutTV)
MinValue = min(ESPVPutTV, ASPVPutTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "ES and AS Binomial Option Time Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Time Value"
legtxt = c("ES Put Time Value", "AS Put Time Value")
lTitle = "Parameter"
plot(StockPrice, ESPVPutTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVPutTV, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1),
  col = c("black", "black"), pch = c(1,2), bty = "n", title = lTitle)
# Calls and puts
MaxYValue = max(ESPVCallValue, ASPVCallValue,
  ESPVPutValue, ASPVPutValue)
MinYValue = min(ESPVCallValue, ASPVCallValue,
  ESPVPutValue, ASPVPutValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("ES Call Value", "AS Call Value", "ES Put Value", "AS Put Value")
mTitle = "Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, ESPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVCallValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPVPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ASPVPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1),
  col = c("black","black","black","black"), pch = c(1,2,3,4), bty = "n", 
  title = lTitle)
# Digital Options with respect to the Stock Price
MaxYValue = max(ESDigitalCallValue, ASDigitalCallValue,
  ESDigitalPutValue, ASDigitalPutValue)
MinYValue = min(ESDigitalCallValue, ASDigitalCallValue,
  ESDigitalPutValue, ASDigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("ES Call Value", "AS Call Value", "ES Put Value", "AS Put Value")
mTitle = "Binomial Digital Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Digital Option Value"
lTitle = "Parameter"
plot(StockPrice, ESDigitalCallValue, type = "b", main = mTitle,
  sub = sTitleBINDP, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASDigitalCallValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESDigitalPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ASDigitalPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("bottom", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1),
  col = c("black","black","black","black"), pch = c(1,2,3,4), bty = "n", 
  title = lTitle)
# #
# # Plots with Number Of Steps
# #
# StepCount = as.integer((as.numeric(MaxStep)-as.numeric(MinStep)) /
#   as.numeric(StepSize) + 1)
# NumberOfSteps = as.integer(seq(MinStep,MaxStep,StepSize))
# ESPVCallValue <- numeric(StepCount)
# ESPVPutValue <- numeric(StepCount)
# ESDigitalCallValue <- numeric(StepCount)
# ESDigitalPutValue <- numeric(StepCount)
# ASPVCallValue <- numeric(StepCount)
# ASPVPutValue <- numeric(StepCount)
# ASDigitalCallValue <- numeric(StepCount)
# ASDigitalPutValue <- numeric(StepCount)
# j=0L
# for(i in seq(MinStep, MaxStep, StepSize)){
#   j = j + 1
#   BINInputData$NumberOfSteps = as.integer(i)
#   Value <- ABMESOptionValue(BINInputData)
#   ESPVCallValue[j] = Value$CallValue
#   ESPVPutValue[j] = Value$PutValue
#   ESDigitalCallValue[j] = Value$DigitalCallValue
#   ESDigitalPutValue[j] = Value$DigitalPutValue
#   Value <- ABMASOptionValue(BINInputData)
#   ASPVCallValue[j] = Value$CallValue
#   ASPVPutValue[j] = Value$PutValue
#   ASDigitalCallValue[j] = Value$DigitalCallValue
#   ASDigitalPutValue[j] = Value$DigitalPutValue
# }
# BINInputData$NumberOfSteps = inputNumberOfSteps
# # Plain vanilla option values with respect to the number of steps
# xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
# MaxOptionValue = max(ESPVCallValue, ASPVCallValue,
#   ESPVPutValue, ASPVPutValue)
# MinOptionValue = min(ESPVCallValue, ASPVCallValue,
#   ESPVPutValue, ASPVPutValue)
# ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
# legtxt = c("ES Call Value", "AS Call Value", "ES Put Value", "AS Put Value")
# mTitle = "Binomial Option Value (ABM)"
# xTitle = "Number of Steps"
# yTitle = "Option Value"
# lTitle = "Parameter"
# plot(NumberOfSteps, ESPVCallValue, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black",
#   xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
# lines(NumberOfSteps, ASPVCallValue, type = "b", col ="black",
#   xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
# lines(NumberOfSteps, ESPVPutValue, type = "b", col ="black",
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(NumberOfSteps, ASPVPutValue, type = "b", col ="black",
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1),
#   col = c("black","black","black","black"), pch = c(1,2,3,4), bty = "n",
#   title = lTitle)
# # Digital Options with respect to the Stock Price
# MaxYValue = max(ESDigitalCallValue, ASDigitalCallValue,
#   ESDigitalPutValue, ASDigitalPutValue)
# MinYValue = min(ESDigitalCallValue, ASDigitalCallValue,
#   ESDigitalPutValue, ASDigitalPutValue)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
# legtxt = c("ES Call Value", "AS Call Value", "ES Put Value", "AS Put Value")
# mTitle = "Binomial Digital Option Value (ABM)"
# xTitle = "Number of Steps"
# yTitle = "Digital Option Value"
# lTitle = "Parameter"
# plot(NumberOfSteps, ESDigitalCallValue, type = "b", main = mTitle,
#   sub = sTitleBINDP, xlab = xTitle, ylab = yTitle, col = "black",
#   xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
# lines(NumberOfSteps, ASDigitalCallValue, type = "b", col ="black",
#   xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
# lines(NumberOfSteps, ESDigitalPutValue, type = "b", col ="black",
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(NumberOfSteps, ASDigitalPutValue, type = "b", col ="black",
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("bottom", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1),
#   col = c("black","black","black","black"), pch = c(1,2,3,4), bty = "n",
#   title = lTitle)

beep(sound = 10, print('Finished')) # Helpful when running long program
