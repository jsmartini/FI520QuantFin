# Valuation GBM Binomial OVM American-Style Test.R
# Geometric Brownian Motion
# Illustrating American-style binomial option valuation and 
#  related functions in R
# rmarkdown::render("Valuation GBM Binomial OVM American-Style Test.R", 
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
source('ASGBMBINOVM Functions.R')
inputStockPrice = 100.0     # Need "input" as using variable names below
inputStrikePrice = 100.0    # In currency units, numeric
inputInterestRate = 5.0     # In percent
inputDividendYield = 5.0         # In percent
inputVolatility = 30.0           # In percent
inputTimeToMaturity = 1.0        # In fraction of year
inputType = 1L                   # 1 for call, -1 for put
inputNumberOfSteps = as.integer(500)   # Or use L: 1000L
inputPayoutType = 1L             # 1 Plain vanilla, 2 digital
inputEMMProbability = 50.0       # In percent
inputDigitalPayout = 100.0
LowerBoundSP = 50
UpperBoundSP = 150
NumberOfObservations = 101
# Plot footers
TX = paste0('X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
Td = paste0(',d=', inputDividendYield, ',')
TV = paste0('Vol=', inputVolatility)
TT = paste0(',T=',inputTimeToMaturity)
TN = paste0(',N=',inputNumberOfSteps)
TDP = paste0(',DP=', inputDigitalPayout)
sTitleBIN = paste0(TX, TR, Td, TV, TT, TN)
sTitleD = paste0(TX, TR, Td, TV, TT, TN, TDP)
#  BINInputData - list of inputs with associated names
BINInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate,
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType,
  inputNumberOfSteps, inputPayoutType, inputEMMProbability,
  inputDigitalPayout)
names(BINInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
  "DividendYield", "Volatility", "TimeToMaturity", "Type", "NumberOfSteps",
  "PayoutType", "EMMProbability", "DigitalPayout")
BINInputData
# Test functions
BINInputData$Type = 1L
BINInputData$PayoutType = 1L
ESCallValue = ESBINOptionValue(BINInputData)
ASCallValue = ASBINOptionValue(BINInputData)
BINInputData$Type = -1L
ESPutValue = ESBINOptionValue(BINInputData)
BINInputData$PayoutType = 2L
BINInputData$Type = 1L
ESDigitalCallValue = ESBINOptionValue(BINInputData)
BINInputData$PayoutType = 2L
BINInputData$Type = -1L
ESDigitalPutValue = ESBINOptionValue(BINInputData)
PV = exp(-(inputInterestRate/100.0)*inputTimeToMaturity)*inputDigitalPayout
DV = ESDigitalCallValue + ESDigitalPutValue
ESDiff = PV - DV
ESCallValue; ESPutValue; ESDigitalCallValue; ESDigitalPutValue; DV; ESDiff
BINInputData$Type = 1L
BINInputData$PayoutType = 1L
ASCallValue = ASBINOptionValue(BINInputData)
BINInputData$Type = -1L
ASPutValue = ASBINOptionValue(BINInputData)
BINInputData$PayoutType = 2L
BINInputData$Type = 1L
ASDigitalCallValue = ASBINOptionValue(BINInputData)
BINInputData$PayoutType = 2L
BINInputData$Type = -1L
ASDigitalPutValue = ASBINOptionValue(BINInputData)
PV = exp(-(inputInterestRate/100.0)*inputTimeToMaturity)*inputDigitalPayout
DV = ASDigitalCallValue + ASDigitalPutValue
ASDiff = PV - DV
ASCallValue; ASPutValue; ASDigitalCallValue; ASDigitalPutValue; DV; ASDiff
#
# Plots with Stock Price
#
StepSize = (UpperBoundSP - LowerBoundSP)/(NumberOfObservations - 1)
StockPrice <- c(1:NumberOfObservations)
ESPVCallLB <- c(1:NumberOfObservations)
ESPVCallUB <- c(1:NumberOfObservations)
ESPVPutLB <- c(1:NumberOfObservations)
ESPVPutUB <- c(1:NumberOfObservations)
ESPVCallValue <- c(1:NumberOfObservations)
ESPVPutValue <- c(1:NumberOfObservations)
ESDigitalCallValue <- c(1:NumberOfObservations)
ESDigitalPutValue <- c(1:NumberOfObservations)
ASPVCallLB <- c(1:NumberOfObservations)
ASPVCallUB <- c(1:NumberOfObservations)
ASPVPutLB <- c(1:NumberOfObservations)
ASPVPutUB <- c(1:NumberOfObservations)
ASPVCallValue <- c(1:NumberOfObservations)
ASPVPutValue <- c(1:NumberOfObservations)
ASDigitalCallValue <- c(1:NumberOfObservations)
ASDigitalPutValue <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  StockPrice[i] <-LowerBoundSP + (i - 1)*StepSize
  BINInputData$StockPrice = StockPrice[i]
# European-style
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  ESPVCallLB[i] = ESBINOptionLowerBound(BINInputData)
  ESPVCallUB[i] = ESBINOptionUpperBound(BINInputData)
  ESPVCallValue[i] = ESBINOptionValue(BINInputData)
  BINInputData$Type = -1L
  ESPVPutLB[i] = ESBINOptionLowerBound(BINInputData)
  ESPVPutUB[i] = ESBINOptionUpperBound(BINInputData)
  ESPVPutValue[i] = ESBINOptionValue(BINInputData)
  BINInputData$PayoutType = 2L
  BINInputData$Type = 1L
  ESDigitalCallValue[i] = ESBINOptionValue(BINInputData)
  BINInputData$Type = -1L
  ESDigitalPutValue[i] = ESBINOptionValue(BINInputData)
# American-style
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  ASPVCallLB[i] = ASBINOptionLowerBound(BINInputData)
  ASPVCallUB[i] = ASBINOptionUpperBound(BINInputData)
  ASPVCallValue[i] = ASBINOptionValue(BINInputData)
  BINInputData$Type = -1L
  ASPVPutLB[i] = ASBINOptionLowerBound(BINInputData)
  ASPVPutUB[i] = ASBINOptionUpperBound(BINInputData)
  ASPVPutValue[i] = ASBINOptionValue(BINInputData)
  BINInputData$PayoutType = 2L
  BINInputData$Type = 1L
  ASDigitalCallValue[i] = ASBINOptionValue(BINInputData)
  BINInputData$Type = -1L
  ASDigitalPutValue[i] = ASBINOptionValue(BINInputData)
}
# # Plots: ES and AS call and put values
# MaxValue = max(ESPVCallValue, ESPVPutValue, ASPVCallValue, ASPVPutValue)
# MinValue = min(ESPVCallValue, ESPVPutValue, ASPVCallValue, ASPVPutValue)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# MaxValue = max(StockPrice)
# MinValue = min(StockPrice)
# xlim1 = c(1:2)
# xlim1[1] = MinValue
# xlim1[2] = MaxValue
# legtxt = c("ESCall Value", "ASCall Value", "ESPut Value", "ASPut Value")
# mTitle = "Binomial Option Values (GBM)"
# xTitle = "Stock Price"
# yTitle = "Option Value"
# lTitle = "Parameter"
# plot(StockPrice, ESPVCallValue, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(StockPrice, ASPVCallValue, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(StockPrice, ESPVPutValue, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
# lines(StockPrice, ASPVPutValue, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black","black","black"), pch = c(1, 3, 2, 4), bty = "n", 
#   title = lTitle)
# Plots: ES and AS call values and LB
MaxValue = max(ESPVCallValue, ESPVCallLB, ASPVCallValue, ASPVCallLB)
MinValue = min(ESPVCallValue, ESPVCallLB, ASPVCallValue, ASPVCallLB)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
MaxValue = max(StockPrice)
MinValue = min(StockPrice)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
legtxt = c("ES Call Value", "ES Call LB", "AS Call Value", "AS Call LB")
mTitle = "Binomial Option Values (GBM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, ESPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ESPVCallLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ASPVCallValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ASPVCallLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
# Plots: ES and AS put values and LB
MaxValue = max(ESPVPutValue, ESPVPutLB, ASPVPutValue, ASPVPutLB)
MinValue = min(ESPVPutValue, ESPVPutLB, ASPVPutValue, ASPVPutLB)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
MaxValue = max(StockPrice)
MinValue = min(StockPrice)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
legtxt = c("ES Put Value", "ES Put LB", "AS Put Value", "AS Put LB")
mTitle = "Binomial Option Values (GBM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, ESPVPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ESPVPutLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ASPVPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ASPVPutLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
# # Plots: AS call values and LB
# MaxValue = max(ASPVCallValue, ASPVCallLB)
# MinValue = min(ASPVCallValue, ASPVCallLB)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# MaxValue = max(StockPrice)
# MinValue = min(StockPrice)
# xlim1 = c(1:2)
# xlim1[1] = MinValue
# xlim1[2] = MaxValue
# legtxt = c("AS Call Value", "AS Call LB")
# mTitle = "American-Style Binomial Call Option Values and LBs (GBM)"
# xTitle = "Stock Price"
# yTitle = "Option Value"
# lTitle = "Parameter"
# plot(StockPrice, ASPVCallValue, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(StockPrice, ASPVCallLB, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black"), pch = c(1, 2), bty = "n", 
#   title = lTitle)
#  Call time value
ESCallTV <- ESPVCallValue - ESPVCallLB
ASCallTV <- ASPVCallValue - ASPVCallLB
MaxValue = max(ESCallTV, ASCallTV)
MinValue = min(ESCallTV, ASCallTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "ES and AS Binomial Option Time Value (GBM)"
xTitle = "Stock Price"
yTitle = "Call Time Values"
legtxt = c("ES Call Time Value","AS Call Time Value")
lTitle = "Parameter"
plot(StockPrice, ESCallTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASCallTV, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("bottom", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)



#  Put time value
ESPutTV <- ESPVPutValue - ESPVPutLB
ASPutTV <- ASPVPutValue - ASPVPutLB
MaxValue = max(ESPutTV, ASPutTV)
MinValue = min(ESPutTV, ASPutTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "ES and AS Binomial Option Time Value (GBM)"
xTitle = "Stock Price"
yTitle = "Put Time Values"
legtxt = c("ES Put Time Value","AS Put Time Value")
lTitle = "Parameter"
plot(StockPrice, ESPutTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPutTV, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("bottom", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# Call and put digital option value
MaxValue = max(ESDigitalCallValue, ESDigitalPutValue)
MinValue = min(ESDigitalCallValue, ESDigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Digital Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Digital Option Value"
lTitle = "Parameter"
plot(StockPrice, ESDigitalCallValue, type = "b", main = mTitle,
  sub = sTitleD, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ESDigitalPutValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# American-style call and put values
MaxValue = max(ASPVCallValue, ASPVPutValue)
MinValue = min(ASPVCallValue, ASPVPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "American-Style Binomial Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, ASPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# # AS call and put digital option values
# MaxValue = max(ASDigitalCallValue, ASDigitalPutValue)
# MinValue = min(ASDigitalCallValue, ASDigitalPutValue)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# MaxValue = max(StockPrice)
# MinValue = min(StockPrice)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# legtxt = c("Call Value","Put Value")
# mTitle = "American-Style Binomial Digital Option Value (GBM)"
# xTitle = "Stock Price"
# yTitle = "Digital Option Value"
# lTitle = "Parameter"
# plot(StockPrice, ASDigitalCallValue, type = "b", main = mTitle,
#   sub = sTitleD, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(StockPrice, ASDigitalPutValue, type = "b", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("bottom", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# AS- and ES call and put values
MaxValue = max(ASPVCallValue, ASPVPutValue, ESPVCallValue, ESPVPutValue)
MinValue = min(ASPVCallValue, ASPVPutValue, ESPVCallValue, ESPVPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("AS Call Value","AS Put Value", "ES Call Value","ES Put Value")
mTitle = "ES and AS Binomial Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, ASPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVPutValue, type = "b", col ="black",
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPVCallValue, type = "b", col ="black",
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ESPVPutValue, type = "b", col ="black",
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1), lty = c(1, 1, 1, 1),
  col = c("black","black", "black", "black"), pch = c(1, 2, 3, 4),
  bty = "n", title = lTitle)
# AS- and ES- digital options
MaxValue = max(ASDigitalCallValue, ASDigitalPutValue,
  ESDigitalCallValue, ESDigitalPutValue)
MinValue = min(ASDigitalCallValue, ASDigitalPutValue,
  ESDigitalCallValue, ESDigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("AS Call Value","AS Put Value", "ES Call Value","ES Put Value")
mTitle = "ES and AS Binomial Digital Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Digital Option Value"
lTitle = "Parameter"
plot(StockPrice, ASDigitalCallValue, type = "b", main = mTitle,
  sub = sTitleD, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASDigitalPutValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESDigitalCallValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ESDigitalPutValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("left", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1), lty = c(1, 1, 1, 1),
  col = c("black","black", "black", "black"), pch = c(1, 2, 3, 4), 
  bty = "n", title = lTitle)

beep(sound = 10, print('Finished')) # Helpful when running long program
