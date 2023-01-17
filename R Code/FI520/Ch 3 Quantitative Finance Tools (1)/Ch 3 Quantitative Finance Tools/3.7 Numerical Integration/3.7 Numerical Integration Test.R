# 3.7 Numerical Integration Test.R
# function(), plot(), lines(), legend(), integrate()
# Normal and lognormal statistics
# rmarkdown::render("3.7 Numerical Integration Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  stats: integrate function
Packages <- c("stats") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Test Data
StockPrice = 100.0
StrikePrice = 100.0
InterestRate = 12.0
DividendYield = 0.0
Volatility = 30.0
TimeToMaturity = 1.0
source('Normal Distribution Functions.R')
source('Lognormal Distribution Functions.R')
#
# Function tests
#
NMean <- NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility)
NSD <- NormalStandardDeviation(Volatility, TimeToMaturity)
NSKewness <- NormalSkewness() # Known to be zero
NExcessKurtosis <- NormalExcessKurtosis() # Known to be zero
NEntropy <- NormalEntropy(Volatility, TimeToMaturity)
LNMean <- LognormalMean(StockPrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
LNSD <- LognormalStandardDeviation(StockPrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
LNSKewness <- LognormalSkewness(Volatility, TimeToMaturity)
LNExcessKurtosis <- LognormalExcessKurtosis(Volatility, TimeToMaturity)
LNEntropy <- LognormalEntropy(StockPrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
#
# Test normal PDF via integration
#
d = 0
LowerBound = -Inf
NMean <- NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility)
NSD <- NormalStandardDeviation(Volatility, TimeToMaturity)
UpperBound = NMean
Results = integrate(NormalPDF, LowerBound, UpperBound, NMean, NSD)
NV = Results$value
NV
#
# Test BSMOVM N(d2) via integration
#
NMean <- NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility)
NSD <- NormalStandardDeviation(Volatility, TimeToMaturity)
d2 = (NMean - log(StrikePrice)) / NSD
LowerBound = -Inf
UpperBound = d2
Results = integrate(NormalPDF, LowerBound, UpperBound, 0, 1)
Nd2V = Results$value
Nd2V
#
# Various calculations via integration functions
#
Nd1V = Nd1(StockPrice, StrikePrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
Nd2V = Nd2(StockPrice, StrikePrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
CallITMProb = Nd2(StockPrice, StrikePrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
PutITMProb <- 1.0 - Nd2(StockPrice, StrikePrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
TotalProb = CallITMProb + PutITMProb
CallITMProb; PutITMProb; TotalProb
#
# Density and distribution study
#
FixXAxis <- TRUE
FixYPDFAxis <- TRUE
FixYCDFAxis <- TRUE
OriginalVolatility <- Volatility
Increment <- 50
for(i in 1:5){
  Volatility <- OriginalVolatility + (i-1)*Increment
  source('Density and Distribution Study.R')
}
Volatility <- OriginalVolatility
#
# Plots with volatility
#
# Lognormal PDF Illustration Data
NumberOfObservations = 41
LowerBound = 5.0
UpperBound = 200
StepSize = (UpperBound - LowerBound)/(NumberOfObservations-1)
VolatilityV = c(1:NumberOfObservations)
LognormalMeanV <- c(1:NumberOfObservations)
LognormalMedianV <- c(1:NumberOfObservations)
LognormalModeV <- c(1:NumberOfObservations)
FI = 1L # KEY: 1 = Call, 2 = Put
for(i in 1:NumberOfObservations){
  VolatilityV[i] <- as.double(LowerBound + (i-1)*StepSize)
  LognormalMeanV[i] = LognormalMean(StockPrice, InterestRate, DividendYield, 
    TimeToMaturity, VolatilityV[i])
  LognormalMedianV[i] = LognormalMedian(StockPrice, InterestRate, 
    DividendYield, TimeToMaturity, VolatilityV[i])
  LognormalModeV[i] = LognormalMode(StockPrice, InterestRate, DividendYield, 
    TimeToMaturity, VolatilityV[i])
}
MaxYValue = max(LognormalMeanV, LognormalMedianV, LognormalModeV)
MinYValue = min(LognormalMeanV, LognormalMedianV, LognormalModeV)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(VolatilityV)
MinXValue = min(VolatilityV)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("Mean","Median","Mode")
mTitle = "Lognormal Mean, Median, and Mode"
xTitle = "Volatility"
yTitle = "Value"
lTitle = "Parameter"
# Plot footers
TS = paste0('S=', StockPrice)
TR = paste0(', r=', InterestRate)
TD = paste0(', d=', DividendYield)
# TSD = paste0(', Vol=', Volatility)
TTM = paste0(', Maturity=', TimeToMaturity)
sTitle = paste0(TS, TR, TD, TTM)
plot(VolatilityV, LognormalMeanV, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(VolatilityV, LognormalMedianV, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(VolatilityV, LognormalModeV, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(2,2,2), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# Plots with probability of being in-the-money
#
StepSize = (UpperBound - LowerBound)/(NumberOfObservations-1)
VolatilityV = c(1:NumberOfObservations)
CallITMProb <- c(1:NumberOfObservations)
PutITMProb <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  VolatilityV[i] <- LowerBound + (i-1)*StepSize
  CallITMProb[i] = Nd2(StockPrice, StrikePrice, InterestRate, DividendYield, 
    TimeToMaturity, VolatilityV[i])
  PutITMProb[i] <- 1.0 - Nd2(StockPrice, StrikePrice, InterestRate, 
    DividendYield, TimeToMaturity, VolatilityV[i])
}
MaxYValue = max(CallITMProb, PutITMProb)
MinYValue = min(CallITMProb, PutITMProb)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(VolatilityV)
MinXValue = min(VolatilityV)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("Call","Put")
mTitle = "Probability Option In-The-Money"
xTitle = "Volatility"
yTitle = "Probability"
TS = paste0('S=', StockPrice)
TR = paste0(', r=', InterestRate)
TD = paste0(', d=', DividendYield)
TX = paste0(', X=', StrikePrice)
TTM = paste0(', Maturity=', TimeToMaturity)
sTitle = paste0(TS, TX, TR, TD, TTM)
lTitle = "Parameter"
plot(VolatilityV, CallITMProb, type = "b", main = mTitle, sub = sTitle, 
  xlab = xTitle,ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1,
  pch = 1, cex = 0.5)
lines(VolatilityV, PutITMProb, type = "b", col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(2,2,2), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
#
# Built in random numbers: BSMOVM parameters 
#
ExpectedReturnD = (InterestRate/100.0) + 0.10  # Decimal form
VolatilityD = (Volatility/100.0) + 0.3 # Artificially increase parameters
# Sample PDF with normal or lognormal distribution random number generator
NumberOfObservations = 10001
StockPriceTN = c(1:NumberOfObservations) # Terminal distribution, normal
StockPriceTLN = c(1:NumberOfObservations) # Terminal distribution, lognormal
NMean = log(StockPrice) + (ExpectedReturnD - VolatilityD^2/2)*TimeToMaturity
NStdDev = VolatilityD*sqrt(TimeToMaturity)
NMean2 = round(NMean, digits = 2)
NStdDev2 = round(NStdDev, digits = 2)
StockPriceTN = exp(rnorm(StockPriceTN,mean=NMean,sd=NStdDev))
StockPriceTLN = rlnorm(StockPriceTLN,mean=NMean,sd=NStdDev)
# 
# MinValueSP <- max(StockPriceTLN,StockPriceTN)
# MinValueSP
# 
# legtxt = c("Normal","Lognormal")
xTitle = "Underlying Variable"
yTitle = "PDF"
mTitle = "Sample Lognormal PDF (Exponential of Normal Variable)"
lTitle = "Parameter"
hist(StockPriceTN, main=mTitle, breaks=50, freq=FALSE, col="black", 
  labels = FALSE, sub=paste("Mean = ",NMean2,", Standard Deviation = ", 
  NStdDev2, ", Sample Size = ", NumberOfObservations),
  xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10)  
curve(dlnorm(x, NMean, NStdDev), add=TRUE, col="black", lwd=4) 
mTitle = "Sample Lognormal PDF (Lognormal Sample)"
hist(StockPriceTLN, main=mTitle, breaks=50, freq=FALSE, col="black", 
  labels = FALSE, sub=paste("Mean = ",NMean2,", Standard Deviation = ", 
    NStdDev2, ", Sample Size = ", NumberOfObservations),
  xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10)  
curve(dlnorm(x, NMean, NStdDev), add=TRUE, col="black", lwd=4) 
#
# Lognormal PDFs with different volatilities
#
# Reset Number of Observations to improve figure

NumberOfObservations <- NumberOfObservations/10

LowVol <- 45.0
MediumVol <- 90.0
HighVol <- 135.0
LowerBound = 1
UpperBound = 1000.0
StepSize = (UpperBound - LowerBound)/(NumberOfObservations-1)
Vol1 = c(1:NumberOfObservations)
v = c(1:NumberOfObservations)
ExpectedReturnD = 0.1
VolatilityD = LowVol/100.0     
NMean = log(StockPrice) + (ExpectedReturnD - VolatilityD^2/2)*TimeToMaturity
NStdDev = VolatilityD*sqrt(TimeToMaturity)
for(i in 1:NumberOfObservations){
  v[i] <- LowerBound + (i-1)*StepSize
  Vol1[i] = dlnorm(v[i], NMean, NStdDev)  # Probability density function
}
VolatilityD = MediumVol/100.0
NMean = log(StockPrice) + (ExpectedReturnD - VolatilityD^2/2)*TimeToMaturity
NStdDev = VolatilityD*sqrt(TimeToMaturity)
Vol2 = c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  Vol2[i] = dlnorm(v[i], NMean, NStdDev)
}
VolatilityD = HighVol/100.0       
NMean = log(StockPrice) + (ExpectedReturnD - VolatilityD^2/2)*TimeToMaturity
NStdDev = VolatilityD*sqrt(TimeToMaturity)
Vol3 = c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  Vol3[i] = dlnorm(v[i], NMean, NStdDev)
}
MaxYValue = max(Vol1, Vol2, Vol3)
MinYValue = min(Vol1, Vol2, Vol3)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
txt1 = paste0("Volatility = ", LowVol, "%")
txt2 = paste0("Volatility = ", MediumVol, "%")
txt3 = paste0("Volatility = ", HighVol, "%")
legtxt = c(txt1,txt2, txt3)
mTitle = "Lognormal PDF"
xTitle = "Underlying Variable"
yTitle = "PDF"
sTitlePDF = expression(paste("S=100.0,", mu,"=10%,","T=1"))
lTitle = "Parameter"
plot(v, Vol1, type = "b", main = mTitle, sub = sTitlePDF, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(v, Vol2, type = "b", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(v, Vol3, type = "b", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(1,1,1), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
