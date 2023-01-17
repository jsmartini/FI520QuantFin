# Density and Distribution Study.R
#  Sources in 3.8 Numerical Integration Test.R
# PDFs and CDFs
#
NumberOfObservations = 1001
SP = c(1:NumberOfObservations)
NormalPDFV = c(1:NumberOfObservations)
NormalCDFV <- c(1:NumberOfObservations)
LognormalPDFV = c(1:NumberOfObservations)
LognormalCDFV <- c(1:NumberOfObservations)
NormalM = StockPrice*exp((InterestRate/100)*TimeToMaturity)
NormalSD = Volatility*TimeToMaturity
LNMean <- log(StockPrice) + (InterestRate/100 - DividendYield/100 -
  ((Volatility/100)^2)/2)*TimeToMaturity
LNSD <- (Volatility/100)*sqrt(TimeToMaturity)
LowerBound = NormalM - 10*NormalSD
UpperBound = NormalM + 10*NormalSD
StepSize = (UpperBound - LowerBound)/(NumberOfObservations-1)
OriginalSP <- StockPrice
for(i in 1:NumberOfObservations){
  SP[i] <- as.double(LowerBound + (i-1)*StepSize)
  StockPrice <- SP[i]
  NormalPDFV[i] <- NormalPDF(StockPrice, NormalM, NormalSD)
  NormalCDFV[i] <- NormalCDF(StockPrice, NormalM, NormalSD)
  if(StockPrice > 0){
    LognormalPDFV[i] <- LognormalPDF(StockPrice, LNMean, LNSD)
    LognormalCDFV[i] <- LognormalCDF(StockPrice, LNMean, LNSD)
  } else {
    LognormalPDFV[i] <- NA
    LognormalCDFV[i] <- NA
  }
}
StockPrice <- OriginalSP
if(FixXAxis == TRUE){
  MaxXValue <- 500
  MinXValue <- -500
  xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
}
# Normal PDF
x <- SP
y1 <- NormalPDFV
if(FixYPDFAxis == TRUE){
  MaxYPDFValue <- 0.015
  MinYPDFValue <- 0
  ylim1 = c(1:2); ylim1[1] = MinYPDFValue; ylim1[2] = MaxYPDFValue
}
if(FixYPDFAxis == FALSE){
  MaxYPDFValue = max(y1)
  MinYPDFValue = min(y1)
  ylim1 = c(1:2); ylim1[1] = MinYPDFValue; ylim1[2] = MaxYPDFValue
}
if(FixXAxis == FALSE){
  MaxXValue = max(x)
  MinXValue = min(x)
  xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
}
legtxt = c("Normal PDF")
mTitle = "Normal PDF"
xTitle = "Stock Price"
yTitle = "PDF"
lTitle = "Parameter"
NormalM = StockPrice*exp((InterestRate/100)*TimeToMaturity)
NormalSD = Volatility*TimeToMaturity
NMedian <- NormalM
NMode <- NormalM
NSkewness <- NormalSkewness()
NExcessKurtosis <- NormalExcessKurtosis()
# Plot footers
TNM = paste0('E(S)=', round(NormalM,2))
TNSD = paste0(',SD(S)=', round(NormalSD,4))
TNMed = paste0(',Median=', round(NMedian,2))
TNMod = paste0(',Mode=', round(NMode,2))
TNSk = paste0(',Sk=', round(NSkewness,2))
TNEK = paste0(',EKurt=', round(NExcessKurtosis,2))
sTitle = paste0(TNM, TNSD, TNMed, TNMod, TNSk, TNEK)
plot(x, y1, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
# Normal CDF
x <- SP
y1 <- NormalCDFV
if(FixYCDFAxis == TRUE){
  MaxYCDFValue <- 1.0
  MinYCDFValue <- 0
  ylim1 = c(1:2); ylim1[1] = MinYCDFValue; ylim1[2] = MaxYCDFValue
}
if(FixYCDFAxis == FALSE){
  MaxYCDFValue = max(y1)
  MinYCDFValue = min(y1)
  ylim1 = c(1:2); ylim1[1] = MinYCDFValue; ylim1[2] = MaxYCDFValue
}
if(FixXAxis == FALSE){
  MaxXValue = max(x)
  MinXValue = min(x)
  xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
}
legtxt = c("Normal CDF")
mTitle = "Normal CDF"
xTitle = "Stock Price"
yTitle = "CDF"
lTitle = "Parameter"
plot(x, y1, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Lognormal PDF
x <- SP
y1 <- LognormalPDFV
if(FixYPDFAxis == TRUE){
  MaxYPDFValue <- 0.015
  MinYPDFValue <- 0
  ylim1 = c(1:2); ylim1[1] = MinYPDFValue; ylim1[2] = MaxYPDFValue
}
if(FixYPDFAxis == FALSE){
  MaxYPDFValue = max(y1,na.rm=TRUE)
  MinYPDFValue = min(y1,na.rm=TRUE)
  ylim1 = c(1:2); ylim1[1] = MinYPDFValue; ylim1[2] = MaxYPDFValue
}
if(FixXAxis == FALSE){
  MaxXValue = max(x)
  MinXValue = min(x)
  xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
}
legtxt = c("Lognormal PDF")
mTitle = "Lognormal PDF"
xTitle = "Stock Price"
yTitle = "PDF"
lTitle = "Parameter"
LNMean <- LognormalMean(StockPrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
LNSD <- LognormalStandardDeviation(StockPrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
LNMedian <- LognormalMedian(StockPrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
LNMode <- LognormalMode(StockPrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility)
LNSKewness <- LognormalSkewness(Volatility, TimeToMaturity)
LNExcessKurtosis <- LognormalExcessKurtosis(Volatility, TimeToMaturity)
# Plot footers
TLNM = paste0('E(S)=', round(LNMean,2))
TLNSD = paste0(',SD(S)=', round(LNSD,4))
TLNMed = paste0(',Median=', round(LNMedian,2))
TLNMod = paste0(',Mode=', round(LNMode,2))
TLNSk = paste0(',Sk=', round(LNSKewness,2))
TLNEK = paste0(',EKurt=', round(LNExcessKurtosis,2))
sTitle = paste0(TLNM, TLNSD, TLNMed, TLNMod, TLNSk, TLNEK)
plot(x, y1, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
# Lognormal CDF
x <- SP
y1 <- LognormalCDFV
if(FixYCDFAxis == TRUE){
  MaxYCDFValue <- 1.0
  MinYCDFValue <- 0
  ylim1 = c(1:2); ylim1[1] = MinYCDFValue; ylim1[2] = MaxYCDFValue
}
if(FixYCDFAxis == FALSE){
  MaxYCDFValue = max(y1,na.rm=TRUE)
  MinYCDFValue = min(y1,na.rm=TRUE)
  ylim1 = c(1:2); ylim1[1] = MinYCDFValue; ylim1[2] = MaxYCDFValue
}
if(FixXAxis == FALSE){
  MaxXValue = max(x)
  MinXValue = min(x)
  xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
}
legtxt = c("Lognormal CDF")
mTitle = "Lognormal CDF"
xTitle = "Stock Price"
yTitle = "CDF"
plot(x, y1, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)


