# Normal Distribution Functions.R
# Implied normal mean (e.g., within BSM framework)
NormalMean <- function(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  return(log(StockPrice) + ( ((InterestRate - DividendYield)/100.0) 
    - (((Volatility/100.0)^2.0)/2.0) )*TimeToMaturity)
}
# Implied normal standard deviation
NormalStandardDeviation <- function(Volatility, TimeToMaturity){
  return((Volatility/100.0)*(TimeToMaturity^0.5))
}
# Normal skewness
NormalSkewness <- function(){
  return(0)
}
# Normal excess kurtosis
NormalExcessKurtosis <- function(){
  return(0)
}
# Implied normal median
NormalMedian <- function(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  return(NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
    Volatility))
}
# Implied normal mode
NormalMode <- function(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  return(NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
    Volatility))
}
# Implied normal entropy
NormalEntropy <- function(Volatility, TimeToMaturity){
  return(0.5*(log(2.0*pi*NormalStandardDeviation(Volatility, TimeToMaturity)^2)+1))
}
#
# Normal PDF, CDF
#
# n - probability density function of standard normal (0,1)
n = function(d){
  return( exp( -(d^2) / 2.0 ) / ( sqrt( 2.0 * pi ) ) )
}
# PDF of normal (NMean, NSD)
NormalPDF <- function(x, NMean, NSD){
  PDF <- (1.0 / (((2.0*pi)^0.5)*NSD)) * 
    exp(-((x - NMean)^2.0) / (2.0*(NSD^2.0)))
  return(PDF)
}
# N - cumulative distribution function of standard normal (0,1)
N = function(d){
  return( as.numeric(integrate(n, -Inf, d, 0, 1)[1]) )
}
# CDF of normal (NMean, NSD)
NormalCDF <- function(x, NMean, NSD){ # Normal cumulative distribution function
  LowerBound = -Inf
  UpperBound = x
  Results = integrate(NormalPDF, LowerBound, UpperBound, NMean, NSD)
  NCDF <- Results$value
  if(x > NMean + 7*NSD){ # Integration unstable above 7 SDs in some cases
    NCDF <- 1.0
  }
  return(NCDF)
}
# Approximation of CDF of standard normal: Useful to avoid double integral
NAp = function(D){
  LengthD = length(D)
  CN = numeric(LengthD)
  for(i in 1:LengthD){
    if(is.na(D[i]))D[i] = -10
    CN[i] = -99
    if(D[i] > 7) CN[i] = 1.0
    if(D[i] < -7) CN[i] = 0.0
  }
  for(j in 1:LengthD){
    if(CN[j] < 0){
      CN[j] = 0.0
      for(i in 0:12){
        CN[j]=CN[j] + exp(-( (i+0.5)^2 )/9) * sin(abs(D[j])*(i+0.5) * 
          sqrt(2)/3)*(i+0.5)^(-1)
      }
      CN[j] = 0.5 + (1/pi)*CN[j]
      if(D[j] < 0) CN[j] = 1.0 - CN[j]
    }
  }
  return(CN)
}
# Selected applications of integration
Nd1 <- function(StockPrice, StrikePrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  NMean <- NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
    Volatility)
  NSD <- NormalStandardDeviation(Volatility, TimeToMaturity)
  d1 = (NMean - log(StrikePrice) + NSD^2.0) / NSD
  LowerBound = -Inf
  UpperBound = d1
  Results = integrate(NormalPDF, LowerBound, UpperBound, 0, 1)
  return (Results$value)
}

Nd2 <- function(StockPrice, StrikePrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  NMean <- NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
    Volatility)
  NSD <- NormalStandardDeviation(Volatility, TimeToMaturity)
  d2 = (NMean - log(StrikePrice)) / NSD
  LowerBound = -Inf
  UpperBound = d2
  Results = integrate(NormalPDF, LowerBound, UpperBound, 0, 1)
  return (Results$value)
}
