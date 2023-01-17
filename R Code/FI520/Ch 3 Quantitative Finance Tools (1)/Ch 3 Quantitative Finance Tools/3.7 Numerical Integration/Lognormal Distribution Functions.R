# Lognormal Distribution Functions.R
# BSMOVM implied lognormal mean
LognormalMean <- function(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  return( exp(NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
    Volatility) + (NormalStandardDeviation(Volatility, TimeToMaturity)^2.0)/2.0) )
}
# BSMOVM implied lognormal standard deviation
LognormalStandardDeviation <- function(StockPrice, InterestRate, DividendYield, 
  TimeToMaturity, Volatility){
  return( ((exp(NormalStandardDeviation(Volatility, TimeToMaturity)^2.0) - 1.0)
    * exp(2.0*NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
      Volatility) + NormalStandardDeviation(Volatility, TimeToMaturity)^2.0))^0.5 )
}
# BSMOVM implied lognormal skewness
LognormalSkewness <- function(Volatility, TimeToMaturity){
  return( (exp(NormalStandardDeviation(Volatility, TimeToMaturity)^2.0) + 2.0)
    * (exp(NormalStandardDeviation(Volatility, TimeToMaturity)^2.0) - 1.0)^0.5 )
}
# BSMOVM implied lognormal excess kurtosis
LognormalExcessKurtosis <- function(Volatility, TimeToMaturity){
  return( exp(4.0*NormalStandardDeviation(Volatility, TimeToMaturity)^2.0)
    + 2.0*exp(3.0*NormalStandardDeviation(Volatility, TimeToMaturity)^2.0)
    + 3.0*exp(2.0*NormalStandardDeviation(Volatility, TimeToMaturity)^2.0)
    - 6.0 )
}
# BSMOVM implied lognormal median
LognormalMedian <- function(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  return(exp(NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
    Volatility)))
}
# BSMOVM implied lognormal mode
LognormalMode <- function(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  return( exp(NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
    Volatility) - NormalStandardDeviation(Volatility, TimeToMaturity)^2) )
}
# BSMOVM implied lognormal entropy
LognormalEntropy <- function(StockPrice, InterestRate, DividendYield, TimeToMaturity, 
  Volatility){
  return (0.5 + 0.5*(log(2.0*pi*NormalStandardDeviation(Volatility, TimeToMaturity)^2)
    + NormalMean(StockPrice, InterestRate, DividendYield, TimeToMaturity, Volatility)) )
}
# Lognormal PDF
LognormalPDF <- function(x, LNMean, LNSD){ # Normal probability density function
  PDF <- (1.0 / (x*((2.0*pi)^0.5)*LNSD)) * exp(-((log(x) - LNMean)^2.0) / (2.0*(LNSD^2.0)))
  return(PDF)
}
# Lognormal CDF
LognormalCDF <- function(x, LNMean, LNSD){ # Normal cumulative distribution function
  LowerBound = 0.0000001
  UpperBound = x
  Results = integrate(LognormalPDF, LowerBound, UpperBound, LNMean, LNSD)
  return(Results$value)
}
