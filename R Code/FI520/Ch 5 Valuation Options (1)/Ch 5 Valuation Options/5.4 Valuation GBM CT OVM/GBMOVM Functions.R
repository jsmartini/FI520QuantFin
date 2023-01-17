# GBMOVM Functions.R
#
#  INPUT STRUCTURE
#  GBMInputData - list of inputs with associated names; percent, not decimal
#  GBMInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
#    inputDividendYield, inputVolatility, inputTimeToMaturity, inputType)
#  names(GBMInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
#    "DividendYield", "Volatility", "TimeToMaturity", "Type")
#
# Available functions
#  PV1(Maturity, Rate) - present value of $1
#  B = GBMInputData
#  d1(B) - value of d1
#  d2(B) - value of d2
#  n(d) - standard normal PDF, given scalar d
#  N(d) - standard normal CDF, given scalar d
#  GBMOptionValue(B) - option value, type = 1 is call, type = -1 is put
#  OptionLowerBound(B) - option lower bounds
#  OptionUpperBound(B) - option upper bounds
#
# Functions for GBMOVM-related calculations
#
PV1 = function(Maturity, Rate){
  return(exp( -(Rate/100.0) * Maturity) )
}
# d - functions used in GBMOVM
# with: Evaluates R expressions based on a set of data (B here)
d1 = function(B){
  with(B, {
    Num = ( ((InterestRate - DividendYield) / 100) + ((Volatility/100)^2)/2) *
      TimeToMaturity
    Num = log(StockPrice/StrikePrice) + Num
    Den = (Volatility/100)*sqrt(TimeToMaturity)  
    return( Num/Den )
  })
}

d2 = function(B){
  with(B, {
    return( d1(B) - (Volatility/100)*sqrt(TimeToMaturity) )
  })
}
# Normal distribution functions
# n - probability density function of standard normal (0,1)
n = function(d){
  return( exp( -(d^2) / 2.0 ) / ( sqrt( 2.0 * pi ) ) )
}
# N - cumulative distribution function of standard normal (0,1)
N = function(d){
  return( as.numeric(integrate(n,-Inf,d)[1]) )
}
# GBMOVM
GBMOptionValue = function(B){
  with(B, {
    OptionValue = Type * StockPrice * PV1(TimeToMaturity, DividendYield) * 
      N(Type * d1(B)) - Type * StrikePrice * PV1(TimeToMaturity, InterestRate) * 
      N(Type * d2(B))
    LowerBound = Type * StockPrice * PV1(TimeToMaturity, DividendYield) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate)
    return( max(OptionValue, LowerBound) )
  })
}
# Lower bound
OptionLowerBound = function(B){
  with(B, {
    LowerBound = Type * StockPrice * PV1(TimeToMaturity, DividendYield) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate)
    LowerBound = max(0, LowerBound)
    return( LowerBound )
  })
}
# Upper bound
OptionUpperBound = function(B){
  with(B, {
    if(Type == 1)UpperBound = StockPrice * PV1(TimeToMaturity, DividendYield)
    if(Type == -1)UpperBound = StrikePrice * PV1(TimeToMaturity, InterestRate)
    return( UpperBound )
  })
}