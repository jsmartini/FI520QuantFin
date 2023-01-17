# ABMOVM Functions.R
#
#  INPUT STRUCTURE
#  ABMInputData - list of inputs with associated names; percent, not decimal
#  ABMInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
#    inputDividendYield, inputVolatility, inputTimeToMaturity, inputType)
#  names(ABMInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
#    "DividendYield", "Volatility", "TimeToMaturity", "Type")
#
# Available functions
#  PV1(Maturity, Rate) - present value of $1
#  B = ABMInputData
#  d1(B) - value of d1
#  d2(B) - value of d2
#  n(d) - standard normal PDF, given scalar d
#  N(d) - standard normal CDF, given scalar d
#  ABMOptionValue(B) - option value, type = 1 is call, type = -1 is put
#  OptionLowerBound(B) - option lower bounds
#  OptionUpperBound(B) - option upper bounds
#
# Functions for ABMOVM-related calculations
#
PV1 = function(Maturity, Rate){
  return(exp( -(Rate/100.0) * Maturity) )
}
# Adjusted volatility to use in dn and option values
AdjustedSigma = function(B){
  with(B, {
    if (abs(InterestRate - DividendYield)<0.00001){
      AdjSigma <- Volatility * sqrt(TimeToMaturity)
    } else {
      AdjSigma <- Volatility * 
        sqrt( (( PV1(TimeToMaturity,-2*(InterestRate-DividendYield)) )-1) /
                (2*(InterestRate-DividendYield)/100) )
    }
    return(AdjSigma)
  })
}
# dn - functions used in ABMOVM
# with: Evaluates R expressions based on a set of data (B here)
dn = function(B){
  with(B, {
    AdjSigma <- AdjustedSigma(B)
    Num = StockPrice*PV1(TimeToMaturity, -(InterestRate-DividendYield)) -
      StrikePrice
    Den = AdjSigma 
    return( Num/Den )
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
# ABMOVM
ABMOptionValue = function(B){
  with(B, {
    AdjSigma <- AdjustedSigma(B)
    OptionValue = Type * ( StockPrice * PV1(TimeToMaturity, DividendYield) - 
      StrikePrice * PV1(TimeToMaturity, InterestRate) ) * N(Type * dn(B)) +
      PV1(TimeToMaturity, InterestRate) * AdjSigma * n(dn(B))
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