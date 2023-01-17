# GBMOVM and Extended Functions.R
#
#  INPUT STRUCTURE
#  GBMInputData - list of inputs with associated names; percent, not decimal
#  GBMInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
#    inputDividendYield, inputVolatility, inputTimeToMaturity, inputType)
#  names(GBMInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
#    "DividendYield", "Volatility", "TimeToMaturity", "Type")
# Available core functions
#  PV1(Maturity, Rate) - present value of $1
#  B = GBMInputData
#  d1(B) - value of d1
#  d2(B) - value of d2
#  n(d) - standard normal PDF, given scalar d
#  N(d) - standard normal CDF, given scalar d
# GBM value functions
#  GBMOptionValue(B) - option value, type = 1 is call, type = 2 is put
#  OptionLowerBound(B) - option lower bounds
#  OptionUpperBound(B) - option lower bounds
# GBM implied functions
#  GBMDYOptionImpliedVolatility(B, inputOptionValue)
#  GBMDYOptionImpliedStockPrice(B, inputOptionValue)
#  GBMDYOptionImpliedStrikePrice(B, inputOptionValue)
#  GBMDYOptionImpliedTimeToMaturity(B, inputOptionValue)
#  GBMDYOptionImpliedInterestRate(B, inputOptionValue)
#  GBMDYOptionImpliedDividendYield(B, inputOptionValue)
#
# Core fuctions
#
# Present value function for GBMOVM-related calculations
#
PV1 = function(Maturity, Rate){
  return(exp( -(Rate/100.0) * Maturity) )
}
#
# d - functions used in BBMOVM
#
d1 = function(B){
  with(B, {
    Num = ( ((InterestRate - DividendYield) / 100) + ((Volatility/100)^2)/2)*
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
#
# Normal distribution functions
# n - probability density function of standard normal (0,1)
#
n = function(d){
  return( exp( -(d^2) / 2.0 ) / ( sqrt( 2.0 * pi ) ) )
}
#
# N - cumulative distribution function of standard normal (0,1)
#
N = function(d){
  return( as.numeric(integrate(n,-Inf,d)[1]) )
}
#
# GBMOVM
#
GBMOptionValue = function(B){
  with(B, {
    OptionValue = Type * StockPrice * PV1(TimeToMaturity, DividendYield) * 
      N(Type * d1(B)) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate) * 
      N(Type * d2(B))
    LowerBound = Type * StockPrice * PV1(TimeToMaturity, DividendYield) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate)
    return( max(OptionValue, LowerBound) )
  })
}
#
# Lower bound
#
OptionLowerBound = function(B){
  with(B, {
    LowerBound = Type * StockPrice * PV1(TimeToMaturity, DividendYield) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate)
    LowerBound = max(0, LowerBound)
    return( LowerBound )
  })
}
#
# Implied volatility function
#
GBMDYOptionImpliedVolatility <- function(B, inputOptionValue){
  TestFunctionGBMDYOptionImpliedVolatility<-function(testImpliedVolatility,B, 
    inputOptionValue){
      B$Volatility = testImpliedVolatility
      return( abs(inputOptionValue - GBMOptionValue(B))^2 )  
  }
  solution = optimize(TestFunctionGBMDYOptionImpliedVolatility, B, 
    inputOptionValue, interval = c(B$ImpliedLowerBound, B$ImpliedUpperBound), 
    tol = .Machine$double.eps^0.25)
  ImpliedVolatility = solution$minimum
  B$Volatility = ImpliedVolatility
  Difference = inputOptionValue - GBMOptionValue(B)
  if (abs(Difference) < 0.01)return(ImpliedVolatility)
  else return(NA)
}
#
# Implied stock price function
#
GBMDYOptionImpliedStockPrice <- function(B, inputOptionValue){
  TestFunctionGBMDYOptionStockPrice <- function(testStockPrice, B, 
    inputOptionValue){
    B$StockPrice = testStockPrice
    return( abs(inputOptionValue - GBMOptionValue(B))^2 )  
  }
  solution = optimize(TestFunctionGBMDYOptionStockPrice, B, inputOptionValue, 
    interval = c(B$ImpliedLowerBound, B$ImpliedUpperBound), 
    tol = .Machine$double.eps^0.25)
  ImpliedStockPrice = solution$minimum
  B$StockPrice = ImpliedStockPrice
  Difference = inputOptionValue - GBMOptionValue(B)
  if (abs(Difference) < 0.01 )return(ImpliedStockPrice)
  else return(NA)
}
#
# Implied strike price function
#
GBMDYOptionImpliedStrikePrice <- function(B, inputOptionValue){
  TestFunctionGBMDYOptionStrikePrice <- function(testStrikePrice, B, 
    inputOptionValue){
    B$StrikePrice = testStrikePrice
    return( abs(inputOptionValue - GBMOptionValue(B))^2 )  
  }
  solution = optimize(TestFunctionGBMDYOptionStrikePrice, B, inputOptionValue, 
    interval = c(B$ImpliedLowerBound, B$ImpliedUpperBound), 
    tol = .Machine$double.eps^0.25)
  ImpliedStrikePrice = solution$minimum
  B$StrikePrice = ImpliedStrikePrice
  Difference = inputOptionValue - GBMOptionValue(B)
  if (abs(Difference) < 0.01 )return(ImpliedStrikePrice)
  else return(NA)
}
#
# Implied Time to Maturity function: May need to pass upper bound as input
#
GBMDYOptionImpliedTimeToMaturity <- function(B, inputOptionValue){
  TestFunctionGBMDYOptionTimeToMaturity <- function(testTimeToMaturity, B, 
    inputOptionValue){
    B$TimeToMaturity = testTimeToMaturity
    return( abs(inputOptionValue - GBMOptionValue(B))^2 )  
  }
  solution=optimize(TestFunctionGBMDYOptionTimeToMaturity,B,inputOptionValue, 
    interval = c(B$ImpliedLowerBound, B$ImpliedUpperBound), 
    tol = .Machine$double.eps^0.25)
  ImpliedTimeToMaturity = solution$minimum
  B$TimeToMaturity = ImpliedTimeToMaturity
  Difference = inputOptionValue - GBMOptionValue(B)
  if (abs(Difference) < 0.01 )return(ImpliedTimeToMaturity)
  else return(NA)
}
#
# Implied Interest Rate function: May need to pass upper bound as input
#
GBMDYOptionImpliedInterestRate <- function(B, inputOptionValue){
  TestFunctionGBMDYOptionInterestRate <- function(testInterestRate, B, 
    inputOptionValue){
    B$InterestRate = testInterestRate
    return( abs(inputOptionValue - GBMOptionValue(B))^2 )  
  }
  solution = optimize(TestFunctionGBMDYOptionInterestRate,B,inputOptionValue, 
    interval = c(B$ImpliedLowerBound, B$ImpliedUpperBound), 
    tol = .Machine$double.eps^0.25)
  ImpliedInterestRate = solution$minimum
  B$InterestRate = ImpliedInterestRate
  Difference = inputOptionValue - GBMOptionValue(B)
  if (abs(Difference) < 0.01 )return(ImpliedInterestRate)
  else return(NA)
}
#
# Implied Dividend Yield function: May need to pass upper bound as input
#
GBMDYOptionImpliedDividendYield <- function(B, inputOptionValue){
  TestFunctionGBMDYOptionDividendYield <- function(testDividendYield, B, 
    inputOptionValue){
    B$DividendYield = testDividendYield
    return( abs(inputOptionValue - GBMOptionValue(B))^2 )  
  }
  solution = optimize(TestFunctionGBMDYOptionDividendYield,B,inputOptionValue, 
    interval = c(B$ImpliedLowerBound, B$ImpliedUpperBound), 
    tol = .Machine$double.eps^0.25)
  ImpliedDividendYield = solution$minimum
  B$DividendYield = ImpliedDividendYield
  Difference = inputOptionValue - GBMOptionValue(B)
  if (abs(Difference) < 0.01 )return(ImpliedDividendYield)
  else return(NA)
}
