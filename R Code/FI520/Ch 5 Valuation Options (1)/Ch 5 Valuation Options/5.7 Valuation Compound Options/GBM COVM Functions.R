# GBM COVM w Greeks Functions.R
#
# Present value function for GBMOVM-related calculations
PV1 = function(Maturity, Rate){
  return(exp( -(Rate/100.0) * Maturity) )
}
# n - probability density funcetion of standard normal (0,1)
n = function(d){
  return( exp( -(d^2) / 2.0 ) / ( sqrt( 2.0 * pi ) ) )
}
# # N - cumulative distribution function of standard normal (0,1)
# N = function(d){
#   return( as.numeric(integrate(n,-Inf,d)[1]) )
# }
# NAp - approximation of N
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
# d - functions used in BBMOVM
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
# GBMOVM
#
GBMOptionValue = function(B){
  with(B, {
    OptionValue = Type * StockPrice * PV1(TimeToMaturity, DividendYield - OptionYield) * 
      NAp(Type * d1(B)) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate - OptionYield) * 
      NAp(Type * d2(B))
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
# Implied stock price function: May need to pass upper bound as input
GBMDYOptionImpliedStockPrice <- function(B, inputOptionValue, 
  LowerBound, UpperBound){
  TestFunctionGBMDYOptionStockPrice <- function(testStockPrice, B, 
    inputOptionValue){
    B$StockPrice = testStockPrice
    return( abs(inputOptionValue - GBMOptionValue(B))^2 )  
  }
  solution = optimize(TestFunctionGBMDYOptionStockPrice, B, inputOptionValue, 
    interval = c(LowerBound, UpperBound), tol = .Machine$double.eps^0.25)
  ImpliedStockPrice = solution$minimum
  B$StockPrice = ImpliedStockPrice
  Difference = inputOptionValue - GBMOptionValue(B)
  if (abs(Difference) < 0.01 )return(ImpliedStockPrice)
  else return(NA)
}
# Critical stock price for compound option
COCriticalStockPrice <- function(C, UOV, L, U){
  with(C, {
    inputUnderlying <- S
    inputUnderlyingStrikePrice <- XU
    inputInterestRate <- r
    inputUnderlyingYield <- d
    inputOptionYield <- q
    inputVolatility <- v
    inputTimeToMaturity <- TU - TC
    inputType <- iU
    GBMInputData <- list(inputUnderlying, inputUnderlyingStrikePrice, 
      inputInterestRate, inputUnderlyingYield,  inputOptionYield,  
      inputVolatility, inputTimeToMaturity, inputType)
    names(GBMInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
      "DividendYield", "OptionYield", "Volatility", "TimeToMaturity", "Type")
    return(GBMDYOptionImpliedStockPrice(GBMInputData, UOV, L, U))
  })
}
# d21
COd21 <- function(C, L, U){
  with(C, {
    r <- r/100
    d <- d/100
    v <- v/100
    CSP <- COCriticalStockPrice(C, XC, L, U)
    B1Nrd <- exp((r - d)*TC)
    v1 <- v*sqrt(TC)
    d21 <- (log( (S*B1Nrd) /CSP) - ( (v1^2)/2 ))/v1
    return(d21)
  })
}
# d11
COd11 <- function(C, L, U){
  with(C, {
    r <- r/100
    d <- d/100
    v <- v/100
    CSP <- COCriticalStockPrice(C, XC, L, U)
    B1Nrd <- exp((r - d)*TC)
    v1 <- v*sqrt(TC)
    d11 <- (log( (S*B1Nrd) /CSP) + ((v1^2)/2) )/v1
    return(d11)
  })
}
# d22
COd22 <- function(C){
  with(C, {
    r <- r/100
    d <- d/100
    v <- v/100
    B2Nrd <- exp((r - d)*TU)
    v2 <- v*sqrt(TU)
    d22 <- (log( (S*B2Nrd) /XU) - ((v2^2)/2) )/v2
    return(d22)
  })
}
# d12
COd12 <- function(C){
  with(C, {
    r <- r/100
    d <- d/100
    v <- v/100
    B2Nrd <- exp((r - d)*TU)
    v2 <- v*sqrt(TU)
    d12 <- (log( (S*B2Nrd) /XU) + ((v2^2)/2) )/v2
    return(d12)
  })
}
# Compound option value
COValue <- function(C, L, U){
  with(C, {
    r <- r/100
    d <- d/100
    q <- q/100
    v <- v/100
    B2d <- exp(-d*TU)
    B12q <- exp(-q*(TU - TC))
    B12Nq <- exp(q*(TU - TC))
    B2r <- exp(-r*TU)
    B1r <- exp(-r*TC)
    B2q <- exp(-q*TU)
    B12d <- exp(-d*(TU - TC))
    d11 <- COd11(C, L, U)
    d21 <- COd21(C, L, U)
    d12 <- COd12(C)
    d22 <- COd22(C)
    mean1 <- rep(0,2)
    lower1 <- rep(-Inf,2)
    corr1 <- diag(2)
    corr1[lower.tri(corr1)] <- iC*sqrt(TC/TU)
    corr1[upper.tri(corr1)] <- iC*sqrt(TC/TU)
    upper1 <- c(iC*iU*d11,iU*d12)
    N2d11d12 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
    upper1 <- c(iC*iU*d21, iU*d22)
    N2d21d22 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
    CO <- iC*iU*S*B12Nq*B2d*N2d11d12
    CO <- CO - iC*iU*XU*B12Nq*B2r*N2d21d22 - iC*XC*B1r*NAp(iC*iU*d21)
    return(CO)
  })
}
# Compound option lower bound
COLowerBound <- function(C){
  with(C, {
    inputUnderlying <- S
    inputUnderlyingStrikePrice <- XU
    inputInterestRate <- r
    inputUnderlyingYield <- d
    inputOptionYield <- q
    inputVolatility <- v
    inputTimeToMaturity <- TU
    inputType <- iU
    GBMInputData <- list(inputUnderlying, inputUnderlyingStrikePrice, 
      inputInterestRate, inputUnderlyingYield, inputOptionYield, 
      inputVolatility, inputTimeToMaturity, inputType)
    names(GBMInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
      "DividendYield", "OptionYield", "Volatility", "TimeToMaturity", "Type")
    UOption <- GBMOptionValue(GBMInputData)
    r <- r/100
    q <- q/100
    B1r <- exp(-r*TC)
    B1q <- exp(-q*TC)
    if(iC == 1)LB <- pmax(0, B1q*UOption - XC*B1r)
    if(iC == -1)LB <- pmax(0,XC*B1r -  B1q*UOption)
    return(LB)
  })
}
# Compound option upper bound
COUpperBound <- function(C){
  with(C, {
    inputUnderlying <- S
    inputUnderlyingStrikePrice <- XU
    inputInterestRate <- r
    inputUnderlyingYield <- d
    inputOptionYield <- q
    inputVolatility <- v
    inputTimeToMaturity <- TU
    inputType <- iU
    GBMInputData <- list(inputUnderlying, inputUnderlyingStrikePrice, 
      inputInterestRate, inputUnderlyingYield, inputOptionYield, 
      inputVolatility, inputTimeToMaturity, inputType)
    names(GBMInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
      "DividendYield", "OptionYield", "Volatility", "TimeToMaturity", "Type")
    UOption <- GBMOptionValue(GBMInputData)
    r <- r/100
    q <- q/100
    B1r <- exp(-r*TC)
    B1q <- exp(-q*TC)
    if(iC == 1)UB <- B1q*UOption
    if(iC == -1)UB <- XC*B1r
    return(UB)
  })
}
