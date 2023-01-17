# ESGBMBINOVM Functions.R
#  European-style, geometric Brownian motion, binomial OVM  
# Present value function
PV1 <- function(Maturity, Rate){
  return(exp( -(Rate/100.0) * Maturity) )
}
# European-style lower bound
ESBINOptionLowerBound <- function(B){
  with(B, {
    LowerBound <- Type * (StockPrice * PV1(TimeToMaturity, DividendYield) -
      StrikePrice * PV1(TimeToMaturity, InterestRate) )
    LowerBound <- max(0, LowerBound)
    return( LowerBound )
  })
}
# European-style upper bound
ESBINOptionUpperBound <- function(B){
  with(B, {
    if(Type == 1)UpperBound <- 
      StockPrice * PV1(TimeToMaturity, DividendYield)
    if(Type == -1)UpperBound <- 
      StrikePrice * PV1(TimeToMaturity, InterestRate)
    return( UpperBound )
  })
}
# Binomial probability
BinomialProbability <- function(N, J, Probability) {
  sum1 <- 0
  sum2 <- 0
  if(J == 0 || J == N){
    sum1 <- sum2 <- 0
  } else {
    m1 <- J + 1
    m2 <- N - J
    for(i in m1:N)sum1 <- log(i) + sum1
    for(i in 0:m2){
      if(i == 0) lni <- 0
      else lni <- log(i)
      sum2 <- lni + sum2
    }
  }
  return( exp(sum1 - sum2 + J*log(Probability) + 
    (N - J) * log(1.0 - Probability)) )
}
#
# European-style binomial option valuation function
#
ESBINOptionValue <- function(B){
  with(B, {
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    Rate <- (InterestRate - DividendYield)/100.0  # Local variable, decimal
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( exp(Rate*Delta) * A ) / (Prob*A + (1-Prob))
    Down <- exp(Rate*Delta) / (Prob*A + (1-Prob))
# Test that d<PeriodicRate<u otherwise quit
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    for (StateStep in 0:NumberOfSteps){
      dStateStep <- as.numeric(StateStep)
      dSteps <- as.numeric(NumberOfSteps)
      if( (Type == 1) && (PayoutType == 1) ){    # Plain vanilla call
        Moneyness <- (Up^dStateStep) * (Down^(dSteps-dStateStep)) * 
          StockPrice - StrikePrice
      }
      if( (Type == 1) && (PayoutType == 2) ){    # Digital call
        Moneyness <- (Up^dStateStep) * (Down^(dSteps-dStateStep)) * 
          StockPrice - StrikePrice
        if(Moneyness > 0.0) Moneyness <- DigitalPayout
      }
      if( (Type == -1) && (PayoutType == 1) ){   # Plain vanilla put
        Moneyness <- StrikePrice -
          ((Up^dStateStep)*(Down^(dSteps-dStateStep)) * StockPrice)
      }
      if( (Type == -1) && (PayoutType == 2) ){  # Digital put
        Moneyness  <- StrikePrice - 
          ((Up^dStateStep)*(Down^(dSteps-dStateStep)) * StockPrice)
        if(Moneyness > 0.0) Moneyness <- DigitalPayout
      }
      if (Moneyness < -999998.999 && Moneyness > -999999.001){
        Value <- -999999.0
      } else {
        Value <- Value + BinomialProbability(NumberOfSteps, StateStep, Prob) *
          max(0.0, Moneyness)
        Sum <- BinomialProbability(NumberOfSteps, StateStep, Prob) + Sum
      }
    }  
    Value <- Value * PV1(TimeToMaturity, InterestRate)
    # Check lower boundary conditions
    if(PayoutType == 1){    # Plain vanilla option
      LowerBound <- ESBINOptionLowerBound(B)
    } else {
      LowerBound <- 0
    }
    Value <- max(Value, LowerBound)
    return(Value)
  })
}
