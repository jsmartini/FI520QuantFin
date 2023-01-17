# ASGBMBINOVM Functions.R
#  American-style, geometric Brownian motion, binomial OVM  
source('ESGBMBINOVM Functions.R')
# American-style lower bound
ASBINOptionLowerBound <- function(B){
  with(B, {
    LowerBound <- Type * (StockPrice * PV1(TimeToMaturity, DividendYield) -
      StrikePrice * PV1(TimeToMaturity, InterestRate) )
    IntrinsicValue = max(0, Type*(StockPrice - StrikePrice))
    Value = max(0, LowerBound, IntrinsicValue)
    return( Value )
  })
}
# American-style upper bound (Same as ES)
ASBINOptionUpperBound <- function(B){
  with(B, {
    if(Type == 1){
      UpperBound <- StockPrice 
    }
    if(Type == -1){
      UpperBound <- StrikePrice 
    }
    return( UpperBound )
  })
}
# Option valuation -- Binomial American-style
ASBINOptionValue = function(B){
  with(B, {
    Sum = 0
    Moneyness = 0
    Value = 0
    DriftRate = (InterestRate - DividendYield)/100.0
    Rate = InterestRate/100.0  # Local variable, in decimal
    Sigma = Volatility/100.0   # Local variable, in decimal
    Delta = TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate = exp(Rate*Delta)             # Periodic rate (1+r)
    PeriodDriftRate = exp(DriftRate*Delta)   
    # N = NumberOfSteps
    OptionValue <- numeric(NumberOfSteps + 1)
# Lattice structure: Up, Down, and Prob
    Prob = EMMProbability/100.0
    A = exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up = ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down = PeriodDriftRate / (Prob*A + (1-Prob))
# Test that d<PeriodicRate<u otherwise quit
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
# AS: Backward induction
    for (TimeStep in NumberOfSteps:0){
# Inner loop
      for (i in 0:TimeStep){
        dStateStep <- as.numeric(i)
        dTimeStep <- as.numeric(TimeStep)
        if(TimeStep == NumberOfSteps){  # At expiration
          if( (Type == 1) && (PayoutType == 1) ){    # Plain vanilla call
            Moneyness <- (Up^dStateStep) * (Down^(dTimeStep-dStateStep)) * 
              StockPrice - StrikePrice
          }
          if( (Type == 1) && (PayoutType == 2) ){    # Digital call
            Moneyness <- (Up^dStateStep) * (Down^(dTimeStep-dStateStep)) * 
              StockPrice - StrikePrice
            if(Moneyness > 0.0) Moneyness <- DigitalPayout
          }
          if( (Type == -1) && (PayoutType == 1) ){   # Plain vanilla put
            Moneyness <- StrikePrice -
              (Up^dStateStep) * (Down^(dTimeStep-dStateStep)) * StockPrice
          }
          if( (Type == -1) && (PayoutType == 2) ){  # Digital put
            Moneyness  <- StrikePrice - 
              (Up^dStateStep) * (Down^(dTimeStep-dStateStep)) * StockPrice 
            if(Moneyness > 0.0) Moneyness <- DigitalPayout
          }
          OptionValue[i+1] <- max(0, Moneyness)
        } else { # AS prior to expiration
          OptionValue[i+1] = (1.0/PeriodRate) * (Prob*OptionValue[i+2] +
            (1.0 - Prob)*OptionValue[i+1])
          if((Type == 1) && (PayoutType == 1)){    # Plain vanilla call
            MarketStockPrice = (Up^dStateStep) * (Down^(dTimeStep-dStateStep)) *
              StockPrice
            Moneyness = max(0, MarketStockPrice - StrikePrice)
            OptionValue[i+1] = max(Moneyness, OptionValue[i+1])
          }
          if( (Type == 1) && (PayoutType == 2) ){    # Digital call
            MarketStockPrice = (Up^dStateStep) * (Down^(dTimeStep-dStateStep)) *
              StockPrice
            Moneyness = MarketStockPrice - StrikePrice
            if(Moneyness > 0.0) Moneyness <- DigitalPayout
            OptionValue[i+1] = max(Moneyness, OptionValue[i+1])
          }
          if((Type == -1) && (PayoutType == 1)){    # Plain vanilla put
            MarketStockPrice = (Up^dStateStep) * (Down^(dTimeStep-dStateStep)) *
              StockPrice
            Moneyness = max(0, StrikePrice - MarketStockPrice)
            OptionValue[i+1] = max(Moneyness, OptionValue[i+1])
          }
          if( (Type == -1) && (PayoutType == 2) ){    # Digital put
            MarketStockPrice = (Up^dStateStep) * (Down^(dTimeStep-dStateStep)) *
              StockPrice
            Moneyness = StrikePrice - MarketStockPrice
            if(Moneyness > 0.0) Moneyness <- DigitalPayout
            OptionValue[i+1] = max(Moneyness, OptionValue[i+1])
          }
        }
      }
      Value = OptionValue[1]  
    }
# Check lower boundary conditions
    if(PayoutType == 1){
      LowerBound = Type * ( StockPrice * PV1(TimeToMaturity, DividendYield) -
        StrikePrice * PV1(TimeToMaturity, InterestRate) )
      LowerBound = max(0, LowerBound)
      IntrinsicValue = max(0, Type*(StockPrice - StrikePrice))
      Value = max(Value, LowerBound, IntrinsicValue)
    }
    return(Value)
  })
}
