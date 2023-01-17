# ASABMBINOVM Backward Recursion Function.R
#  
# American-style binomial option valuation function (ABM)
source('ESABMBINOVM Backward Recursion Function.R')
ABMASOptionValue = function(B){
  with(B,{
    CallValue <- numeric(NumberOfSteps+1)
    PutValue <- numeric(NumberOfSteps+1)
    DigitalCallValue <- numeric(NumberOfSteps+1)
    DigitalPutValue <- numeric(NumberOfSteps+1)
    Rate = InterestRate/100.0  # Local variable, decimal
    Dividend = DividendYield/100.0
    Sigma = Volatility              # Local variable, in DOLLARS
    Delta = TimeToMaturity / NumberOfSteps
    PeriodRate = exp(Rate*Delta)
    PeriodDiv = exp(Dividend*Delta)
    PeriodRateMDiv = exp((Rate - Dividend)*Delta)
    R <- PeriodRateMDiv - 1
    Prob = EMMProbability/100.0
    A = Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) 
    Up = (1 - Prob)*A
    Down = -Prob*A
    for (TimeStep in NumberOfSteps:0){
      for (StateStep in 0:TimeStep){
        if(TimeStep == NumberOfSteps){
          S <- StockPrice + StateStep*Up + (TimeStep - StateStep)*Down
          CallValue[StateStep+1] <- max(0, S - StrikePrice)
          PutValue[StateStep+1] <- max(0, StrikePrice - S)
          if(S > StrikePrice){
            DigitalCallValue[StateStep+1] <- DigitalPayout
            DigitalPutValue[StateStep+1] <- 0
          } else {
            DigitalCallValue[StateStep+1] <- 0
            DigitalPutValue[StateStep+1] <- DigitalPayout
          }
        } else {
          S <- StockPrice + StateStep*Up + (TimeStep - StateStep)*Down
          phi <- (S*R - Down)/(Up - Down)
          CallValue[StateStep+1] <- (1/PeriodRate) * 
            (phi*CallValue[StateStep+2] + (1 - phi)*CallValue[StateStep+1])
          PutValue[StateStep+1] <- (1/PeriodRate) * 
            (phi*PutValue[StateStep+2] + (1 - phi)*PutValue[StateStep+1])
          DigitalCallValue[StateStep+1] <- (1/PeriodRate) * 
            (phi*DigitalCallValue[StateStep+2] + 
            (1 - phi)*DigitalCallValue[StateStep+1])
          DigitalPutValue[StateStep+1] <- (1/PeriodRate) * 
            (phi*DigitalPutValue[StateStep+2] + 
            (1 - phi)*DigitalPutValue[StateStep+1])
# Check for early exercise
          IVCall <- max(0, S - StrikePrice)
          IVPut <- max(0, StrikePrice - S)
          IVDCall <- 0
          IVDPut <- 0
          if(S > StrikePrice){
            IVDCall <- DigitalPayout
            IVDPut <- 0
          } 
          if (S < StrikePrice){
            IVDCall <- 0
            IVDPut <- DigitalPayout
          }
          CallValue[StateStep+1] <- max(CallValue[StateStep+1], IVCall)
          PutValue[StateStep+1] <- max(PutValue[StateStep+1], IVPut)
          DigitalCallValue[StateStep+1] <- 
            max(DigitalCallValue[StateStep+1], IVDCall)
          DigitalPutValue[StateStep+1] <- 
            max(DigitalPutValue[StateStep+1], IVDPut)
        } 
      }
    }
    CV <- CallValue[1]
    PV <- PutValue[1]
    DCV <- DigitalCallValue[1]
    DPV <- DigitalPutValue[1]
    ABMOptionOutput <- list(CV, PV, DCV, DPV)
    names(ABMOptionOutput) <- c("CallValue", "PutValue", "DigitalCallValue", 
      "DigitalPutValue")
    return(ABMOptionOutput) 
  })
}
# Lower bound
ASOptionLowerBound = function(B){
  with(B, {
    LowerBound = Type * StockPrice * PV1(TimeToMaturity, DividendYield) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate)
    IntrinsicValue = max(0, Type * (StockPrice - StrikePrice))
    LowerBound = max(0, LowerBound, IntrinsicValue)
    return( LowerBound )
  })
}
# Upper bound
ASOptionUpperBound = function(B){
  with(B, {
    if(Type == 1)UpperBound = StockPrice
    if(Type == -1)UpperBound = StrikePrice
    return( UpperBound )
  })
}