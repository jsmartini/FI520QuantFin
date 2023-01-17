# Bond Functions.R
#
# Available functions:
#
#
#  CouponsRemaining(B)
#
# Frequency: 1, 2, 4, and 12 (anything not 1, 2, or 4)
CouponsRemaining = function(B){
  with(B,{
    SM = as.numeric(SettlementDateMonth)
    SD = as.numeric(SettlementDateDay)
    SY = as.numeric(SettlementDateYear)
    MM = as.numeric(MaturityDateMonth)
    MD = as.numeric(MaturityDateDay)
    MY = as.numeric(MaturityDateYear)
    # Decrement settlement date until it is a valid date (e.g., 2/31/YYYY)
    IsValid = mdy.date(SM, SD, SY, nineteen = FALSE)
    tSD = SD
    while(is.na(IsValid)){
      tSD = tSD - 1
      IsValid = mdy.date(SM, tSD, SY, nineteen = FALSE)
    }
    JSettlementDate = IsValid
    # Decrement maturity date until it is a valid date (e.g., 2/31/YYYY)
    IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
    tMD = MD
    while(is.na(IsValid)){
      tMD = tMD - 1
      IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
    }
    JMaturityDate = IsValid
    # Solve for number of remaining cash flows depending on payment frequency
    Counter = 0
    if(Frequency == 1){   # Annual
      while (JMaturityDate > JSettlementDate){
        MY = MY - 1
        IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
        tMD = MD
        while(is.na(IsValid)){
          tMD = tMD - 1
          IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
        }
        JMaturityDate = IsValid
        Counter = Counter + 1
      }
    } else if (Frequency == 2) { # Semi-annual
      while (JMaturityDate > JSettlementDate){
        if(MM > 6){
          MM = MM - 6
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        } else {
          MM = MM + 6
          MY = MY - 1
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        }
        Counter = Counter + 1
      }
    } else if (Frequency == 4) { # Quarterly
      while (JMaturityDate > JSettlementDate){
        if(MM > 3){
          MM = MM - 3
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        } else {
          MM = MM + 9
          MY = MY - 1
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        }
        Counter = Counter + 1
      }
    } else { # Monthly
      while (JMaturityDate > JSettlementDate){
        if(MM > 1){
          MM = MM - 1
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        } else {
          MM = MM + 11
          MY = MY - 1
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        }
        Counter = Counter + 1
      }
    }
    return(Counter)  
  })
}
#
# Elapsed: Fraction of coupon period since last coupon payment date
#   Used for accrued interest calculations
#   Also returns previous, current, and next julian dates
Elapsed = function(B){
  with(B, {
    SM = as.numeric(SettlementDateMonth)
    SD = as.numeric(SettlementDateDay)
    SY = as.numeric(SettlementDateYear)
    MM = as.numeric(MaturityDateMonth)
    MD = as.numeric(MaturityDateDay)
    MY = as.numeric(MaturityDateYear)
    # Decrement settlement date until it is a valid date (e.g., 2/31/YYYY)
    IsValid = mdy.date(SM, SD, SY, nineteen = FALSE)
    tMD = MD
    while(is.na(IsValid)){
      tMD = tMD - 1
      IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
    }
    JSettlementDate = IsValid
    # Decrement maturity date until it is a valid date (e.g., 2/31/YYYY)
    IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
    tMD = MD
    while(is.na(IsValid)){
      tMD = tMD - 1
      IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
    }
    JMaturityDate = IsValid
    Counter = 0  
    if(Frequency == 1){   # Annual
      while (JMaturityDate > JSettlementDate){
        MY = MY - 1
        IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
        tMD = MD
        while(is.na(IsValid)){
          tMD = tMD - 1
          IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
        }
        JMaturityDate = IsValid
        Counter = Counter + 1
      }
      MY = MY + 1
      IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
      tMD = MD
      while(is.na(IsValid)){
        tMD = tMD - 1
        IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
      }
      JNextMaturityDate = IsValid
      tempE = (JSettlementDate - JMaturityDate) / 
        (JNextMaturityDate - JMaturityDate)
    } else if (Frequency == 2) { # Semi-annual
      while (JMaturityDate > JSettlementDate){
        if(MM > 6){
          MM = MM - 6
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        } else {
          MM = MM + 6
          MY = MY - 1
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        }
        Counter = Counter + 1
      }
      if(MM > 6){
        MM = MM - 6
        MY = MY + 1
      } else {
        MM = MM + 6
      }
      IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
      tMD = MD
      while(is.na(IsValid)){
        tMD = tMD - 1
        IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
      }
      JNextMaturityDate = IsValid
      tempE = (JSettlementDate - JMaturityDate) / 
        (JNextMaturityDate - JMaturityDate)
    } else if (Frequency == 4) { # Quarterly
      while (JMaturityDate > JSettlementDate){
        if(MM > 3){
          MM = MM - 3
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        } else {
          MM = MM + 9
          MY = MY - 1
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        }
        Counter = Counter + 1
      }
      if(MM > 9){
        MM = MM - 9
        MY = MY + 1
      } else {
        MM = MM + 3
      }
      IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
      tMD = MD
      while(is.na(IsValid)){
        tMD = tMD - 1
        IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
      }
      JNextMaturityDate = IsValid
      tempE = (JSettlementDate - JMaturityDate) / 
        (JNextMaturityDate - JMaturityDate)
    } else { # Monthly
      while (JMaturityDate > JSettlementDate){
        if(MM > 1){
          MM = MM - 1
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        } else {
          MM = MM + 11
          MY = MY - 1
          IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
          tMD = MD
          while(is.na(IsValid)){
            tMD = tMD - 1
            IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
          }
          JMaturityDate = IsValid
        }
        Counter = Counter + 1
      }
      if(MM > 11){
        MM = MM - 11
        MY = MY + 1
      } else {
        MM = MM + 1
      }
      IsValid = mdy.date(MM, MD, MY, nineteen = FALSE)
      tMD = MD
      while(is.na(IsValid)){
        tMD = tMD - 1
        IsValid = mdy.date(MM, tMD, MY, nineteen = FALSE)
      }
      JNextMaturityDate = IsValid
      tempE = (JSettlementDate - JMaturityDate) / 
        (JNextMaturityDate - JMaturityDate)
    }
    ElapsedOutput <- list(tempE, JMaturityDate, JNextMaturityDate, JSettlementDate)
    names(ElapsedOutput) <- c("Fraction", "LastDate", "NextDate", "CurrentDate")
    return(ElapsedOutput) 
  })
}  
# Return only fraction elapsed since last coupon
FractionElapsed = function(B){
  with(B, {
    ElapsedOutput = Elapsed(B)
    return(ElapsedOutput$Fraction) 
  })
}  
#
#AccruedInterest: Dollar amount of interest accrued on bond since last coupon
#
AccruedInterest = function(B){
  with(B,{
    return( (CouponRate/(Frequency*100.0)*Par*FractionElapsed(B)) ) 
  })
}
#
# BondValue: Dollar value of bond including accrued interest
#
BondValue = function(B){
  with(B,{
    PV = 0.0
    RemainingCoupons = CouponsRemaining(B)
    ElapsedTime = FractionElapsed(B)
    for(i in 1:RemainingCoupons){
      PV = PV + ( (CouponRate/(Frequency*100.0))*Par ) /
        ((1.0 + (YieldToMaturity/(Frequency*100.0)))^(i - ElapsedTime))
    }
    PV = PV+Par/((1.0 +
      (YieldToMaturity/(Frequency*100.0)))^(RemainingCoupons - ElapsedTime))
    return( PV ) 
  })
}
#
# BondValue: Time to maturity
#
TimeToMaturity = function(B){
  with(B,{
    RemainingCoupons = CouponsRemaining(B)
    ElapsedTime = FractionElapsed(B)
    TTM = (RemainingCoupons - ElapsedTime) / Frequency
    return( TTM ) 
  })
}
#
# Function that finds the difference between market price and model value 
#  (used in ytm below)
#
PriceDifference <- function(YTM, B){
  tempActualPrice = B$BondPrice + AccruedInterest(B)
  originalYTM = B$YieldToMaturity
  B$YieldToMaturity = YTM
  tempBondValue = BondValue(B)
  PD = abs(tempActualPrice - BondValue(B))
  B$YieldToMaturity = originalYTM
  return( PD )
}
#
# Yield To Maturity
#
YieldToMaturitySolver = function(B){
# Minimize the objective function (PriceDifference) 
#  by changing YieldToMaturity
# Note using ActualPrice and not BondValue
# optimize will solve for the first parameter in the function 
#    (tempYieldToMatuirity in FRMPriceDifference here)
  solution = optimize(PriceDifference, B, interval = c(0.0001, 5000),
    tol = .Machine$double.eps^0.25)
  # Print YieldToMaturity that equates actual and model bond prices
  YTM = solution$minimum
  return( YTM ) 
}
#
# BondValue: Dollar value of bond including accrued interest 
#  based on LSC model fit
#
BondValueDF = function(B, LSC){
  with(B,{
    PV = 0.0
    RemainingCoupons = CouponsRemaining(B)
    ElapsedTime = Elapsed(B)
    ElapsedTime = ElapsedTime$Fraction
    CF <- (CouponRate/(Frequency*100.0))*Par
    for(i in 1:RemainingCoupons){
      if(i == RemainingCoupons) CF <- CF + Par
# Need cc discount rate
      if(i == 1){
        LSC$Maturity <- 0.00001
      } else {
        LSC$Maturity <- ((i - ElapsedTime)/2.0) #- 0.5
      }
      Rate <- LSCRate(LSC)
      PV <- PV + CF*exp(-(Rate/100)*LSC$Maturity)
    }
    return( PV )
  })
}
