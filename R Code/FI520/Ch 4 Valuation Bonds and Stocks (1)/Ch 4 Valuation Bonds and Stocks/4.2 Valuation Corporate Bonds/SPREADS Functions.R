# SPREADS Functions.R
# Functions:
#   AdjustDate: Attempts to convert invalid dates to valid and also move off 
#     week-ends and holidays
#   PaymentsRemaining: Calculates number of payment dates remaing
#   NAD: Number of accrued days
#   LSCRate: Estimates rate based on maturity and LSC model
#     Discount rate: Maturity is at end of period 
#     Forward rate: Maturity is at beginning of rate period
#
# Holidays: tis: Time Indexes and Time Indexed Series
Packages <- c("tis", "date") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Appraise the validity of a date:
#   TM - Test month, TD - Test day, TY - Test year
AdjustDate = function(TM, TD, TY, Convention){
  TDate <- mdy.date(TM, TD, TY, nineteen = FALSE)
  # Evaluate month: If < 1, make 12 and decrement year, if > 12, make 12
  while(is.na(TDate)){
    if(TM < 1){
      TM = 12
      TY = TY - 1
    }
    if(TM > 12) TM = 12
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
  }
  # Modified business preceeding
  if(Convention == "MBP"){
    TDayOfWeek = weekdays(as.Date(TDate))
    if(TDayOfWeek == "Sunday"){
      TD = TD - 2
    } else if(TDayOfWeek == "Saturday"){
      TD = TD - 1
    }
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
    if(!isBusinessDay(TDate))TD = TD - 1
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
    # Check preceding date is week-end
    TDayOfWeek = weekdays(as.Date(TDate))
    if(TDayOfWeek == "Sunday"){
      TD = TD - 2
    } else if(TDayOfWeek == "Saturday"){
      TD = TD - 1
    }
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
  }
  # Modified business following
  if(Convention == "MBF"){
    TDayOfWeek = weekdays(as.Date(TDate))
    if(TDayOfWeek == "Sunday"){
      TD = TD + 1
    } else if(TDayOfWeek == "Saturday"){
      TD = TD + 2
    }
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
    if(!isBusinessDay(TDate))TD = TD + 1
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
  }
  # Check if following is a week-end
  TDayOfWeek = weekdays(as.Date(TDate))
  if(TDayOfWeek == "Sunday"){
    TD = TD + 1
  } else if(TDayOfWeek == "Saturday"){
    TD = TD + 2
  }
  TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
  while(is.na(TDate)){
    # Decrement one day: Note TD = 1 is always valid
    if(TD > 0)TD = TD - 1
    # If Day is negative
    if(TD <= 0){
      # Decrement month by 1, start with TD = 31
      TM = TM - 1
      if(TM < 1){
        TM = 12
        TY = TY - 1
      }
      TD = 31
    }
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
  }
  return(mdy.date(TM, TD, TY, nineteen = FALSE))
}
#
# PaymentsRemaining
#
PaymentsRemaining = function(N){
  PayDates <- c(1:1200)
  PayDates[] <- NA
  with(N,{
    SM = as.numeric(SettlementDateMonth)
    SD = as.numeric(SettlementDateDay)
    SY = as.numeric(SettlementDateYear)
    MM = as.numeric(MaturityDateMonth)
    MD = as.numeric(MaturityDateDay)
    MY = as.numeric(MaturityDateYear)
# Decrement settlement date until it is a valid date (e.g., 2/31/YYYY)
    JSettlementDate = AdjustDate(SM, SD, SY, Convention)
# Decrement maturity date until it is a valid date (e.g., 2/31/YYYY)
    JMaturityDate = AdjustDate(MM, MD, MY, Convention)
    PayDates[1] = JMaturityDate
    # Solve for number of remaining cash flows depending on payment frequency
    Counter = 0
    if (Frequency == 2) { # Semi-annual
      while (JMaturityDate > JSettlementDate){
        if(MM > 6){
          MM = MM - 6
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        } else {
          MM = MM + 6
          MY = MY - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        }
        Counter = Counter + 1
        PayDates[Counter+1] = JMaturityDate
      }
    } else { # Error
      Counter = -99
      PayDates[1] = -98
    }
    RData <- list(Counter, PayDates)
    names(RData) <- c("Counter", "PayDates")
    return(RData)
  })
}
#
# NAD: Number of Accrued Days
#
NAD = function(N){
  with(N, {
    SM = as.numeric(SettlementDateMonth)
    SD = as.numeric(SettlementDateDay)
    SY = as.numeric(SettlementDateYear)
    MM = as.numeric(MaturityDateMonth)
    MD = as.numeric(MaturityDateDay)
    MY = as.numeric(MaturityDateYear)
# Decrement settlement date until it is a valid date (e.g., 2/31/YYYY)
    JSettlementDate = AdjustDate(SM, SD, SY, Convention)
# Decrement maturity date until it is a valid date (e.g., 2/31/YYYY)
    JMaturityDate = AdjustDate(MM, MD, MY, Convention)
    Counter = 0
    if (Frequency == 2) { # Semi-annual
      while (JMaturityDate > JSettlementDate){
        if(MM > 6){
          MM = MM - 6
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        } else {
          MM = MM + 6
          MY = MY - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        }
        Counter = Counter + 1
      }
      if(MM > 6){
        MM = MM - 6
        MY = MY + 1
      } else {
        MM = MM + 6
      }
      JNextMaturityDate = AdjustDate(MM, MD, MY, Convention)
      OutNAD = as.numeric(JSettlementDate - JMaturityDate)
    } else { # Error
      OutNAD = -99
    }
    return(OutNAD)
  })
}
#
# LSC Rates
#
LSCRate = function(LSC){
  with(LSC, {
    if(NumberOfFactors==1){
      Rate = Intercept
    }
    if(NumberOfFactors==2){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1))
    }
    if(NumberOfFactors==3){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1))+
        Curvature1*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1) -
          exp(-Maturity/Tau1))
    }
    if(NumberOfFactors==4){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1))+
        Curvature1*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1) -
          exp(-Maturity/Tau1)) +
        Curvature2*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - 
          exp(-Maturity/Tau2))
    }
    if(NumberOfFactors==5){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1))+
        Curvature1*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1) -
          exp(-Maturity/Tau1)) +
        Curvature2*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - 
          exp(-Maturity/Tau2)) +
        Curvature3*((1.0 - exp(-Maturity/Tau3))/(Maturity/Tau3) - 
          exp(-Maturity/Tau3))
    }
    if(NumberOfFactors==6){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1))+
        Curvature1*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1) - 
          exp(-Maturity/Tau1)) +
        Curvature2*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - 
          exp(-Maturity/Tau2)) +
        Curvature3*((1.0 - exp(-Maturity/Tau3))/(Maturity/Tau3) - 
          exp(-Maturity/Tau3)) +
        Curvature4*((1.0 - exp(-Maturity/Tau4))/(Maturity/Tau4) -
          exp(-Maturity/Tau4))
    }
    if(NumberOfFactors==7){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1))+
        Curvature1*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1) - 
          exp(-Maturity/Tau1)) +
        Curvature2*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - 
          exp(-Maturity/Tau2)) +
        Curvature3*((1.0 - exp(-Maturity/Tau3))/(Maturity/Tau3) - 
          exp(-Maturity/Tau3)) +
        Curvature4*((1.0 - exp(-Maturity/Tau4))/(Maturity/Tau4) - 
          exp(-Maturity/Tau4)) +
        Curvature5*((1.0 - exp(-Maturity/Tau5))/(Maturity/Tau5) - 
          exp(-Maturity/Tau5))
    }
    if(NumberOfFactors==8){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1))+
        Curvature1*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1) - 
          exp(-Maturity/Tau1)) +
        Curvature2*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - 
          exp(-Maturity/Tau2)) +
        Curvature3*((1.0 - exp(-Maturity/Tau3))/(Maturity/Tau3) - 
          exp(-Maturity/Tau3)) +
        Curvature4*((1.0 - exp(-Maturity/Tau4))/(Maturity/Tau4) - 
          exp(-Maturity/Tau4)) +
        Curvature5*((1.0 - exp(-Maturity/Tau5))/(Maturity/Tau5) - 
          exp(-Maturity/Tau5)) +
        Curvature6*((1.0 - exp(-Maturity/Tau6))/(Maturity/Tau6) - 
          exp(-Maturity/Tau6))
    }
    return(Rate)
  })
}
#
# Given coefficients for discount curve based on LSC, 
#  estimate squared difference
#
DiffSwRates <- function(x, NFac, S, NCMTs, MSR){
  C <- numeric(NFac)   # Coefficients
  C[1] <- 1.0
  SR <- numeric(NCMTs)
  for(j in 1:NCMTs){
    NFix <- j*2 # Semi-annual pay (30/360 assumed)
    r <- numeric(NFix) # Discount rates
    DF <- numeric(NFix) # Discount factors  
    for(i in 1:NFix){
      tau = i/2.0 # Does this need to be lagged by 1/2?
      if(NFac==1){
        r[i] <- C[1]*x[1]
      }
      if(NFac==2){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        r[i] <- C[1]*x[1] + C[2]*x[2]
      }
      if(NFac==3){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3]
      }
      if(NFac==4){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3] + C[4]*x[4]
      }
      if(NFac==5){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3] + C[4]*x[4] + C[5]*x[5]
      }
      if(NFac==6){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3] + C[4]*x[4] + C[5]*x[5] +
          C[6]*x[6]
      }
      if(NFac==7){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
        C[7] <- (S[5]/tau)*(1-exp(-tau/S[5])) - exp(-tau/S[5]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3] + C[4]*x[4] + C[5]*x[5] +
          C[6]*x[6] + C[7]*x[7]
      }
      if(NFac==8){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
        C[7] <- (S[5]/tau)*(1-exp(-tau/S[5])) - exp(-tau/S[5]) 
        C[8] <- (S[6]/tau)*(1-exp(-tau/S[6])) - exp(-tau/S[6]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3] + C[4]*x[4] + C[5]*x[5] + 
          C[6]*x[6] + C[7]*x[7] + C[8]*x[8]
      }
      DF[i] <- exp(-(r[i]/100.0)*tau)
    }
    # r; DF
    SR[j] <- ((1.0 - DF[NFix])/(0.5*sum(DF)))*100.0
  }
  Diff <- sum((MSR - SR)^2, na.rm = TRUE)
  return(Diff)
}
# Given LSC estimates of discount rates, compute CMT curve
CMTRates <- function(y, NFac, S, NCMTs){
  C <- numeric(NFac)   # Coefficients
  C[1] <- 1.0
  SR <- numeric(NCMTs)
  for(j in 1:NCMTs){
    NFix <- j*2 # Semi-annual pay (30/360 assumed)
    r <- numeric(NFix) # Discount rates
    DF <- numeric(NFix) # Discount factors  
    for(i in 1:NFix){
      tau = i/2.0
      if(NFac==1){
        r[i] <- C[1]*y[1]
      }
      if(NFac==2){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        r[i] <- C[1]*y[1] + C[2]*y[2]
      }
      if(NFac==3){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3]
      }
      if(NFac==4){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4]
      }
      if(NFac==5){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5]
      }
      if(NFac==6){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5] +
          C[6]*y[6]
      }
      if(NFac==7){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
        C[7] <- (S[5]/tau)*(1-exp(-tau/S[5])) - exp(-tau/S[5]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5] +
          C[6]*y[6] + C[7]*y[7]
      }
      if(NFac==8){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
        C[7] <- (S[5]/tau)*(1-exp(-tau/S[5])) - exp(-tau/S[5]) 
        C[8] <- (S[6]/tau)*(1-exp(-tau/S[6])) - exp(-tau/S[6]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5] +
          C[6]*y[6] + C[7]*y[7] + C[8]*y[8]
      }
      DF[i] <- exp(-(r[i]/100.0)*tau)
    }
    # r; DF
    SR[j] <- ((1.0 - DF[NFix])/(0.5*sum(DF)))*100.0
  }
  return(SR)
}
# Given LSC estimates of discount curve, estimate discount rates
DiscountRates <- function(y, NFac, S, NCMTs){
  C <- numeric(NFac)   # Coefficients
  C[1] <- 1.0
  SR <- numeric(NCMTs)
  r <- numeric(NCMTs) # Discount rates
  for(j in 1:NCMTs){
    tau = j
    if(NFac==1){
      r[j] <- C[1]*y[1]
    }
    if(NFac==2){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      r[j] <- C[1]*y[1] + C[2]*y[2]
    }
    if(NFac==3){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3]
    }
    if(NFac==4){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4]
    }
    if(NFac==5){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
      C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5]
    }
    if(NFac==6){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
      C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
      C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5] +
        C[6]*y[6]
    }
    if(NFac==7){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
      C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
      C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
      C[7] <- (S[5]/tau)*(1-exp(-tau/S[5])) - exp(-tau/S[5]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5] +
        C[6]*y[6] + C[7]*y[7]
    }
    if(NFac==8){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
      C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
      C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
      C[7] <- (S[5]/tau)*(1-exp(-tau/S[5])) - exp(-tau/S[5]) 
      C[8] <- (S[6]/tau)*(1-exp(-tau/S[6])) - exp(-tau/S[6]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5] +
        C[6]*y[6] + C[7]*y[7] + C[8]*y[8]
    }
  }
  return(r)
}
