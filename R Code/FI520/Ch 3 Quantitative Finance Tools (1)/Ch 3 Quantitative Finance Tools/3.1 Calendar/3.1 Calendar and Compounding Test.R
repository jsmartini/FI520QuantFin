# 3.1 Calendar and Compounding Test.R
# Insight: Illustrate various day counting and interest compounding concepts.
#  Dealing with dates is very tricky in programing for a host of reasons.
#  Also illustrates day counting via ACT/360 and ACT/365.
# date: mdy.date(), date.mdy()
# base: while, if, paste(), format(), c(), as.numeric(), as.integer()
# To generate a Word document, in the console below insert:
# rmarkdown::render("3.1 Calendar and Compounding Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
Packages <- c("date") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Inputs
Principal = 1000000.00
Frequency = 2 # Payments per year: Must be integer when computing (12/Frequency)
Rate = 5.0  # Annual rate
InputStartMonth <- 1 # Input start and end dates
InputStartDay <- 1
InputStartYear <- 2020
InputEndMonth <- 1
InputEndDay <- 1
InputEndYear <- 2050
# Convert MDY to Julian: Returns a number (or vector) of type "date"
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
JulianStartDate
as.integer(JulianStartDate)
class(JulianStartDate)
JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
JulianEndDate
as.integer(JulianEndDate)
Days = JulianEndDate - JulianStartDate # Total number of days between start and end
class(Days) # Subtraction yields integer
JulianStartDate; JulianEndDate; Days # Note formatting of date
IStartDate = as.integer(JulianStartDate) # Convert to integer
IEndDate = as.integer(JulianEndDate)
IDays = IEndDate - IStartDate
IStartDate; IEndDate; IDays # Note formatting of date
# Convert Julian to MDY
StartDate = date.mdy(JulianStartDate) # Creates a list, month, day, year
StartDate
EndDate = date.mdy(JulianEndDate)
class(StartDate)
StartDate; EndDate
# Calculate number of payments between start and end date
k = 0
JulianNextDate = JulianStartDate # JulianNextDate keeps track of periodic payments
while(JulianNextDate < JulianEndDate){ # While loop: do this while the statement evaluations true
  k = k + 1 # Increment k by 1
  if(k == 1){ # Note this is assessment not assignment
    LastMonth = InputStartMonth
    LastDay = InputStartDay
    LastYear = InputStartYear
    NextDay = LastDay
    if ((12/Frequency) < 2){
      k = -99
      break
    };
    NextMonth = LastMonth + (12/Frequency)
    NextYear = LastYear
    if(NextMonth > 12){
      NextMonth = NextMonth - 12
      NextYear = NextYear + 1
    }
  } else {
    LastMonth = NextMonth
    LastDay = NextDay
    LastYear = NextYear
    NextMonth = NextMonth + (12/Frequency)
    if(NextMonth > 12){
      NextMonth = NextMonth - 12
      NextYear = NextYear + 1
    }
  }
  JulianLastDate = mdy.date(LastMonth, LastDay, LastYear, nineteen = FALSE)
  JulianNextDate = mdy.date(NextMonth, NextDay, NextYear, nineteen = FALSE)
}

if(k < 1){
 Error <- "Frequency is wrong"
 Error
} else {
  NumberOfPeriods = k + 1 # Includes start day
  FVACT360 = c(1:NumberOfPeriods) # Future value based on ACT/360
  FVACT365 = c(1:NumberOfPeriods) # Future value based on ACT/365
  ACT = c(1:NumberOfPeriods) # Actual number of days in coupon period
  FVACT360[1] = Principal # Assume both methods start with same initial amount
  FVACT365[1] = Principal
  ACT[1] = -99
  for(i in 1:NumberOfPeriods){
    if(i==1){
      LastMonth = InputStartMonth
      LastDay = InputStartDay
      LastYear = InputStartYear
      NextDay = LastDay
      NextMonth = LastMonth + (12/Frequency)
      NextYear = LastYear
      if(NextMonth > 12){
        NextMonth = NextMonth - 12
        NextYear = NextYear + 1
      }
    } else {
      LastMonth = NextMonth
      LastDay = NextDay
      LastYear = NextYear
      NextMonth = NextMonth + (12/Frequency)
      if(NextMonth > 12){
        NextMonth = NextMonth - 12
        NextYear = NextYear + 1
      }
      JulianLastDate = mdy.date(LastMonth, LastDay, LastYear, nineteen = FALSE)
      JulianNextDate = mdy.date(NextMonth, NextDay, NextYear, nineteen = FALSE)
      ACT[i] = as.numeric(JulianNextDate - JulianLastDate)
      # Assume period compounding
      PeriodRate = (Rate/100.0)/Frequency
      FVACT360[i] = FVACT360[i-1]*(1.0 + PeriodRate)^((ACT[i]/360)*Frequency)
      FVACT365[i] = FVACT365[i-1]*(1.0 + PeriodRate)^((ACT[i]/365)*Frequency)
    }
  }
  FVACT360
  FVACT365
  NumberOfPeriods
  ACT
  # TV - Terminal Values
  TVDifference = round(FVACT360[NumberOfPeriods] - FVACT365[NumberOfPeriods],0)
  TVDiff = round(FVACT360 - FVACT365, 0)
  TVACT360 = FVACT360[NumberOfPeriods]
  TVACT365 = FVACT365[NumberOfPeriods]
  # Formatting output for printing
  TVACT360 = paste("$",format(TVACT360, big.mark=","),sep="")
  TVACT365 = paste("$",format(TVACT365, big.mark=","),sep="")
  TVDiff1 = paste("$",format(TVDiff, big.mark=","),sep="")
  TVDifference = paste("$",format(TVDifference, big.mark=","),sep="")
  TVACT360; TVACT365; TVDifference
  TVDiff1 # Illustrates the dollar difference between two compounding methods
  Period = c(1:NumberOfPeriods)
  plot(Period, TVDiff)
}
Period
TVDiff
TVACT360
TVACT365

