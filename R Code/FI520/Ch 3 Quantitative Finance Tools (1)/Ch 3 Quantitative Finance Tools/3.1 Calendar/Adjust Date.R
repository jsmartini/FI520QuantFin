# Adjust Date.R
#  Function to adjust date to valid business date
#  Support module for 3.1 Calendar Test.R
# Holidays: tis: Time Indexes and Time Indexed Series
#  isBusinessDay()
Packages <- c("date", "tis") 
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




