# 3.1 Ancient Calendar Test.R
# Insight: Illustrate why packages should be used cautiously (ancient day counting)
#  Some details of ancient day counting need further clarification.
# rmarkdown::render("2.1 Ancient Calendar Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
#if("date" %in% rownames(installed.packages()) == FALSE)install.packages("date")
library(date)
# Input start date
InputStartMonth <- as.integer(1)
InputStartDay <- 1L
InputStartYear <- 1960L
InputStartMonth; InputStartDay; InputStartYear
# Number of days since 1/1/1960, consistent with SAS, Julian format, inconsistent with Excel
JulianStartDate = mdy.date(InputStartMonth, InputStartDay, InputStartYear, nineteen = FALSE)
JulianStartDate
as.numeric(JulianStartDate)
CheckDate = date.mdy(JulianStartDate) # Converts from Julian to M, D, Y list
CheckMonth = CheckDate$month
CheckDay = CheckDate$day
CheckYear = CheckDate$year
is.integer(CheckDay) # Not an integer, handles fraction of day
typeof(CheckDay)
class(CheckDay)
CheckDay = is.integer(CheckMonth)
is.integer(CheckYear)
# Managing system dates
Today = c(1:9)
Today[1] <- date()
charSize = nchar(Today[1])
Today[2] <- format(Sys.time(), "%a %b %d %H:%M:%S %Y")
Today[3] <-as.character(Sys.Date())
Today[4] <- as.character(Sys.time())
op <- options(digits.secs = 6)
Today[5] <- as.character(Sys.time())
Today[6] <- as.character(date.mmddyy(JulianStartDate))
Today[7] <- as.character(date.ddmmmyy(JulianStartDate))
Today[8] <- as.character(date.mmddyyyy(JulianStartDate))
Today[9] <- as.numeric(mdy.date(1, 1, 60))
Today
# Dates in a data frame
JulianD <- c(1:10)
Dates <- as.date(JulianD)
Dates  # Prints dates after 1Jan60
test <- data.frame(x = JulianD, date = Dates)
test
summary(test) # Summary statistics of Julian dates, nonsense but illustrates formatting

# Ancient day counting: Last BC date
InputStartMonth <- 12
InputStartDay <- 31
InputStartYear <- -1
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
# First AD date
InputEndMonth <- 1
InputEndDay <- 1
InputEndYear <- 1
JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
EstimatedDays = JulianEndDate - JulianStartDate
EstimatedDays 
# Ancient day counting: Last BC date
InputStartMonth <- 3
InputStartDay <- 14
InputStartYear <- -445
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
# First AD date
InputEndMonth <- 12
InputEndDay <- 31
InputEndYear <- -1
JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
EstimatedDaysBC = JulianEndDate - JulianStartDate
EstimatedDaysBC

# Ancient day counting: Last BC date
InputStartMonth <- 1
InputStartDay <- 1
InputStartYear <- 1
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
# First AD date
InputEndMonth <- 4
InputEndDay <- 6
InputEndYear <- 32
JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
EstimatedDaysAD = JulianEndDate - JulianStartDate
EstimatedDaysAD
EstimatedDays <- EstimatedDaysBC + EstimatedDaysAD - 1
EstimatedDays

# Ancient day counting: Well-known date of Artaxerxes' order to rebuild Jerusalem walls
InputStartMonth <- 3
InputStartDay <- 14
InputStartYear <- -445
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
# Sir Robert Anderson's estimate of Christ's entrance into Jerusalem (4/6/32AD)
InputEndMonth <- 4
InputEndDay <- 6
InputEndYear <- 32
JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
EstimatedDays = JulianEndDate - JulianStartDate
EstimatedDays # Why 2 extra days?

# Oops ... Should have been 173,880 (= 69 "weeks" * 7 = 483 years * 360)
ActualDays = 69*7*360

# Potential explanation: -300, -200, -100 wrongfully treated as leap years, 
# Also need to add the last day (4/6/32 inclusive)
N = (32 - (-445) - 1) * 365 # No zero year
InputStartMonth <- 3
InputStartDay <- 14
InputStartYear <- 32
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
# Sir Robert Anderson's estimate of Christ's entrance into Jerusalem (4/6/32AD)
InputEndMonth <- 4
InputEndDay <- 6
InputEndYear <- 32
JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
ADays = JulianEndDate - JulianStartDate + 1 # Inclusive
ADays # Why 2 extra days?
LeapYears = (32 - (-445) - 1)/4 
AdjustHundredYears = 3 # Not leap years
TotalDays = N + ADays + LeapYears - AdjustHundredYears # Not 400
TotalDays

# Audit: mdy.date() function for ancient days
#   Appears wrong on hundred year rule
InputStartMonth <- 1
InputStartDay <- 1
InputStartYear <- -100 # -300, -200, -100 wrong
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
# Sir Robert Anderson's estimate of Christ's entrance into Jerusalem (4/6/32AD)
InputEndMonth <- 1
InputEndDay <- 1
InputEndYear <- -99
JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
Days = JulianEndDate - JulianStartDate
Days 


# # Alternative: Source: Wikipedia "Julian day" (Wrong on all leap years!!!)
# M = InputStartMonth
# D = InputStartDay
# Y = InputStartYear
# JDNStart = ( (1461 * (Y + 4800 +   ((M - 14)%/%12)  ))%/%4 ) +
#   ((367 * (M - 2 - 12 * ((M - 14)%/%12)))%/%12) -
#   (  (3 * ((Y + 4900 +   ((M - 14)%/%12))  %/%100))%/%4  ) + D - 32075
# M = InputEndMonth
# D = InputEndDay
# Y = InputEndYear
# JDNEnd = ( (1461 * (Y + 4800 +   ((M - 14)%/%12)  ))%/%4 ) +
#   ((367 * (M - 2 - 12 * ((M - 14)%/%12)))%/%12) -
#   (  (3 * ((Y + 4900 +   ((M - 14)%/%12))  %/%100))%/%4  ) + D - 32075
# # JDNEnd = (1461 * (Y + 4800 + (M - 14)%/%12))%/%4 +
# #   (367 * (M - 2 - 12 * ((M - 14)%/%12)))%/%12 -
# #   (3 * ((Y + 4900 + (M - 14)%/%12)%/%100))%/%4 + D - 32075
# Days1 = JDNEnd - JDNStart
# Days; Days1

# # Alternative interpretation of Christ's crucifixion (needs more analysis)
# InputEndMonth <- 4
# InputEndDay <- 4 # Days start in the evening (3)
# InputEndYear <- 32
# JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
# Days = JulianEndDate - JulianStartDate
# Years360 = Days/360
# Years360
# # Number of "weeks"
# Weeks = Years360/7
# Weeks
# df = data.frame(date=c("32-04-04")) 
# df$day <- weekdays(as.Date(df$date, origin = 1990-01-01))
# df



