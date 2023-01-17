# 3.1 Calendar Test.R
# Insight: Illustrate various day counting with jrvFinance package
# jrvFinance: yearFraction()
# date: mdy.date()
# Exploring the package jrvFinance
# rmarkdown::render("3.1 Calendar Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  jrvFinance - basic financial analysis functions (similar to spreadsheets) 
#?jrvFinance # Remember the help command (only works if installed)
Packages <- c("jrvFinance", "date", "tis") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Part 1: Computing fraction of year with jrvFinance package
InputStartMonth <- 6
InputStartDay <- 1
InputStartYear <- 2020
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
InputEndMonth <- 6
InputEndDay <- 1
InputEndYear <- 2021
JulianEndDate = mdy.date(InputEndMonth, InputEndDay, InputEndYear, nineteen = FALSE)
d1 <- JulianStartDate
d2 <- JulianEndDate
r1 <- mdy.date(1, 1, InputStartYear, nineteen = FALSE) # Beginning of year, not June
r2 <- mdy.date(1, 1, InputEndYear, nineteen = FALSE)
# 2020 is a leap year, see jrvFinance package help
F1 <- yearFraction(d1, d2, r1, r2, freq = 2, convention = "30/360") # 360/360
F2 <- yearFraction(d1, d2, r1, r2, freq = 1, convention = "ACT/ACT") # 366/365
F3 <- yearFraction(d1, d2, r1, r2, freq = 2, convention = "ACT/360") # 366/360
F4 <- yearFraction(d1, d2, r1, r2, freq = 2, convention = "30/360E") # 360/360 (semi ?)
F5 <- yearFraction(d1, d2, r1, r2, freq = 12, convention = "ACT/ACT") # Monthly
F6 <- daycount.actual(d1, d2, variant = "bond")
F7 <- daycount.30.360(d1, d2, variant = "US")
F1; F2; F3; F4; F5; F6; F7
# Part 2: Find today on the system
?'Sys.Date'
TodaysDate = Sys.Date() # But in unusable format
TodaysYear <- as.integer(format(TodaysDate, "%Y")) # year -- upper case
TodaysMonth <- as.integer(format(TodaysDate, "%m")) # month -- note case sensitive
TodaysDay <- as.integer(format(TodaysDate, "%d")) # day -- note case sensitive
JulianTodaysDate = mdy.date(TodaysMonth, TodaysDay, TodaysYear, nineteen = FALSE)
TodaysDate; TodaysYear; TodaysMonth; TodaysDay
# Difference in days
JulianTodaysDate = mdy.date(TodaysMonth, TodaysDay, TodaysYear, nineteen = FALSE)
InputStartMonth <- 6
InputStartDay <- 14
InputStartYear <- 1986
JulianStartDate = mdy.date(InputStartMonth,InputStartDay,InputStartYear,nineteen = FALSE)
ActualDays = abs(JulianTodaysDate - JulianStartDate)
YearsIn365 = ActualDays/365.0
YearsIn365
# Just checking various date functions
SDate = date.mdy(JulianStartDate)
SMonth = SDate$month
SDay = SDate$day
SYear = SDate$year
SDate; SMonth; SDay; SYear
EDate = date.mdy(JulianTodaysDate)
EMonth = EDate$month
EDay = EDate$day
EYear = EDate$year
EDate; EMonth; EDay; EYear
x <- 20220101 # Friday and holiday
x <- mdy.date(1, 1, 2021, nineteen = FALSE)
# Part 3: See package tis: Time Indexes and Time Indexed Series
board <- FALSE # Presidential inauguration is not a holiday
nextBusinessDay(x, holidays = NULL, goodFriday = F, board = F, inaug = board)
previousBusinessDay(x, holidays = NULL, goodFriday = F, board = F, inaug = board)
isHoliday(x, goodFriday = TRUE, board = FALSE, inaug = board, businessOnly = TRUE)
isBusinessDay(x)
isGoodFriday(x)
isEaster(x)
years <- 2021
holidays(years, goodFriday = F, board = F, inaug = board, businessOnly = T)
federalHolidays(years, board = F, businessOnly = T)
goodFriday(years)
easter(years)
inaugurationDay(years)
startTi <- 20190101
endTi <- 20210101
holidaysBetween(startTi, endTi, goodFriday = F, board = F, inaug = board,
  businessOnly = T)
#
# Part 4: Illustration of building your own date function
#  Test whether a date is valid and either move it to preceeding or following
#
source('Adjust Date.R')
TestMonth <- 9
TestDay <- 26 # Normal Tuesday
TestYear <- 2019
Convention <- "MBP" # Modified Business Following or MBP (Preceeding)
AdjustDate(TestMonth, TestDay, TestYear, Convention)
TestDay <- 29 # Sunday
AdjustDate(TestMonth, TestDay, TestYear, Convention)
Convention <- "MBF"
AdjustDate(TestMonth, TestDay, TestYear, Convention)

