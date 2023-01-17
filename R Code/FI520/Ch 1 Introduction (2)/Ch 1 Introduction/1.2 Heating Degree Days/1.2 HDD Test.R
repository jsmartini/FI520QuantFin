# 1.2 HDD Test.R
# rmarkdown::render("1.2 HDD Test.R", "word_document")
# HDD - heating degree day 
#   "The number of degrees that a day's average temperature is below 65 degrees 
#   Fahrenheit (approx. 18 degrees Celsius), the temperature below which 
#   buildings need to be heated. The price of weather derivatives traded in the 
#   winter is based on an index made up of monthly HDD values." Investopedia
rm(list = ls())  # Take out the Environment "trash"
# library(date)
# library(stats) # lm() - linear model
# Read a file and create data frame
HDDFile <- read.table("ATLHDD.csv", header = TRUE, sep = ",")
head(HDDFile, 5) # Show first 5 lines in Console
tail(HDDFile, 5) # Show first 5 lines in Console
is.data.frame(HDDFile) # TRUE: HDDFile is a data frame
is.numeric(HDDFile$Date) # FALSE: Imported as a factor 
is.factor(HDDFile$Date) # TRUE: A categorical variable, idealy limited in number (not here)
is.numeric(HDDFile$HDD) # TRUE: Numeric data suitable for mathematical calculations
is.numeric(HDDFile$Estimated) # TRUE
is.ordered(HDDFile$Date) # FALSE: It is stored as a factor variable 
is.character(HDDFile$Date) # FALSE
max(HDDFile$Estimated) # Estimated variable is not relevant here, returns the max
min(HDDFile$Estimated) # Minimum reported to the console
# Often data sets have columns that need to be deleted
HDDFile$Estimated <- NULL # Estimated column is removed
head(HDDFile, 5)
HDDFile$TDate <- as.character(HDDFile$Date) # Convert factor to character
is.factor(HDDFile$TDate) # Now FALSE
is.character(HDDFile$TDate) # Factor Date converted to character TDate
# Convert character date (TDate) to Julian date but display in calendar format
#   Note default output format is YYYY-MM-DD, format = "..." denotes input
HDDFile$TDate2 <- as.Date(HDDFile$TDate, format = "%m/%d/%y")
# Insights on dates
head(HDDFile, 5)
# Insights on dates
as.integer(HDDFile$Date) # Nonsense and definitely not a Julian date
as.integer(HDDFile$TDate) # Cannot convert character string to integer, NAs
as.integer(HDDFile$TDate2) # Julian dates, base = 1/1/1970
BaseDate = as.Date("1970-01-01")
as.integer(BaseDate)
head(HDDFile, 5)
# Some simple plots
# plot(HDDFile$Date, HDDFile$HDD) # x-axis not formatted correctly, nonsense
# plot(HDDFile$TDate, HDDFile$HDD) # TDate not formatted for plotting
plot(HDDFile$TDate2, HDDFile$HDD) 
# Thus, when programming, managing dates is tricky, and you must be careful
