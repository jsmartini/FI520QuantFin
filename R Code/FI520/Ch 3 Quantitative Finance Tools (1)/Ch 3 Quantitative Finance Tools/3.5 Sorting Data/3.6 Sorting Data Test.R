# 3.6 Sorting Data Test.R
# sort(), order()
# read.delim(), head(), tail(), plot(), hist(), curve()
# Simple Sorting
# rmarkdown::render("3.6 Sorting Data Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date: functions for handling dates
#  data.table: fread, fast read, regular delimited files
#  moments: skewness
Packages <- c("data.table", "date", "moments") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Inputs contained in file
#SortData <- read.delim("SortingData.dat", header = FALSE, sep = "")
# SortingData.dat is space delimited, use = not <-
SortData <- fread("SortingData.dat", header = FALSE, sep = " ")
head(SortData,5) # Check successfully read
tail(SortData,5)
NumberOfObservations <- nrow(SortData)
DataVector <- c(1:NumberOfObservations)
DataVector <- SortData$V1
# Base R, single vector sorting
DataVectorSorted <- sort(DataVector, na.last = NA, method = "quick") 
head(DataVectorSorted,5) # Check successfully sorted
tail(DataVectorSorted,5)
# Sorting stock data from CSI, Inc.
LowerDate <- 19001231 # No bounds
UpperDate <- 25001231
# LowerDate <- 20061231 # Pre and post crisis
# UpperDate <- 20201231
# SortData <- read.delim("SO.PRN", header = TRUE, sep = ",")
Company <- "Southern Company" # Used in plot titles, change is new data used
SortData <- fread("SO.PRN", sep = ",") # Input variables faster, data.table
sapply(SortData, class) # Way too much stuff
# Upper and lower limit of dates
SortData <- SortData[SortData$Date <= UpperDate,]
SortData <- SortData[SortData$Date >= LowerDate,]
keeps <- c("Date", "Close") # Only keep variables needed here
SortData <- as.data.frame(SortData) # keeps does not work with data.table 
SortData <- SortData[keeps]
head(SortData,5) # Check successfully sorted
tail(SortData,5)
# Find start and finish date 
SortData$Year <- trunc(SortData$Date/10000)
SortData$Month <- trunc((SortData$Date - SortData$Year*10000)/100)
SortData$Day <- trunc(SortData$Date - SortData$Year*10000 - SortData$Month*100)
SortData$CDate <- as.Date(as.character(SortData$Date),format = "%Y%m%d")
SortData$JDate <- as.numeric(as.Date(as.character(SortData$Date),format = "%Y%m%d"))
# Note: ISO 8601 international standard format %Y-%m-%d
head(SortData,5) # Check successfully sorted
Length1 <- length(SortData$Close) # Number of observations for loop
StartDate <- SortData$CDate[1]
EndDate <- SortData$CDate[Length1]
SortData$FD <- NA
SortData$Return <- NA
for (i in 1:Length1){ # Compute first differences and percentage returns
  if(i > 1){
    SortData$FD[i] <- SortData$Close[i] - SortData$Close[i-1]
    SortData$Return[i] <- SortData$FD[i]/SortData$Close[i-1] 
  }
}
SortData <- SortData[-1,] # Remove first observation lost with differencing and returns
# Plot FD and Return by date (need Julian dates)
SortData$Year <- trunc(SortData$Date/10000)
SortData$Month <- trunc((SortData$Date - SortData$Year*10000)/100)
SortData$Day <- trunc(SortData$Date - SortData$Year*10000 - SortData$Month*100)
SortData$JDate <- as.numeric(as.Date(as.character(SortData$Date),format = "%Y%m%d"))
# Sort by First Difference
SortDataFD <- SortData[order(SortData$FD), ]
# Histogram of first differences
xTitle <- "First Difference"
y <- SortDataFD$FD
yMean <- round(mean(y), 4)
yStdDev <- round(sd(y), 4)
ySkewness <- round(skewness(y), 4) # moments package
yTitle <- "Empirical PDF"
Title1 <- "Daily First Difference"
mTitle <- paste0(Company, ' ', Title1)
# Plot footers
TSD = paste0('Dates(', StartDate)
TED = paste0(', ', EndDate, ')')
TM = paste0(', Mean=$', yMean)
TSDev = paste0(', Std. Dev.=$', yStdDev)
TSk = paste0(', Skewness=', ySkewness)
sTitle = paste0(TSD, TED, TM, TSDev, TSk)
hist(y, main=mTitle, breaks=50, freq=FALSE, col="black", labels = FALSE, 
  sub=sTitle, xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10)  
curve(dnorm(x, yMean, yStdDev), add=TRUE, col="black", lwd=4)
# Sort by return
SortDataR <- SortData[order(SortData$R), ]
# Histogram of returns
xTitle <- "Return (%)"
y <- SortDataR$R*100 # Express as percent
yMean <- round(mean(y), 4)
yStdDev <- round(sd(y), 4)
ySkewness <- round(skewness(y), 4) # moments package
yTitle <- "Empirical PDF"
Title1 <- "Daily Returns (%)"
mTitle <- paste0(Company, ' ', Title1)
# Plot footers
TSD = paste0('Dates(', StartDate)
TED = paste0(', ', EndDate, ')')
TM = paste0(', Mean=', yMean, '%')
TSDev = paste0(', Std. Dev.=', yStdDev, '%')
TSk = paste0(', Skewness=', ySkewness)
sTitle = paste0(TSD, TED, TM, TSDev, TSk)
hist(y, main=mTitle, breaks=50, freq=FALSE, col="black", labels = FALSE, 
  sub=sTitle, xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10)  
curve(dnorm(x, yMean, yStdDev), add=TRUE, col="black", lwd=4)
# Illustrte multiple Sort by Date (not needed-could have sorted on JDate)
SortDataDate <- SortData[order(SortData$Year, SortData$Month, SortData$Day), ]
# x axis same for next few plots
x <- SortDataDate$JDate
xlim1 = c(1:2); MinXValue = min(x); MaxXValue = max(x)
xlim1[1] = MinXValue; xlim1[2] = MaxXValue
x <- SortDataDate$CDate # Note conversion to character for plot
xTitle = "Date"
# Plot closing prices
y <- SortDataDate$Close
ylim1 = c(1:2); MinYValue = min(y); MaxYValue = max(y)
ylim1[1] = MinYValue; ylim1[2] = MaxYValue
yTitle = "Prices"
yMean <- round(mean(y), 4)
SortDataDate$yMean <- yMean
Title1 = "Prices"
mTitle <- paste0(Company, ' ', Title1)
# Plot footers
TSD = paste0('Dates(', StartDate)
TED = paste0(', ', EndDate, ')')
TM = paste0(', Mean=$', yMean)
sTitle = paste0(TSD, TED, TM)
plot(x, y, type="p", main=mTitle, sub=sTitle, xlab=xTitle, ylab=yTitle, 
  col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(x, SortDataDate$yMean, type="l", col="black", lwd=1.0, xlim = xlim1, 
  ylim = ylim1)
# First Differences
y <- SortDataDate$FD
ylim1 = c(1:2); MinYValue = min(y); MaxYValue = max(y)
ylim1[1] = MinYValue; ylim1[2] = MaxYValue
yTitle = "First Differences ($)"
yMean <- round(mean(y), 4)
SortDataDate$yMean <- yMean
yStdDev <- round(sd(y), 4)
ySkewness <- round(skewness(y), 4) # moments package
# Plot footers
TSD = paste0('Dates(', StartDate)
TED = paste0(', ', EndDate, ')')
TM = paste0(', Mean=$', yMean)
TSDev = paste0(', Std. Dev.=$', yStdDev)
TSk = paste0(', Skewness=', ySkewness)
sTitle = paste0(TSD, TED, TM, TSDev, TSk)
Title1 = "Daily First Differences ($)"
mTitle <- paste0(Company, ' ', Title1)
plot(x, y, type="p", main=mTitle, sub=sTitle, xlab=xTitle, ylab=yTitle, 
  col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(x, SortDataDate$yMean, type="l", col="black", lwd=1.0, xlim = xlim1, 
  ylim = ylim1)
# Returns
y <- SortDataDate$R*100.0
ylim1 = c(1:2); MinYValue = min(y); MaxYValue = max(y)
ylim1[1] = MinYValue; ylim1[2] = MaxYValue
yTitle = "Returns (%)"
yMean <- round(mean(y), 4)
SortDataDate$yMean <- yMean
yStdDev <- round(sd(y), 4)
ySkewness <- round(skewness(y), 4) # moments package
# Plot footers
TSD = paste0('Dates(', StartDate)
TED = paste0(', ', EndDate, ')')
TM = paste0(', Mean=', yMean, '%')
TSDev = paste0(', Std. Dev.=', yStdDev, '%')
TSk = paste0(', Skewness=', ySkewness)
sTitle = paste0(TSD, TED, TM, TSDev, TSk)
Title1 = "Daily Returns (%)"
mTitle <- paste0(Company, ' ', Title1)
plot(x, y, type="p", main=mTitle, sub=sTitle, xlab=xTitle, ylab=yTitle, 
  col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(x, SortDataDate$yMean, type="l", col="black", lwd=4.0, xlim = xlim1, 
  ylim = ylim1)
# First Differences -- Illustration of decimalization (min = $0.01)
y <- SortDataDate$FD
MinYValue = -0.05
MaxYValue = 0.05
ylim1[1] = MinYValue; ylim1[2] = MaxYValue
yTitle = "First Differences ($)"
yMean <- round(mean(y), 4)
SortDataDate$yMean <- yMean
yStdDev <- round(sd(y), 4)
ySkewness <- round(skewness(y), 4) # moments package
# Plot footers
TSD = paste0('Dates(', StartDate)
TED = paste0(', ', EndDate, ')')
TM = paste0(', Mean=$', yMean)
TSDev = paste0(', Std. Dev.=$', yStdDev)
TSk = paste0(', Skewness=', ySkewness)
sTitle = paste0(TSD, TED, TM, TSDev, TSk)
Title1 = "Daily First Differences +-$0.05"
mTitle <- paste0(Company, ' ', Title1)
plot(x, y, type="p", main=mTitle, sub=sTitle, xlab=xTitle, ylab=yTitle, 
  col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(x, SortDataDate$yMean, type="l", col="black", lwd=4.0, xlim = xlim1, 
  ylim = ylim1)
# Returns-Some drift down as prices rise
y <- SortDataDate$Return*100
MinYValue = -0.5
MaxYValue = 0.5
ylim1[1] = MinYValue; ylim1[2] = MaxYValue
yTitle = "Returns (%)"
yMean <- round(mean(y), 4)
SortDataDate$yMean <- yMean
yStdDev <- round(sd(y), 4)
ySkewness <- round(skewness(y), 4) # moments package
# Plot footers
TSD = paste0('Dates(', StartDate)
TED = paste0(', ', EndDate, ')')
TM = paste0(', Mean=', yMean, '%')
TSDev = paste0(', Std. Dev.=', yStdDev, '%')
TSk = paste0(', Skewness=', ySkewness)
sTitle = paste0(TSD, TED, TM, TSDev, TSk)
Title1 = "Daily Returns +-0.5%"
mTitle <- paste0(Company, ' ', Title1)
plot(x, y, type="p", main=mTitle, sub=sTitle, xlab=xTitle, ylab=yTitle, 
  col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(x, SortDataDate$yMean, type="l", col="black", lwd=4.0, xlim = xlim1, 
  ylim = ylim1)
