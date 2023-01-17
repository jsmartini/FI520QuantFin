# 4.3 NDDM Valuation Test.R
# rmarkdown::render("4.3 NDDM Valuation Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
Packages <- c("openxlsx") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# NDDM Inputs
DaysPerYear <- 365.25
DividendsPerYear <- 4 # Dividend policy changes 1x per year
DeltaTau <- 1/DividendsPerYear # Time between dividends (except stub)
#
# Stub Component Inputs
#
LastDividend <- 1.0 # Quarterly dividend dollar amount
DeltaTau1 <- 0.1 # Only if on a dividend payment date, otherwise input value
StubPayments <- 2  # Number of dividends until next change in dividend amount
ForwardRate0 <- 0.10 # Continuously compounded, annualized
#
# Series Component Inputs
#
NumberOfStages <- 3
# NumberOfStages+3: Stub, last infinite series and totals
Series = data.frame(matrix(vector(), NumberOfStages+3, 6, dimnames=list(c(), 
  c("Stage", "ForwardRate", "GrowthRate", "YearsInStage", "SeriesValue", 
    "InitialDividend"))), stringsAsFactors=F)
Series$Stage[1] <- 0
Series$ForwardRate[1] <- ForwardRate0
Series$GrowthRate[1] <- NA
Series$YearsInStage[1] <- StubPayments
Series$InitialDividend[1] <- LastDividend
Series$Stage[2] <- 1
Series$ForwardRate[2] <- 0.12
Series$GrowthRate[2] <- 0.06
Series$YearsInStage[2] <- 5
Series$InitialDividend[2] <- LastDividend
Series$Stage[3] <- 2
Series$ForwardRate[3] <- 0.09
Series$GrowthRate[3] <- 0.03
Series$YearsInStage[3] <- 5
Series$InitialDividend[3] <- LastDividend*exp(Series$GrowthRate[2] * 
  Series$YearsInStage[2])
Series$Stage[4] <- 3
Series$ForwardRate[4] <- 0.07
Series$GrowthRate[4] <- 0.01
Series$YearsInStage[4] <- 5
Series$InitialDividend[4] <- Series$InitialDividend[3] *
  exp(Series$GrowthRate[3]*Series$YearsInStage[3])
Series$Stage[5] <- 4
Series$ForwardRate[5] <- 0.06
Series$GrowthRate[5] <- 0.0
Series$YearsInStage[5] <- Inf
Series$InitialDividend[5] <- Series$InitialDividend[4] *
  exp(Series$GrowthRate[4]*Series$YearsInStage[4])
# Last stage needs very large number: Need these PVDs for future research
if(identical(Series$YearsInStage[NumberOfStages+2], Inf)){
  Series$YearsInStage[NumberOfStages+2] = 150
}
# Any number so that last dividend is greater than LengthDiv
LengthDiv <- StubPayments  # Number of future dividends to estimate 
# Be careful to put parentheses around addition in loops 
#  Including last infinite series as finite for future research
for(i in 2:(NumberOfStages+2)){
  LengthDiv <- LengthDiv + 4*Series$YearsInStage[i]
}
Div = data.frame(matrix(vector(), LengthDiv, 4, dimnames=list(c(), 
  c("MaturityTime", "DollarDividend", "PV", "PVD"
  ))), stringsAsFactors=F)
Div$MaturityTime <- 0.0
Div$DollarDividend <- 0.0
Div$PV <- 0.0
Div$PVD <- 0.0
#
# Stub Information
#
StubValue <- 0.0
for(i in 1:StubPayments){
  PaymentCounter = i
  Div$DollarDividend[PaymentCounter] <- LastDividend
  if(i == 1){
    Div$MaturityTime[PaymentCounter] <- DeltaTau1
    Div$PV[PaymentCounter] <- exp(-ForwardRate0 * DeltaTau1)
  } else {
    Div$MaturityTime[PaymentCounter] <- Div$MaturityTime[PaymentCounter-1] + DeltaTau
    Div$PV[PaymentCounter] <- Div$PV[PaymentCounter-1]*exp(-ForwardRate0*DeltaTau)
  }
  Div$PVD[PaymentCounter] <- Div$DollarDividend[PaymentCounter] * Div$PV[PaymentCounter]
  StubValue <- StubValue + Div$PVD[PaymentCounter]
}
Series$SeriesValue[1] <- StubValue
#
# Series Dividend Information
#
for(i in 1:(NumberOfStages+1)){
  Series$SeriesValue[i+1] <- 0.0
  for(j in 1:Series$YearsInStage[i+1]){
    for(k in 1:DividendsPerYear){
      PaymentCounter = PaymentCounter + 1
      Div$MaturityTime[PaymentCounter] <- Div$MaturityTime[PaymentCounter-1]+ 
        DeltaTau
      Div$PV[PaymentCounter] <- Div$PV[PaymentCounter-1] *
        exp(-Series$ForwardRate[i+1]*DeltaTau)
      if(k == 1){
        Div$DollarDividend[PaymentCounter] <- 
          Div$DollarDividend[PaymentCounter-1]*exp(Series$GrowthRate[i+1])
      } else {
        Div$DollarDividend[PaymentCounter] <- 
          Div$DollarDividend[PaymentCounter-1]
      }
      Div$PVD[PaymentCounter] <- Div$DollarDividend[PaymentCounter] * 
        Div$PV[PaymentCounter]
      Series$SeriesValue[i+1] <- Series$SeriesValue[i+1] + 
        Div$PVD[PaymentCounter]
    }
  }
}
Series$SeriesValue[NumberOfStages+3] <- 0.0
N <- NumberOfStages + 2
for(i in 1:N){
  Series$SeriesValue[NumberOfStages+3]<-Series$SeriesValue[NumberOfStages+3]+ 
    Series$SeriesValue[i]
}
# Infinite series, but use finite to estimate: 
#   Will need each dividend for subsequent work
write.xlsx(Div, file = "Dividends.xlsx", col.names = TRUE, 
  sheetName = "Dividends", showNA = TRUE, row.names = FALSE)
write.xlsx(Series, file = "Series.xlsx", col.names = TRUE, 
  sheetName = "Series", showNA = TRUE, row.names = FALSE)
StockValue <- Series$SeriesValue[NumberOfStages+3]
StockValue
# Plot of present value of dividends
MinMaturity = 0; MaxMaturity = max(Div$MaturityTime)
xlim1 = c(1:2); xlim1[1] = MinMaturity; xlim1[2] = MaxMaturity
# OVERRIDE
xlim1[1] = 0; xlim1[2] = 30
MinPVD = 0; MaxPVD = max(Div$PVD)
ylim1 = c(1:2); ylim1[1] = MinPVD; ylim1[2] = MaxPVD
plot(Div$MaturityTime, Div$PVD, type="l", 
  main="Present Vaue of Dividends", xlab="Maturity", ylab="PVD", col="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
# Plot of dividends
# MinMaturity = 0; MaxMaturity = max(Div$MaturityTime)
# xlim1 = c(1:2); xlim1[1] = MinMaturity; xlim1[2] = MaxMaturity
MinD = min(Div$DollarDividend); MaxD = max(Div$DollarDividend)
ylim1 = c(1:2); ylim1[1] = MinD; ylim1[2] = MaxD
plot(Div$MaturityTime, Div$DollarDividend, type="l", 
  main="Dollar Dividends", xlab="Maturity", ylab="Dollar Dividends", col="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
# Plot of present value of $1
# MinMaturity = 0; MaxMaturity = max(Div$MaturityTime)
# xlim1 = c(1:2); xlim1[1] = MinMaturity; xlim1[2] = MaxMaturity
MinPV = 0; MaxPV = 1
ylim1 = c(1:2); ylim1[1] = MinPV; ylim1[2] = MaxPV
plot(Div$MaturityTime, Div$PV, type="l", 
  main="Present Vaue of $1", xlab="Maturity", ylab="Present Value $1", col="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)



