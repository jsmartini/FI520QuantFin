# 6.3 Discount Curve Estimation Test.R
# Model to illustrate MVN simulation of discount rates 
# rmarkdown::render("6.3 Discount Curve Estimation Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Install requisite libraries: 
#   openxlsx - opening IRSwapBook.xlsx
PackagesToLibrary <- c("optimx", "MASS", "openxlsx")
if (length(setdiff(PackagesToLibrary, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(PackagesToLibrary, rownames(installed.packages())))  
} # Make sure libraries are installed on this computer
lapply(PackagesToLibrary, library, character.only = TRUE) # Load and attach libraries
rm(PackagesToLibrary)
# Numerous swap functions
source('SWAP Functions.R')
#
# Global inputs
#
FixRange <- TRUE
FRMax <- 3.0
FRMin <- 2.5
#
# Number of factors from 1 to 6
NF <- 3
# MVNParameters <- read.xlsx('Parameters IRS Discount Rate.xlsx')  
NSw <- 30 # Swap every year for 30 years
MarketSwapRates <- numeric(NSw)
for(i in 1:NSw){
  MarketSwapRates[i] <- NA
# # Small input data set
#   if(i==1)MarketSwapRates[i] <- 2.744
#   if(i==2)MarketSwapRates[i] <- 2.745
#   if(i==3)MarketSwapRates[i] <- 2.699
#   if(i==5)MarketSwapRates[i] <- 2.684
#   if(i==7)MarketSwapRates[i] <- 2.720
#   if(i==10)MarketSwapRates[i] <- 2.794
#   if(i==30)MarketSwapRates[i] <- 2.893
# Large input data set
  if(i==1)MarketSwapRates[i] <- 2.793
  if(i==2)MarketSwapRates[i] <- 2.764
  if(i==3)MarketSwapRates[i] <- 2.720
  if(i==4)MarketSwapRates[i] <- 2.709
  if(i==5)MarketSwapRates[i] <- 2.696
  if(i==6)MarketSwapRates[i] <- 2.711
  if(i==7)MarketSwapRates[i] <- 2.749
  if(i==8)MarketSwapRates[i] <- 2.767
  if(i==9)MarketSwapRates[i] <- 2.797
  if(i==10)MarketSwapRates[i] <- 2.809
  if(i==15)MarketSwapRates[i] <- 2.894
  if(i==20)MarketSwapRates[i] <- 2.929
  if(i==25)MarketSwapRates[i] <- 2.929
  if(i==30)MarketSwapRates[i] <- 2.923
}
# Scalars: Taus
NTau <- 4   # Must be between 0 and 5 (not inclusive, integer)
Tau <- numeric(NTau)   # b (Level, slope, and curvatures)
Tau[1] <- 2.0
if(NTau>1)Tau[2] <- 0.5
if(NTau>2)Tau[3] <- 7.0
if(NTau>3)Tau[4] <- 15.0
x <- numeric(NF)   # b (Level, slope, and curvatures)
Sc <- numeric(NTau) # Scalars
for(i in 1:NF){
  if(i==1){
    x[1] <- MarketSwapRates[30]    # Level: Might be NA
    if(is.na(x[1]))x[1] <- 5.0
    Sc[1] <- 0.0
  }
  if(i==2){
    x[2] <- MarketSwapRates[1] - MarketSwapRates[30] # Slope
    if(is.na(x[2]))x[2] <- 0.0
    Sc[1] <- Tau[1]
  }
  if(i>2){
    x[i] <- 0
    Sc[i-2] <- Tau[i-2]
  }
}
# x; NF; Sc; NSw; MarketSwapRates
Answer <- DiffSwRates(x, NF, Sc, NSw, MarketSwapRates)
# Answer
# OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NF, NSwaps = NSw, MSR = MarketSwapRates,
#   control=list(save.failures=TRUE, maxit=2500, all.methods=TRUE))
# OptOutput
# Method "nlminb" usually fastest: A bounds constrained quasi-Newton method (nlminb). This is a complicated code by
#   David Gay in the Bell Labs PORT library collection
OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NF, S = Sc, NSwaps = NSw, 
  MSR = MarketSwapRates, method=c('nlminb'), control=list(save.failures=FALSE, 
  maxit=2500)) #, all.methods=TRUE))
# OptOutput
y <- 0
y <- numeric(NF)
for(i in 1:NF){
  if(i==1)y[1] <- OptOutput$p1[1]
  if(i==2)y[2] <- OptOutput$p2[1]
  if(i==3)y[3] <- OptOutput$p3[1]
  if(i==4)y[4] <- OptOutput$p4[1]
  if(i==5)y[5] <- OptOutput$p5[1]
  if(i==6)y[6] <- OptOutput$p6[1]
}
SREstimates <- SwRates(y, NF, Sc, NSw)
DREstimates <- DiscountRates(y, NF, Sc, NSw)
y; SREstimates; DREstimates
Maturity <- seq(1:NSw)
# Plot footers
NFs = paste0('LSC Factors = ', NF)
Scs = paste0(', Scalars = ')
if(NF==1){
  Scs = paste0('')
  Ts = paste0('')
}
if(NF==2)Ts = paste0(Tau[1])
if(NF==3)Ts = paste0(Tau[1])
if(NF==4)Ts = paste0(Tau[1], ', ', Tau[2])
if(NF==5)Ts = paste0(Tau[1], ', ', Tau[2], ', ', Tau[3])
if(NF==6)Ts = paste0(Tau[1], ', ', Tau[2], ', ', Tau[3], ', ', Tau[4])
sTitle = paste(NFs, Scs, Ts)
MaxValue = max(Maturity, na.rm=TRUE)
MinValue = min(0.0, Maturity, na.rm=TRUE)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
if(FixRange){
  MaxValue = FRMax
  MinValue = FRMin
} else {
  MaxValue = max(MarketSwapRates, SREstimates, DREstimates, na.rm=TRUE)
  MinValue = min(MarketSwapRates, SREstimates, DREstimates, na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Market","LSC Fit","Discount Rates")
mTitle = "Swap Rates: January 18, 2019"
xTitle = "Maturity"
yTitle = "Rates"
lTitle = "Variable"
plot(Maturity, MarketSwapRates, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(Maturity, SREstimates, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(Maturity, DREstimates, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(2,2,2), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)

