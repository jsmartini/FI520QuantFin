# One Factor Models.R
#
# One factor, lnPVD, with intercept
#
LSC <- lm(formula = Div$lnPVD ~ 1)
Betas <- LSC$coefficients # Make clear grabbing beta coefficients
Level <- Betas[1]
PredictedlnPVD <- Level 
FittedlnPVD <- matrix(nrow = NumberOfDates, ncol = NumberOfMaturities)
i = 1
for (j in 1:NumberOfMaturities){
  FittedlnPVD[i,j] = Level 
}
# Plots
if(PlotlnPVD){
  MinMaturity = 0; MaxMaturity = max(Div$MaturityTime)
  xlim1 = c(1:2); xlim1[1] = MinMaturity; xlim1[2] = MaxMaturity
  MinlnPVD = min(Div$lnPVD, FittedlnPVD) 
  MaxlnPVD = max(Div$lnPVD, FittedlnPVD)
  ylim1 = c(1:2); ylim1[1] = MinlnPVD; ylim1[2] = MaxlnPVD
  plot(Div$MaturityTime, Div$lnPVD, type="l", 
    main="LSC Model-One Factor (Gray-Fitted, Black-lnPVD)", 
    xlab="Maturity", ylab="PVD", col="black", xlim = xlim1, ylim = ylim1,
    pch = 2, cex = 0.5)
  lines(Div$MaturityTime, FittedlnPVD[1,], type="l", col="darkgray", xlim = xlim1, 
        ylim = ylim1, pch = 2, cex = 0.5)
}
#
# One factor, PVD, with intercept
#
LSC <- lm(formula = Div$PVD ~ 1)
Betas <- LSC$coefficients # Make clear grabbing beta coefficients
Level <- Betas[1]
PredictedPVD <- Level
FittedPVD <- matrix(nrow = NumberOfDates, ncol = NumberOfMaturities)
i = 1
for (j in 1:NumberOfMaturities){
  FittedPVD[i,j] = Level 
}
# Plots
if(PlotPVD){
  MinMaturity = 0; MaxMaturity = max(Div$MaturityTime)
  xlim1 = c(1:2); xlim1[1] = MinMaturity; xlim1[2] = MaxMaturity
  MinPVD = min(Div$PVD, FittedPVD); MaxPVD = max(Div$PVD, FittedPVD)
  ylim1 = c(1:2); ylim1[1] = MinPVD; ylim1[2] = MaxPVD
  plot(Div$MaturityTime, Div$PVD, type="l", 
    main="LSC Model-One Factors (Gray-Fitted, Black-lnPVD)", 
    xlab="Maturity", ylab="PVD", col="black", xlim = xlim1, ylim = ylim1,
    pch = 2, cex = 0.5)
  lines(Div$MaturityTime, FittedPVD[1,], type="l", col="darkgray", xlim = xlim1, 
    ylim = ylim1, pch = 2, cex = 0.5)  
}
