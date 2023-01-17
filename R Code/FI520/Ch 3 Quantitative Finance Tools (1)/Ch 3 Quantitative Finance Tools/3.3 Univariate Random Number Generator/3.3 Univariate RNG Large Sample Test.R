# 3.3 Univariate RNG Large Sample Test.R
# Insight: Illustrate uniform random number generation with large sample sizes
# stats: runif(), mean(), sd(), functions()
# if("stats" %in% rownames(installed.packages()) == FALSE)install.packages("stats")
# rmarkdown::render("3.3 Univariate RNG Large Sample Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
Packages <- c("stats") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Input parameters and structures
SampleMean = -99.9
SampleStandardDeviation = -99.9
PopulationMean = -99.9
PopulationStandardDeviation = -99.9
SimulationTimeInSeconds = -99.9
FRMDSTATS <- data.frame(SampleMean, SampleStandardDeviation, PopulationMean, 
  PopulationStandardDeviation, SimulationTimeInSeconds)
#
# Example 1: Uniform Real
#
FRMUniformReal <- function(FRMDSTATS, SampleSize, RealLowerBound, UpperBound) {
# Return CPU (Central Processing Unit) times that the expression () used
  Time <- system.time(Draw<-runif(SampleSize, LowerBound, UpperBound), gcFirst = TRUE)
  FRMDSTATS[1] = mean(Draw, na.rm = TRUE) # Sample mean
  FRMDSTATS[2] = sd(Draw, na.rm = TRUE) # Sample standard deviation
  FRMDSTATS[3] = (LowerBound + UpperBound)/2.0 # Population mean
  FRMDSTATS[4] = (((UpperBound - LowerBound)^2)/12.0)^0.5 # Pop. standard deviation
  FRMDSTATS[5] = Time[3] # Simulation Time In Seconds
  return(FRMDSTATS)
}
# Test the function
SampleSize = 1000000
UpperBound = 100
LowerBound = -100
USS <- FRMUniformReal(FRMDSTATS, SampleSize, RealLowerBound, UpperBound)
USS
# Extract relevant data from vector if needed
SampleMean <- USS[1]
SampleStandardDeviation <- USS[2]
PopulationMean <- USS[3]
PopulationStandardDeviation <- USS[4]
SimulationTimeInSeconds <- USS[5]
rm(LowerBound, UpperBound, USS, FRMUniformReal) # Remove unwanted items
#
# Example 2: Integer Uniform Distribution random number generator
#
SampleSize = 1000000
UpperBound = 100
LowerBound = 0
Draw <- runif(SampleSize,0,1)
for (i in 1:SampleSize){
  Draw[i] = as.integer(Draw[i] * (UpperBound - LowerBound + 1)) + LowerBound
}
SampleMean = mean(Draw, na.rm = TRUE)
SampleStandardDeviation = sd(Draw, na.rm = TRUE)
PopulationMean = (LowerBound + UpperBound)/2.0
PopulationStandardDeviation = (((UpperBound - LowerBound + 1)^2 - 1.0)/12.0)^0.5
Time <- system.time(runif(SampleSize, LowerBound, UpperBound), gcFirst = TRUE)
SimulationTimeInSeconds = Time[3] # Simulation Time In Seconds
USS <- FRMDSTATS
USS[1] = PopulationMean
USS[2] = PopulationStandardDeviation
USS[3] = SampleMean
USS[4] = SampleStandardDeviation
USS[5] = SimulationTimeInSeconds
USS
rm(Draw, i, LowerBound, UpperBound, Time)
#
# Example 3: Likelihood random number generator
#
FRMLikelihood <- function(FRMDSTATS, SampleSize, Likelihood) {
  Time <- system.time(Draw <- runif(SampleSize, 0, 1), gcFirst = TRUE)
  for(i in 1:SampleSize){
    if(Draw[i] <= DesiredLikelihood) Draw[i] = 1.0
    else Draw[i] = 0.0
  }
  FRMDSTATS[1] = mean(Draw, na.rm = TRUE) # Sample mean
  FRMDSTATS[2] = sd(Draw, na.rm = TRUE) # Sample standard deviation
  FRMDSTATS[3] = DesiredLikelihood # Population mean
  FRMDSTATS[4] = sqrt(DesiredLikelihood-(DesiredLikelihood^2.0)) # Population standard deviation
  FRMDSTATS[5] = Time[3] # Simulation Time In Seconds
  return(FRMDSTATS)
}
# Test the function
SampleSize = 1000000
DesiredLikelihood = 0.5
USS <- FRMLikelihood(FRMDSTATS, SampleSize, Likelihood)
USS
rm(DesiredLikelihood, USS, FRMLikelihood) # Remove unwanted values and functions
#
# Example 4: Normal Distribution random number generator
#
FRMNormal <- function(FRMDSTATS, SampleSize, NMean, NSD) {
  Time <- system.time(Draw <- rnorm(SampleSize, NMean, NSD), gcFirst = TRUE)
  FRMDSTATS[1] = mean(Draw, na.rm = TRUE) # Sample mean
  FRMDSTATS[2] = sd(Draw, na.rm = TRUE) # Sample standard deviation
  FRMDSTATS[3] = NMean # Population mean
  FRMDSTATS[4] = NSD # Population standard deviation
  FRMDSTATS[5] = Time[3] # Simulation Time In Seconds
  return(FRMDSTATS)
}
# Test the function
SampleSize = 10000000
NMean = 15.0
NSD = 30.0
USS <- FRMNormal(FRMDSTATS, SampleSize, NMean, NSD)
USS
rm(NMean, NSD, USS, FRMNormal) # Remove unwanted values and functions

