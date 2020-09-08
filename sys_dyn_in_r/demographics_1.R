#demographics_1.R

# Attach packages
library(deSolve)
library(ggplot2)
library(scales)

# Define model data structures
SimTime<-seq(0, 20, by = 1)
Stocks <-c(
  Age1Pop = 1e6, 
  Age2Pop = 1.5e6, 
  Age3Pop = 2e6, 
  Age4Pop = 0.5e6)
Auxs <- c(
  DelayAge1 = 15, 
  DelayAge2 = 25,
  DelayAge3 = 25,
  BirthRate = 20 / 1000,
  DeathRate = 7 / 1000,
  Age1GpvRate = 3,
  Age2GpvRate = 4,
  Age3GpvRate = 5,
  Age4GpvRate = 10)

# Define model function
PopModel <- function(Time, Stocks, Auxs) {
  with(as.list(c(Stocks, Auxs)), {
    TotPop <- Age1Pop + Age2Pop + Age3Pop + Age4Pop
    Births <- TotPop * BirthRate
    Age1to2 <- Age1Pop / DelayAge1
    Age2to3 <- Age2Pop / DelayAge2
    Age3to4 <- Age3Pop / DelayAge3
    Deaths <- Age4Pop * DeathRate
    GPVisits <- 
      Age1Pop * Age1GpvRate +
      Age2Pop * Age2GpvRate +
      Age3Pop * Age3GpvRate +
      Age4Pop * Age4GpvRate
    dA1_dt <- Births - Age1to2
    dA2_dt <- Age1to2 - Age2to3
    dA3_dt <- Age2to3 - Age3to4
    dA4_dt <- Age3to4 - Deaths
    return(list(
      c(dA1_dt, dA2_dt, dA3_dt, dA4_dt),
      TotPop = TotPop,
      Births = Births,
      Age1to2 = Age1to2,
      Age2to3 = Age2to3,
      Age3to4 = Age3to4,
      Deaths = Deaths,
      GPVisits = GPVisits
    ))
  })
}

# Run Model
PopModel_df <- data.frame(ode(
  y = Stocks,
  time = SimTime,
  func = PopModel,
  parms = Auxs,
  method = "euler"
))

with(PopModel_df, plot(time, Births))
with(PopModel_df, plot(time, Deaths))
with(PopModel_df, plot(time, TotPop))
with(PopModel_df, plot(TotPop, GPVisits))
with(PopModel_df, plot(time, GPVisits / TotPop))
ToPlot_ <- c("Age1Pop", "Age2Pop", "Age3Pop", "Age4Pop")
matplot(PopModel_df$time, PopModel_df[, ToPlot_], type = "l")
legend(
  "bottomright", 
  legend = ToPlot_,
  lty = 1:length(ToPlot_),
  col = 1:length(ToPlot_)
)

