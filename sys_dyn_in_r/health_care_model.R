#health_care_model.R

#Attach packages
library(deSolve)
library(ggplot2)
library(scales)

#Define simulation times
SimTime<-seq(0, 20, by = 1)

#-------------
#Demand Sector
#-------------

#Define demand model data structures
DemandStocks <-c(
  Age1Pop = 1e6, 
  Age2Pop = 1.5e6, 
  Age3Pop = 2e6, 
  Age4Pop = 0.5e6)
DemandAuxs <- c(
  DelayAge1 = 15, 
  DelayAge2 = 25,
  DelayAge3 = 25,
  BirthRate = 20 / 1000,
  DeathRate = 7 / 1000,
  Age1GpvRate = 3,
  Age2GpvRate = 4,
  Age3GpvRate = 5,
  Age4GpvRate = 10)

#Define demand model function
DemandModel <- function(Time, Stocks, Auxs) {
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

#Run demand model
DemandModel_df <- data.frame(ode(
  y = DemandStocks,
  time = SimTime,
  func = DemandModel,
  parms = DemandAuxs,
  method = "euler"
))

#---------------
#Delivery Sector
#---------------

#Define delivery model data structures
DeliveryStocks <- c(
  PatientsTreated = 2.4e7)
DeliveryAuxs <- c(
  TargetCompTime = 1,
  StdWrkDays = 250,
  StdGpProd = 24,
  DesiredGpPopRate = 0.8 / 1000
)
#Define function to calculate factor to adjust work days
calcWorkdayFactor <- approxfun(
  x = seq(0, 2.5, by = 0.25),
  y = c(0.75, 0.79, 0.84, 0.90, 1, 1.09, 1.17, 1.23, 1.25, 1.25, 1.25),
  method = "linear",
  yleft = 0,
  yright = 1.0
)
#Define function to calculate pressure to increase daily productivity
calcProductivityFactor <- approxfun(
  x = seq(0, 2, by = 0.2),
  y = c(0.62, 0.65, 0.79, 0.84, 0.89, 1, 1.14, 1.24, 1.32, 1.37, 1.4),
  method = "linear",
  yleft = 0,
  yright = 1.0
)

#Define delivery model function
DeliveryModel <- function(Time, Stocks, Auxs) {
  with(as.list(c(Stocks, Auxs)), {
    #Get patient demand
    PatientVisits <- DemandModel_df$GPVisits[Time + 1]
    #Get number of GPs
    if (exists("SupplyModel_df")) {
      NumGP <- SupplyModel_df$NumGP[Time + 1]
    } else {
      NumGP <- DemandModel_df$TotPop[Time + 1] * DesiredGpPopRate
    }
    #Desired number of completed visits
    DesiredCompVisits <- PatientsTreated / TargetCompTime
    #Standard number of completed visits
    StdCompVisits <- NumGP * StdWrkDays * StdGpProd
    #System pressure
    SysPressure <- DesiredCompVisits / StdCompVisits
    #Adjust work days
    WorkDays <- calcWorkdayFactor(SysPressure) * StdWrkDays
    #Adjust daily GP productivity
    DailyGpProd <- calcProductivityFactor(SysPressure) * StdGpProd
    #Calculate potential completed vists
    PotentialCompVisits <- NumGP * WorkDays * DailyGpProd
    #Calculate completed visits
    CompVisits <- min(DesiredCompVisits, PotentialCompVisits)
    #Calculate change in patients being treated
    dPT_dt <- PatientVisits - CompVisits
    #Return the result
    return(list(
      c(dPT_dt),
      DesiredCompVisits = DesiredCompVisits,
      StdCompVisits = StdCompVisits,
      SysPressure = SysPressure,
      WorkDays = WorkDays,
      DailyGpProd = DailyGpProd,
      PotentialCompVisits = PotentialCompVisits,
      CompVisits = CompVisits
    ))
  })
}

#Run the delivery model
DeliveryModel_df <- data.frame(ode(
  y = DeliveryStocks,
  time = SimTime,
  func = DeliveryModel,
  parms = DeliveryAuxs,
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

