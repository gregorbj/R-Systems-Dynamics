#s_shaped_growth.R

library(deSolve)
library(ggplot2)
library(gridExtra)

SimTime <- seq(0, 100, 0.25)
Stocks <- c(Stock = 100)
Auxs <- c(
  Capacity = 10000,
  RefAvailability = 1,
  RefGrowthRate = 0.1
)

Model <- function(Time, Stocks, Auxs) {
  with(as.list(c(Stocks, Auxs)), {
    Availability <- 1 - Stock / Capacity
    Effect <- Availability / RefAvailability
    GrowthRate <- RefGrowthRate * Effect
    NetFlow <- Stock * GrowthRate
    dS_dt <- NetFlow
    return(list(
      c(dS_dt),
      NetFlow = NetFlow,
      GrowthRate = GrowthRate,
      Effect = Effect,
      Availability = Availability
    ))
  })
}

Growth_ode <- data.frame(ode(
  y = Stocks,
  times = SimTime,
  func = Model,
  parms = Auxs,
  method = "euler"
))
