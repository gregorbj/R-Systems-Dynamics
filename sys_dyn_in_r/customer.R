#customer.R

library(deSolve)

Start <- 2015
Finish <- 2030
Step <- 0.25
SimTime <- seq(Start, Finish, Step)
Stocks <- c(Customers = 10000)
Auxs <- c(GrowthFraction = 0.08, DeclineFraction = 0.03)
Model <- function(Time, Stocks, Auxs) {
  with(as.list(c(Stocks, Auxs)), {
       Recruits <- Customers * GrowthFraction
       Losses <- Customers * DeclineFraction
       dC_dt <- Recruits - Losses
       return(
         list(
           dC_dt,
           Recruits = Recruits,
           Losses = Losses,
           GrowthFraction = GrowthFraction,
           DeclineFraction = DeclineFraction
         )
       )
       })
}
Customers_ode <- ode(
  Stocks,
  SimTime,
  Model,
  Auxs,
  method = "euler"
)
