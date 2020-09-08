#econ_model_1.R

SimTime <- seq(0, 100, 0.25)
Stocks <- c(Machines = 100)
Auxs <- c(DepFraction = 0.1,
          Labor = 100,
          ReinvestFraction = 0.2)
Model <- function(Time, Stocks, Auxs) {
  with(as.list(c(Stocks, Auxs)), {
    EconomicOutput <- Labor * sqrt(Machines)
    Investment <- EconomicOutput * ReinvestFraction
    Discards <- Machines * DepFraction
    dM_dt <- Investment - Discards
    return(list(
      c(dM_dt),
      Investment = Investment,
      Discards = Discards,
      EconomicOutput = EconomicOutput
    ))
  })
}
Machine_ode <- data.frame(
  ode(
    y = Stocks,
    times = SimTime,
    func = Model,
    parms = Auxs,
    method = "euler"
  )
)