#non-renewable_stock.R

library(deSolve)
        
SimTime <- seq(0, 200, 0.25)
Stocks <- c(Capital =  5, Resource = 1000)
Auxs <- c(
  DesiredGrowth = 0.07,
  DepreciationRate = 0.05,
  CostPerInvestment = 2.00,
  FractionReinvested = 0.12,
  RevenuePerUnit = 3
)

calcEfficiency <- approxfun(
  x = seq(0, 1000, by = 100),
  y = c(0, 0.25, 0.45, 0.63, 0.75, 0.85, 0.92, 0.96, 0.98, 0.99, 1),
  method = "linear",
  yleft = 0,
  yright = 1.0
)

Model <- function(Time, Stocks, Auxs) {
  with(as.list(c(Stocks, Auxs)), {
    Efficiency <- calcEfficiency(Resource)
    Extraction <- Efficiency * Capital
    TotalRevenue <- RevenuePerUnit * Extraction
    CapitalCosts <- Capital * 0.10
    Profit <- TotalRevenue - CapitalCosts
    CapitalFunds <- FractionReinvested * Profit
    MaximumInvestment <- CapitalFunds / CostPerInvestment
    DesiredInvestment <- Capital * DesiredGrowth
    Investment <- min(MaximumInvestment, DesiredInvestment)
    Depreciation <- Capital * DepreciationRate
    dS_dt <- Investment - Depreciation
    dR_dt <- -Extraction
    return(list(
      c(dS_dt, dR_dt),
      DesiredInvestment = DesiredInvestment,
      MaximumInvestment = MaximumInvestment,
      Investment = Investment,
      Depreciation = Depreciation,
      Extraction = Extraction
    ))
  })
}

Model_ode <- data.frame(
  ode(
    y = Stocks,
    times = SimTime,
    func = Model,
    parms = Auxs,
    method = "euler"
  )
)
