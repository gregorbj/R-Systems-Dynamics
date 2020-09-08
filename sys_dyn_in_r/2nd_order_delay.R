#2nd_order_delay.R

# Attach packages
library(deSolve)
library(ggplot2)
library(scales)

# Define model data structures
SimTime<-seq(0, 20, by = 0.125)
Stocks <-c(FirstTransit = 0, SecondTransit = 0)
Auxs <- c(FirstDelay = 2, SecondDelay = 4)

# Define model function
Model <- function(Time, Stocks, Auxs){
  with(as.list(c(Stocks, Auxs)),{
    # Inflow is 100 at start
    Inflow <- ifelse(Time == 0, 100, 0)
    # First delay
    Outflow1 <- FirstTransit / FirstDelay
    # Second delay
    Outflow2 <- SecondTransit / SecondDelay
    # Change in material in transit
    dFT_dt <- Inflow - Outflow1
    dST_dt <- Outflow1 - Outflow2
    # Return result
    return(
      list(
        c(dFT_dt, dST_dt),
        Inflow = Inflow,
        Outflow1 = Outflow1,
        Outflow2 = Outflow2
      )
    )
  })
}

# Run model
ModelOut_df <- data.frame(
  ode(
    y = Stocks,
    times = SimTime,
    func = Model,
    parms = Auxs,
    method='euler'
    )
  )

# Plot inflow, outflow, and material in transit as function of time
ToPlot_ <- c("Outflow1", "Outflow2")
matplot(ModelOut_df$time, ModelOut_df[, ToPlot_], type = "l")
legend(
  "topright", 
  legend = ToPlot_,
  lty = 1:length(ToPlot_),
  col = 1:length(ToPlot_)
  )
