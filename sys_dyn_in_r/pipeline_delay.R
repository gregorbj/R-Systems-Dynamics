#pipeline_delay.R

# Attach packages
library(deSolve)
library(ggplot2)
library(scales)

# Define model data structures
SimTime<-seq(0, 20, by = 0.125)
Stocks <-c(MaterialInTransit = 400)
Auxs <- c(AverageDelay = 4)
InflowHistory_ <- numeric(length(SimTime))

# Define model function
Model <- function(Time, Stocks, Auxs){
  with(as.list(c(Stocks, Auxs)),{
    # Inflow is 100 time 0-7, else is 200 
    Inflow <- ifelse(Time < 8, 100, 200)
    InflowHistory_[Time + 1] <<- Inflow
    # Pipeline delay
    if (Time - AverageDelay < 0) {
      Outflow <- 100
    } else {
      Outflow <- InflowHistory_[1 + Time - AverageDelay]
    }
    # Change in material in transit
    dMT_dt  <- Inflow - Outflow
    # Return result
    return(
      list(
        c(dMT_dt),
        Inflow = Inflow,
        Outflow = Outflow
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
matplot(ModelOut_df$time, ModelOut_df[,-1], type = "l")
legend(
  "topleft", 
  legend = c("Material In Transit", "Inflow", "Outflow"),
  lty = 1:3,
  col = 1:3
  )




#----------------------------------------------------
# Original text file exported from Vensim
#  Material in Transit = INTEG( Inflow - Outflow , 400) 
#  Inflow = 100 + step ( 100, 8) 
#  Average Delay = 4
#  Outflow = DELAY FIXED ( Inflow ,Average Delay , 100) 
#  FINAL TIME = 20
#  INITIAL TIME = 0
#  TIME STEP = 0.125
#  SAVEPER = TIME STEP
#----------------------------------------------------
