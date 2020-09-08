#============
#seir_model.R
#============
#Simple SEIR model of novel corona virus infection

#---------------
#Attach packages
#---------------
library(deSolve)
library(ggplot2)
library(scales)

#--------------------
#Define global values
#--------------------
#Increments and length of simulation (days)
SimTime <- seq(0, 400, by = 1)
#Total population
TotPop <- 1e7
#Initial population that is exposed to infector
InitE <- 100
#Proportion of population having symptoms that is quarantined
QuarantineRate <- 1
#Number of days between exposure and showing symptoms
LatencyDuration <- 5
#Number of days that symptoms persist
SymptomDuration <- 10
#Number of days that a persons is hospitalized
HospitalDuration <- 14
#Proportion of the infected population that is asymptomatic
AsymptomaticProp <- 0.25
#Proportion of the infected population that is hospitalized
PropHospitalized <- 0.2
#Proportion of the hospitalized population who die
HospitalPropWhoDie <- 0.2
#Basic reproduction number (https://en.wikipedia.org/wiki/Basic_reproduction_number)
R0 <- 3
#Percentage reduction in infection rate with high social-distancing policy
HiSDPct <- 90
#Percentage reduction in infection rate with low social-distancing polity
LoSDPct <- 0
#Number of persons infected threshold for triggering high social-distancing policy
HiSDThreshold <- 1000
#Number of persons infected threshold for triggering low social-distancing policy
LoSDThreshold <- 10
#Initialize vector of number of number of symptomatic persons
NumSymptom_ <- numeric(length(SimTime))
#Initialize vector of change in the number of symptomatic persons
ChgSymptom_ <- numeric(length(SimTime))
#Intialize social-distancing policy level (0 = low, 1 = high)
SDPolicy <- 0

#------------------------------------------
#Define stocks and their initial conditions
#------------------------------------------
#See corona_model.odt
Stocks <- c(
  S = TotPop - InitE, 
  E = InitE, 
  N = 0,
  M = 0,
  H = 0,
  R = 0,
  D = 0)

#-----------------
#Define parameters
#-----------------
#See corona_model.odt
Auxs <- c(
  dl = LatencyDuration,
  dm = SymptomDuration,
  dh = HospitalDuration,
  qr = QuarantineRate,
  pc = 1 - AsymptomaticProp,
  ph = PropHospitalized,
  pd = HospitalPropWhoDie,
  R0 = R0)

#----------------------
#Define model equations
#----------------------
#See corona_model.odt
SirModel <- function(Time, Stocks, Auxs) {
  with(as.list(c(Stocks, Auxs)), {
    #Calculate number of people having symptoms and update tally
    NumSymptom <- M + H
    NumSymptom_[Time + 1] <<- NumSymptom
    #Calculate change in number of people having symptoms and update tally
    if (Time == 0) {
      ChgSymptom <- NumSymptom
    } else {
      ChgSymptom <- NumSymptom - NumSymptom_[Time]
    }
    ChgSymptom_[Time + 1] <<- ChgSymptom
    #Determine social distancing policy
    Policy <- SDPolicy
    OverHiSDThreshold <- NumSymptom > HiSDThreshold
    UnderLoSDThreshold <- NumSymptom < LoSDThreshold
    if (Policy == 0 & OverHiSDThreshold) {
      Policy <- 1
    }
    if (Policy == 1 & UnderLoSDThreshold) {
      Policy <- 0
    }
    SDPolicy <<- Policy
    #Calculate infection rate adjustment (ia)
    if (Policy == 1) {
      SDPct <- HiSDPct
    }
    if (Policy == 0) {
      SDPct <- LoSDPct
    }
    ia <- 1 - SDPct / 100
    #Calculate change values
    B0 <- R0 / (dl + dm)
    ps <- S / TotPop
    se <- B0 * ia * ps * (E + N + M * (1 - qr))
    en <- E * (1 - pc) / dl
    em <- E * pc * (1 - ph) / dl
    eh <- E * pc * ph / dl
    nr <- N * 1 / dm
    mr <- M * 1 / dm
    hr <- H * (1 - pd) / dh
    hd <- H * pd / dh
    dS_dt <- -se
    dE_dt <- se - em - eh
    dN_dt <- en - nr
    dM_dt <- em - mr
    dH_dt <- eh - hr - hd
    dR_dt <- nr + mr + hr
    dD_dt <- hd
    return(list(
      c(dS_dt, dE_dt, dN_dt, dM_dt, dH_dt, dR_dt, dD_dt),
      Infected = E + M + H,
      Infections = se,
      NumSymptom = NumSymptom,
      SDPolicy = Policy,
      SDLevel = SDPct, 
      OverHiSDThreshold = as.numeric(OverHiSDThreshold)
    ))
  })
}

#-----------------
#Calculate results
#-----------------
SirResults_df <- data.frame(ode(
  y = Stocks,
  times = SimTime,
  func = SirModel,
  parms = Auxs,
  method = 'euler'
))

#------------
#Plot results
#------------
ToPlot_ <- c("E", "N", "H", "D", "NumSymptom")
matplot(SirResults_df$time, SirResults_df[, ToPlot_], type = "l")
legend(
  "topleft", 
  legend = ToPlot_,
  lty = 1:length(ToPlot_),
  col = 1:length(ToPlot_)
)

max(SirResults_df$D)
