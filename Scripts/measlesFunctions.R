## Measles Outbreak in Stellenbosch
## DST-NRF Centre of Excellence in Epidemiological Modelling and Analysis (SACEMA)
## Stellenbosch University, Stellenbosch, South Africa 7600
##
## (C) Juliet R.C. Pulliam and James Azam 2017
##
#
# measlesFunctions.R - Defines functions to run and visualize results from a measles SEIR chain binomial model
#

## - Simulation functions - ##

# We assume that the Exposed class is split into 10 stages corresponding
# to the 10 days of latency. After these 10 days of being exposed, the infected
# individual remains infectious for 6 days then enters the immune class. Death from
# measles is not represented in the model.
#
#
# VARIABLE DESCRIPTIONS
# Sus = Susceptibles
# Exp1, Exp 2, ..., Exp10 = ten days of being exposed (latent infection)
# Inf1, Inf2, ..., Inf6 = six days of being infectious
# Rec = immune individuals
# maxTime is the maximum time the code can run for (even if there are infected individuals remaining, in which case a warning is given)


# INITIALIZEPOP() is a function that initializes the school population, using the first 2 generations of cases as inputs

initializePop <- function(N, initPropImmune, I0) {
  immune <- floor(N * initPropImmune)
  data.frame(
    Sus1 = N - immune,
    Sus2 = 0,
    Exp1 = 0,
    Exp2 = 0,
    Exp3 = 0,
    Exp4 = 0,
    Exp5 = 0,
    Exp6 = 0,
    Exp7 = 0,
    Exp8 = 0,
    Exp9 = 0,
    Exp10 = 0,
    Inf1 = I0,
    Inf2 = 0,
    Inf3 = 0,
    Inf4 = 0,
    Inf5 = 0,
    Inf6 = 0,
    Rec = immune
  )
}


# STEP() is a function that updates the various classes (S,E1...E10,I1...I10,R) and
# returns a dataframe containing the epidemic progression.

step <- function(pop, R0, browse = FALSE) {
  if (browse) browser()
  beta <- R0 / length(grep('Inf', names(pop))) #will this formula hold?
  totalPop <- sum(pop)
  totalInf <- sum(pop[, grep("Inf", names(pop))])
  inf_prob <- 1 - exp(-beta * totalInf / totalPop) #infection probability at any time t is 1 minus the probability of escaping infection
  
  incidence1 <- pop$Sus1 * inf_prob
  
  if(incidence1 > pop$Sus1){
    incidence1 <- pop$Sus1
  }
  
  incidence2 <- pop$Sus2 * inf_prob
  
  if(incidence2 > pop$Sus2){
      incidence2 <- pop$Sus2
  }
 
  updatePop <- data.frame(
    Sus1 = pop$Sus1 - incidence1
    , Sus2 = pop$Sus2 - incidence2
    , Exp1 = incidence1 + incidence2
    , Exp2 = pop$Exp1
    , Exp3 = pop$Exp2
    , Exp4 = pop$Exp3
    , Exp5 = pop$Exp4
    , Exp6 = pop$Exp5
    , Exp7 = pop$Exp6
    , Exp8 = pop$Exp7
    , Exp9 = pop$Exp8
    , Exp10 = pop$Exp9
    , Inf1 = pop$Exp10
    , Inf2 = pop$Inf1
    , Inf3 = pop$Inf2
    , Inf4 = pop$Inf3
    , Inf5 = pop$Inf4
    , Inf6 = pop$Inf5
    , Rec = pop$Rec + pop$Inf6
  )
  
  return(updatePop)
}


##################################################################################
#' VACCINATE() is a function that moves some susceptible individuals to the immune 
#' class, based on a daily vaccination rate expected of teams.
##################################################################################

#'params:
#'1. tp = team performance/expected number of vaccinations per day
#'2. v = vaccine efficacy between 0 and 1
#'
vaccinate <- function(pop, tp, v, browse = FALSE) {
  if (browse) browser()
  
  totalPop <- sum(pop)
  
  #calculate the vaccination "rate" from the team performance
  vax_rate <-  ifelse(pop$Sus1 > tp, tp/totalPop, pop$Sus1/totalPop)
  # vax_rate <-  ifelse(pop$Sus1 > tp, tp, pop$Sus1)
  #calculate the proportions of individuals who'll be immunised and those who'll fail immunisation
  newly_immunised_batch <- v * vax_rate * pop$Sus1
  failed_immunisation_batch <- (1 - v) * vax_rate * pop$Sus1
  
  updatePop <- data.frame(
    Sus1 = pop$Sus1 - newly_immunised_batch - failed_immunisation_batch 
    , Sus2 = failed_immunisation_batch
    , Exp1 = pop$Exp1
    , Exp2 = pop$Exp2
    , Exp3 = pop$Exp3
    , Exp4 = pop$Exp4
    , Exp5 = pop$Exp5
    , Exp6 = pop$Exp6
    , Exp7 = pop$Exp7
    , Exp8 = pop$Exp8
    , Exp9 = pop$Exp9
    , Exp10 = pop$Exp10
    , Inf1 = pop$Inf1
    , Inf2 = pop$Inf2
    , Inf3 = pop$Inf3
    , Inf4 = pop$Inf4
    , Inf5 = pop$Inf5
    , Inf6 = pop$Inf6
    , Rec = pop$Rec + newly_immunised_batch
  )
  
  return(updatePop)
}


##################################################################################
#' RUNSIMULATIONS() is a wrapper for the step function. It feeds STEP()  with 
#' initial values and outputs a list of dataframes containing the epidemic progression 
#' (and "collapsed" classes) for each simulation of the scenario.
##################################################################################

runSimulations <- function(R0 # transmission coeficient
                           , run_time 
                           , pop 
                           , vaxDay = NA
                           , orv_duration
                           , strategy
                           , vax_eff
                           , team_performance
                           , time_to_immunity
                           , browse = FALSE
) {
  
  time <- 0
  simResults <- data.frame(time = 0, pop) #at time 0 we have the initial population specified
  if (browse) browser()
  time <- time + 1
  #while (simResults[time, 'Sus1'] > 0) {
  while (time < run_time) {
    # for (time in 1: run_time) {
    if (!is.na(vaxDay) & (time < vaxDay  | time > vaxDay + orv_duration )) {
      simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], R0 = R0)))
    }else if (!is.na(vaxDay) & (time >= vaxDay  & time <= vaxDay + orv_duration)){
      simResults <- rbind(simResults, data.frame(time, step(pop = vaccinate(simResults[time, -1], v = vax_eff, tp = team_performance), R0 = R0)))
    }else if (is.na(vaxDay)){
      simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], R0 = R0)))
    }
    epiDur <- time
    time <- time + 1
  }
  
  # if(time < run_time){
  #   for (i in time:run_time){
  #     simResults <- rbind(simResults, data.frame(time = i, simResults[i - 1, -1]))
  #   }
  # }
  
  susTS <- rowSums(subset(simResults, select = grep("Sus", names(simResults))))
  exposedTS <- rowSums(subset(simResults, select = grep("Exp", names(simResults))))
  infectiousTS <- rowSums(subset(simResults, select = grep("Inf", names(simResults))))
  
  epi_out <- list(
    Detailed = data.frame(simResults, strategy = strategy)
    , Collapsed = data.frame(
      time = simResults$time
      , totalSus = susTS
      , totalExp = exposedTS
      , totalInf = infectiousTS
      , totalRec = simResults$Rec
      , strategy = strategy
    )
    , epiTotal = sum(simResults$Inf5)
    , strategy = strategy
  )
  return(epi_out)
}


# RUNSCENARIO() runs a specified number of simulations for a given scenario.
# runScenario <- function(scenarioParams
#                         , numSims = NUMSIMS
#                         ## PARAMETERS (adjustable)
#                         , schoolSize = 1200 # Number of students in the school
#                         , vaccinationDay = as.numeric(vaxDate - startDate) # Days between when the first case developed a rash to when students are vaccinated in response to the outbreak
#                         , orvCoverage =  0.93 # Proportion of students vaccinated in response to the outbreak
#                         , secondGen = 5 # Number of students infected by the first case
#                         , browse = F) {
#   with(as.list(scenarioParams), {
#     if (browse) browser()
#     effReproNumber <- secondGen * scaleSecondGen
#     if (vaccinate) {
#       outputs <- runSimulations(numSims, beta = betaCalc(R0, schoolSize), pop = initializePop(schoolSize, initPropImmune = 1 - spCalc(effReproNumber, R0), G2 = secondGen), vaxDay = vaccinationDay, coverage = orvCoverage)
#     } else {
#       outputs <- runSimulations(numSims, beta = betaCalc(R0, schoolSize), pop = initializePop(schoolSize, initPropImmune = 1 - spCalc(effReproNumber, R0), G2 = secondGen))
#     }
#     modelParams <- list(schoolSize = schoolSize, vaccinationDay = vaccinationDay, orvCoverage = orvCoverage, secondGen = secondGen)
#     return(
#       list(
#         parameters = c(scenarioParams, modelParams)
#         , results = sapply(1:numSims, epiTraj, output = outputs)
#         , epiDur = sapply(1:numSims, epiDuration, output = outputs)
#         , epiTot = sapply(1:numSims, epiTotal, output = outputs)
#       )
#     )
#   })
# }
# 
# ## - Plotting functions - ##
# 
# # EPICURVE() plots the epidemic curve, given a timeseries of cases
# epiCurve <- function(cases, add=T, day0=startDate, useMar=c(3, 4, 1, 1), ...) {
#   if (!add) {
#     par(mfrow = c(1, 1), mar = useMar)
#   }
#   barplot(cases, space = 0, xpd = FALSE, ...)
#   # axis(1,as.Date(paste(format(day0,'%Y'),1:6,'01',sep='-'))-day0,NA)
#   # axis(1,as.Date(paste(format(day0,'%Y'),1:6,'15',sep='-'))-day0,c(month.abb[1:5],''), F)
#   return()
# }

# EPIHIST() is a function that plots histograms of the output from many simulation runs

epiHist <- function(dat, xlabel=names(dat), xseq=range(dat), obs=NA,
                    xscale=1, ymax=0.3, yval=0.32, add=T, incSummary=T,
                    lightCol="#bcbddc", darkCol="#54278f", addAxes=T, ...) {
  ybar <- yval * length(dat)
  if (!add) {
    par(mfrow = c(1, 1), mar = c(4.5, 5, 4, 1))
  }
  hist(dat, xlim = range(xseq), ylim = c(0, ymax * length(dat)), xaxt = "n", yaxt = "n", yaxs = "i", ann = F, col = lightCol, ...)
  if (addAxes) {
    axis(1, xseq, xseq * xscale)
    mtext(xlabel, 1, line = 3, cex = 1.2)
    axis(2, seq(0, ymax, .1) * length(dat), seq(0, ymax, .1))
    mtext("Proportion of simulations", 2, line = 3, cex = 1.2)
  }
  if (incSummary) {
    segments(quantile(dat, .95), ybar, quantile(dat, .05), ybar, xpd = T, lwd = 1, col = "steelblue1")
    segments(quantile(dat, .75), ybar, quantile(dat, .25), ybar, xpd = T, lwd = 3, col = "palegreen3")
    points(median(dat), ybar, pch = 21, cex = 1.2, col = darkCol, lwd = 2, xpd = T)
    legend("topright", legend = c("median", "5% quartile", "25% quartile"), bty = "n", fill = c(darkCol, "steelblue1", "palegreen3"), cex = 0.85)
  }
  if (!is.na(obs)) {
    segments(obs, ybar * 1.2, obs, ybar * 1.08, lwd = 3, col = "steelblue1", xpd = T)
    points(obs, ybar * 1.08, pch = 25, cex = 1.2, col = darkCol, lwd = 2, xpd = T, bg = "steelblue1")
    text(obs, ybar * 1.27, "observed", xpd = T, cex = .8, font = 2, col = "steelblue1")
    points(obs, 0, pch = 22, xpd = T, cex = 1.2, bg = darkCol)
    legend("topright", legend = c("median", "5% quartile", "25% quartile"), bty = "n", fill = c(darkCol, "steelblue1", "palegreen3"), cex = 0.85)
  }
}

## - Helper functions - ##

# BETACALC() is a function that calculates the transmission coefficient from input parameters.
betaCalc <- function(useR0, N, infPer = 6) {
  -N * log(1 - (useR0 / infPer) / N)
}

# SPCALC() is a function that calculates the susceptible proportion from input parameters
spCalc <- function(useRe, useR0) {
  useRe / useR0
}


# EPIDURATION() is a function that extracts the epidemic duration from the simulation output
epiDuration <- function(ii, output) {
  output[[ii]]$epiDuration
}

# EPITOTAL() is a function that extracts the total number of cases before the end of the outbreak from the simulation output
epiTotal <- function(ii, output) {
  output[[ii]]$epiTotal
}

# EPITRAJ() is a function that extracts the epidemic curve from the simulation output
epiTraj <- function(ii, output) {
  output[[ii]]$Detailed$Inf5
}