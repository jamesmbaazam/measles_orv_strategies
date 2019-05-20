require(shiny)
require(deSolve)


#new inputs for the model will be:
#1. vaccination rate for mobile and fixed teams (as input to SC model)
#2. campaign duration (as output from SC model)
#3. vaccine efficacy (already an input)
#4. delay before start (as output from SC model)


#new calculations
#1. coverage as a result of delay and campaign duration

#outputs
#1. coverage
#2. final size


######################################################
# SEIR model
######################################################
################
# Parameters
# B = transmission rate
# 1/r = latent period
# 1/g = infectious period
# vax_eff = vaccine efficacy
#
# coverage = target coverage
# orv_dur = length of vaccination campaign
# campaign_day = Day of campaign start
# USAGE:
# times<-1:100
# xstrt<-c(S=.999,E=0,I=.001,R=0,K=0)
# par<-c(B=.5, r=1/7, g = 1/7, vax_eff = .8, coverage = 0, orv_dur = 10, campaign_day = 80)
# out<-as.data.frame(lsoda(xstrt,times,simod,par))
# plot(out$time,out$I,type="l")
#
#
# par<-c(B=.5, r=1/7, g = 1/7, vax_eff = .8, coverage = .99, orv_dur = 10, campaign_day = 50)
# out<-as.data.frame(lsoda(xstrt,times,simod,par))
# lines(out$time,out$I,col="red")

simod <- function(t, x, parms) {
  S <- x['S']
  E <- x['E']
  I <- x['I']
  R <- x['R']
  K <- x['K'] #cumulative incidence
  #
  with(as.list(parms), {
    #Q <- ifelse(t < campaign_day | t > campaign_day + orv_dur, 0, (-log(1 - coverage) / orv_dur)) #-log(1 - coverage) / orv_dur what is this formula?
   # Q <- ifelse(t < campaign_day | t > campaign_day + orv_dur, 0, vax_rate/S)
    Q <- ifelse(t < campaign_delay + immunity_delay | t > campaign_delay + orv_dur + immunity_delay, 0, vax_rate/S)
    dS <- -B * S * I - vax_eff * Q * S
    dE <- B * S * I - r * E
    dI <- r * E - g * I
    dR <- g * I + vax_eff * Q * S
    dK <- r * E
    res <- c(dS, dE, dI, dR, dK)
    list(res)
  })
}

################################################################################
#' INITPOP() is a function that initializes the site population
################################################################################
#'params:
#'N=total population, initPropImmune=initial proportion of N that is immune or suspected to be
#'LP=laten period will determine the number of sub-divisions of the Exposed class. Same applies for
#'IP, Inf0=how many are initially infected, Sus_div = how many sub-divisions in the S class
initPop <- function(N, initPropImmune, Sus_div, LP, IP, Inf0) { 
  immune <- floor(N * initPropImmune)
  E_init <- matrix(0, nrow = LP, ncol = 1, dimnames = list(c(1:LP)))
  I_init <- matrix(0, nrow = IP, ncol = 1, dimnames = list(c(1:IP)))
  S_init <- matrix(0, nrow = Sus_div, dimnames = list(1:Sus_div))
  
  S_init[1,1] <- N - immune
  I_init[1,1] <- Inf0 
  pop0 <- data.frame(
    S = t(S_init),
    E = t(E_init),
    I = t(I_init),
    Rec = immune
  )
  return(pop0)
}

last <- function(x) { return( x[length(x)] ) }
################################################################################
# STEP() is a function that updates the various classes (S,E1...E10,I1...I10,R) and
# returns a dataframe containing the epidemic progression.
###############################################################################
#params:
#tp = team performance or vaccination rate
#v=vaccine efficacy

step <- function(pop, beta, browse = FALSE, runType = "deterministic", v, tp) {
  if (browse) browser()
  totalPop <- sum(pop)
  totalInf <- sum(pop[, grep("I.", names(pop))])
#  prob.t <- 1 - exp(-beta * totalInf / totalPop)
  switch(runType,
         "deterministic" = {
          # newE <- pop$S1 * prob.t
           newly_exposed <- beta * pop[ , grep('S.1', names(pop))] * totalInf 
         },
         "stochastic" = {
           newly_exposed <- ifelse(pop[ , grep('S.1', names(pop))] > 0, rbinom(1, pop[ , grep('S.1', names(pop))], prob.t), 0)
         }, {
           stop("Unknown runType.")
         }
  )
  
  E_indices <- grep('E.', names(pop))
  E_updated <- matrix(NA, nrow = length(E_indices), ncol = 1, dimnames = list(1:length(E_indices)))
  E_updated[1 , 1] <- newly_exposed
  
  for (i in 2:length(E_indices)) {
    E_updated[i, 1] <- pop[1, E_indices[i]]
  }
  
  
  I_indices <- grep('I.', names(pop))
  I_updated <- matrix(NA, nrow = length(I_indices), ncol = 1, dimnames = list(1:length(I_indices)))
  I_updated[1 , 1] <- E_updated[length(E_updated), 1]
  
  for (i in 2:length(I_indices)) {
    I_updated[i, 1] <- pop[1, I_indices[i]]
  }
  
  updatePop <- data.frame(
    S.1 = pop[ , grep('S.1', names(pop))] - newly_exposed - v * tp * pop[ , grep('S.1', names(pop))]
    , S.2 = (1 - v) * tp * pop[ , grep('S.1', names(pop))]
    , E = t(E_updated)
    , I = t(I_updated)
    , Rec = pop$Rec + pop[ , grep('I.', names(pop))][, length(I_indices)] + v * tp * pop[ , grep('S.1', names(pop))]
  )
  
  return(updatePop)
}

###############################################################################
#'vaccinate(): model for running vaccination intervention
###############################################################################

vaccinate <- function(pop, vaxProp, browse=FALSE, runType = "stochastic") {
  if (browse) browser()
  totalPop <- sum(pop)
  switch(runType,
         "deterministic" = {
           newR <- round(pop$Sus * vaxProp)
         },
         "stochastic" = {
           newR <- ifelse(pop$Sus > 0, rbinom(1, pop$Sus, vaxProp), 0)
         }, {
           stop("Unknown runType.")
         }
  )
  updatePop <- data.frame(
    Sus = pop$Sus - newR
    , E.1 = pop$E.1
    , E.2 = pop$E.2
    , E.3 = pop$E.3
    , E.4 = pop$E.4
    , E.5 = pop$E.5
    , E.6 = pop$E.6
    , E.7 = pop$E.7
    , E.8 = pop$E.8
    , E.9 = pop$E.9
    , E.10 = pop$E.10
    , I.1 = pop$I.1
    , I.2 = pop$I.2
    , I.3 = pop$I.3
    , I.4 = pop$I.4
    , I.5 = pop$I.5
    , I.6 = pop$I.6
    , Rec = pop$Rec + newR
  )
  
  return(updatePop)
}

#############################################################################
# RUNSIMULATIONS() is a wrapper for the step function. It feeds STEP()  with initial values and outputs
# a list of dataframes containing the epidemic progression (and "collapsed" classes) for each
# simulation of the scenario.
#############################################################################

runSimulations <- function(sims = NUMSIMS, beta, # transmission coeficient
                           maxTime = 1825, # five years!
                           pop = scenarioInit,
                           vaxDay = NA,
                           coverage = NA,
                           browse = FALSE) {
  results <- list()
  for (sim in 1:sims) {
    time <- 0
    popSize <- sum(pop)
    infections <- sum(pop[, grep("I.", names(pop))]) + sum(pop[, grep("E.", names(pop))])
    simResults <- data.frame(time, pop)
    # Because the initilization assumes the second generation of cases has already been infected by the start
    # of the simulation, finish the infectious period of the index case with no new infections:
    time <- time + 1
    simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], beta = 0)))
    time <- time + 1
    simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], beta = 0)))
    while (infections > 0 && time <= maxTime) {
      time <- time + 1
      if (!is.na(vaxDay) & time == vaxDay) {
        if (browse) browser()
        simResults <- rbind(simResults, data.frame(time, step(pop = vaccinate(simResults[time, -1], coverage), beta = beta)))
      } else {
        simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], beta = beta)))
      }
      infections <- sum(simResults[time, grep("I.", names(simResults))]) + sum(simResults[time, grep("E.", names(simResults))])
    }
    if (time == maxTime & infections > 0) {
      warning("Epidemic duration exceeds maximum time of simulation.")
      endTime <- NA
    } else {
      endTime <- time # record the number of days the epidemic lasted
    }
    exposedTS <- rowSums(subset(simResults, select = grep("E.", names(simResults))))
    infectiousTS <- rowSums(subset(simResults, select = grep("I.", names(simResults))))
    
    
    results[[sim]] <- list(
      Detailed = simResults
      , Collapsed = data.frame(
        time = simResults$time
        , totalSus = simResults$Sus
        , totalExp = exposedTS
        , totalInf = infectiousTS
        , totalRec = simResults$Rec
      )
      , epiDuration = endTime
      , epiTotal = sum(simResults$I.5)
    )
  }
  return(results)
}










#####################################################
# % Intervention
#####################################################
p_red <- function(R, 
                  vaccine_efficacy
                  , vax_rate
                  , delay
                  , intervention_length
                  , mtime 
                  , LP = 7 #LP = Latent period
                  , IP = 7 #IP = Infectious period
                  , N 
                  , step = 1
                  ) { 
  steps <- (0:mtime)[seq(1, mtime, by = step)]
  p_red <- rep(NA, length(steps))
  xstrt <- c(S = 1 - 1 / N, E = 0, I = 1 / N, R = 0, K = 0) # starting values as proportions. Starts with 1 infected and remaining being susceptible
  beta <- R / IP # transmission rate
  t <- 1
  for(i in 1:length(steps)){
    par <- c(B = beta 
      , r = 1 / LP
      , g = 1 / IP
      , vax_eff = vaccine_efficacy
      , orv_dur = intervention_length #SC output
      , campaign_delay = delay
      , vax_rate = vax_rate
    )
    out <- as.data.frame(lsoda(xstrt, steps, simod, par))
   # campaign_day = steps[i]
    p_red[t] <- out$K[dim(out)[1]]
    t <- t + 1
    cat("step ", i, "of ", floor(mtime / step), ".\r")
  }
  # par <- c(B = beta 
  #          , r = 1 / LP
  #          , g = 1 / IP
  #          , vax_eff = vaccine_efficacy
  #         # , coverage = 0
  #          , orv_dur = 0
  #          , campaign_day = Inf
  # )
  # outv <- as.data.frame(lsoda(xstrt, steps, simod, par))
  # # fs should really be the prediction with steps=Inf?
  fs <- max(out$K)
  res <- list(
    out = cbind(steps, p_red / max(p_red))
    , R = R
  #  , vaccine_efficacy = vaccine_efficacy
  #  , target_vaccination = target_vaccination
    , intervention_length = intervention_length
  #  , mtime = mtime
   # , LP = LP
   # , IP = IP
  #  , N = N
  #  , step = step
   # , virgin = outv$I #infected individuals from the no-intervention counterfactual
 #   , vfs = fs
 # , campaign_time = t
 # , model_step = i
  )
  class(res) <- "p_red"
  return(res)
}


######################################################

#####################################################
# plotting pred objects
#####################################################
plot.p_red <- function(object) {
  plot(object$out[, 1], object$out[, 2], type = "l", xlab = "First intervention day", ylab = "% final epidemic", ylim = c(0, 1))
  title(paste("target= ", round(100 * object$target_vaccination, 0), "% campaign = ", object$intervention_length, "d"))
  par(new = TRUE)
  plot(object$out[, 1], object$virgin, col = "red", axes = FALSE, xlab = "", ylab = "", type = "l")
  legend(x = "topleft", legend = c("natural epidemic", "%final size"), col = c("red", "black"), lty = c(1, 1))
}

# EX
# out<-p_red(R=4,vaccine_efficacy=.9,target_vaccination=.5,intervention_length=14, step=2)
# plot(out)

#####################################################
# % Sensitivity analysis on R
#####################################################
R_compare <- function(R = c(2, 4, 8), vaccine_efficacy = .9, target_vaccination, intervention_length, mtime = 120, LP = 7, IP = 7, N = 10000, step = 7) {
  out <- numeric(0)
  for (j in 1:length(R)) {
    tmp <- p_red(
      R = R[j], vaccine_efficacy = vaccine_efficacy,
      target_vaccination, intervention_length, mtime = mtime, LP = LP, IP = IP, N = N, step = step
    )
    out <- cbind(out, tmp$out[, 2])
  }
  res <- list(
    R = R, p_red = out, T = tmp$out[, 1],
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, step = step
  )
  class(res) <- "Rcomp"
  return(res)
}

#####################################################
# plotting pred objects
#####################################################
plot.Rcomp <- function(object) {
  plot(NA, xlim = range(object$T), ylim = c(0, 1), xlab = "First intervention day", ylab = "%, final epidemic")
  title(paste("% final size: target= ", round(100 * object$target_vaccination, 0), "% campaign = ", object$intervention_length, "d"))
  for (j in 1:length(object$R)) {
    lines(object$T, object$p_red[, j], lty = j)
  }
  legend(x = "right", legend = c(object$R), lty = 1:length(object$R), title = "R=")
}

# EX
# res<-R_compare(R=c(1.5, 2.5, 3.5), vaccine_efficacy=.9,target_vaccination=.5,intervention_length=7)
# plot(res)

#####################################################
# % Sensitivity analysis on length of intervention
#####################################################
Int_compare <- function(R, vaccine_efficacy, target_vaccination, intervention_length = c(7, 10, 14), mtime = 120, LP = 7, IP = 7, N = 10000, step = 7) {
  out <- numeric(0)
  for (j in 1:length(intervention_length)) {
    tmp <- p_red(
      R = R, vaccine_efficacy = vaccine_efficacy,
      target_vaccination, intervention_length[j], mtime = mtime, LP = LP, IP = IP, N = N, step = step
    )
    out <- cbind(out, tmp$out[, 2])
  }
  res <- list(
    R = R, p_red = out, T = tmp$out[, 1],
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, step = step
  )
  class(res) <- "Intcomp"
  return(res)
}

#####################################################
# plotting Intcomp objects
#####################################################
plot.Intcomp <- function(object) {
  plot(NA, xlim = range(object$T), ylim = c(0, 1), xlab = "First intervention day", ylab = "% final epidemic")
  title(paste("% final size: target= ", round(100 * object$target_vaccination, 0), "%, R = ", object$R))
  for (j in 1:length(object$intervention_length)) {
    lines(object$T, object$p_red[, j], lty = j)
  }
  legend(x = "right", legend = c(object$intervention_length), lty = 1:length(object$intervention_length), title = "Campaign D:")
}

# EX
# res<-Int_compare(intervention_length=c(7,10,14),R=3, vaccine_efficacy=.9, target_vaccination=.5)
# plot(res)

#####################################################
# % Sensitivity analysis on target Vaccination
#####################################################
Vacc_compare <- function(R, vaccine_efficacy, target_vaccination = c(.50, .70, .90), intervention_length = 7, mtime = 120, LP = 7, IP = 7, N = 10000, step = 7) {
  out <- numeric(0)
  for (j in 1:length(target_vaccination)) {
    tmp <- p_red(
      R = R, vaccine_efficacy = vaccine_efficacy,
      target_vaccination[j], intervention_length, mtime = mtime, LP = LP, IP = IP, N = N, step = step
    )
    out <- cbind(out, tmp$out[, 2])
  }
  res <- list(
    R = R, p_red = out, T = tmp$out[, 1],
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, step = step
  )
  class(res) <- "Vacccomp"
  return(res)
}

#####################################################
# plotting Vaccomp objects
#####################################################
plot.Vacccomp <- function(object) {
  plot(NA, xlim = range(object$T), ylim = c(0, 1), xlab = "First intervention day", ylab = "% final epidemic")
  title(paste("% final size: R = ", object$R))
  for (j in 1:length(object$target_vaccination)) {
    lines(object$T, object$p_red[, j], lty = j)
  }
  legend(x = "right", legend = c(round(100 * object$target_vaccination, 0)), lty = 1:length(object$target_vaccination), title = "Target %")
}

# EX
# res<-Vacc_compare(target_vaccination=c(.50,.70,.90), R=4, vaccine_efficacy=.9,intervention_length=7)
# plot(res)

#####################################################
# Mortality analysis on R
#####################################################
M_red <- function(object, case_fatality) {
  par(mfrow = c(2, 1))
  plot(object)
  out <- (1 - object$p_red) * object$N * case_fatality
  plot(NA, xlim = c(1, max(object$T)), ylim = c(0, max(out)), xlab = "First intervention day", ylab = "Extra survivors")
  for (j in 1:dim(out)[2]) {
    lines(object$T, out[, j], lty = j)
  }
  title(paste("Reduced burden of mortality; N = ", object$N))
  par(mfrow = c(1, 1))
}

# EX
# res<-Vacc_compare(target_vaccination=c(.50,.70,.90), R=4, vaccine_efficacy=.9,intervention_length=7)
# M_red(res, 0.5)


#####################################################
# Retrospective
#####################################################
retro <- function(R, day, vaccine_efficacy, target_vaccination, intervention_length, mtime = 120, LP = 7, IP = 7, N = 10000) {
  steps <- 1:mtime
  out <- matrix(NA, nrow = mtime, ncol = 3)
  xstrt <- c(S = 1 - 1 / N, E = 0, I = 1 / N, R = 0, K = 0) # starting values
  beta <- R / IP # transmission rate
  t <- 1
  par <- c(
    B = beta, r = 1 / LP, g = 1 / IP, vax_eff = vaccine_efficacy,
    P = 0, orv_dur = 0, T = Inf
  )
  outv <- as.data.frame(lsoda(xstrt, steps, simod, par))
  # fsv and fsi should really be with steps=Inf?
  fsv <- max(outv$K)

  par <- c(
    B = beta, r = 1 / LP, g = 1 / IP, vax_eff = vaccine_efficacy,
    P = target_vaccination, orv_dur = intervention_length, T = day
  )

  outi <- as.data.frame(lsoda(xstrt, steps, simod, par))

  fsi <- max(outi$K)
  out[, 1] <- steps
  out[, 2] <- outv$I
  out[, 3] <- outi$I
  res <- list(
    out = out,
    redn = fsi / fsv,
    R = R,
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, day = day
  )
  class(res) <- "retro"
  return(res)
}
######################################################

#####################################################
# plotting pred objects
#####################################################
plot.retro <- function(object) {
  plot(object$out[, 1], object$out[, 2], type = "l", ylim = c(0, max(object$out[, 2])), xlab = "day", ylab = "prevalence")
  polygon(c(
    object$day, object$day, object$day + object$intervention_length,
    object$day + object$intervention_length
  ), c(-0.1, 1, 1, -.1), col = "gray")
  lines(object$out[, 1], object$out[, 2])
  lines(object$out[, 1], object$out[, 3], col = "red")
  title(paste("final size: ", round(100 * (object$redn), 1), "% (R=",
    object$R, ", target=", 100 * object$target_vaccination, "%)",
    sep = ""
  ))
  legend(
    x = "topright", legend = c("natural epidemic", "w intervention"),
    col = c("black", "red"), lty = c(1, 1)
  )
  text(
    x = object$day + object$intervention_length, y = 0, pos = 4,
    labels = paste(
      object$intervention_length,
      "d intervention from", object$day
    )
  )
}


# EX
# out<-retro(R=4, day=50, vaccine_efficacy=.9,target_vaccination=.5,intervention_length=14)
# plot(out)


