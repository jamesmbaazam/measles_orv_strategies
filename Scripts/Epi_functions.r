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
  K <- x['K'] #class for tracking new infections
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
######################################################

#####################################################
# % Intervention
#####################################################
p_red <- function(R, 
                  vaccine_efficacy
                  , vax_rate
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
      , campaign_day = steps[i]
    )
    out <- as.data.frame(lsoda(xstrt, steps, simod, par))
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
    , vaccine_efficacy = vaccine_efficacy
  #  , target_vaccination = target_vaccination
    , intervention_length = intervention_length
    , mtime = mtime
    , LP = LP
    , IP = IP
    , N = N
    , step = step
   # , virgin = outv$I #infected individuals from the no-intervention counterfactual
    , vfs = fs
  , campaign_time = t
  , model_step = i
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


