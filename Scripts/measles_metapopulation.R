# 
#' SIR_step
#'
#' @param S the susceptible class
#' @param I the infected class
#' @param births number of births per time period
#' @param migration positive integer number of migrations 
#' @param mortality_rate the number of natural deaths
#' @param run_type the dynamic way to run the function
#' @param browse if true, the browser will be opened for debugging purposes
#' @param beta 
#'
#' @return a list with the current state of the system
#' @export 
#'
#' @examples SIR_step(S = 1000, I = 1, beta = 0.012, births = 20, migration = 20, 
#' mortality_rate = 0.2,run_type = 'det')
#' 
SIR_step <- function(S, I, beta, births, migration, mortality_rate, run_type = "det", browse = F) {
  
  if (browse) browser()
  
  switch(run_type,
    "det" = {
      It <- min(S, beta * S * I^.95) + migration
    },
    "stoc" = {
      It <- rbinom(1, S, 1 - exp(-beta * I^.95)) + rpois(1, migration)
    }
  )
  St <- S + births - It - mortality_rate * S
  return(list(S = St, I = It))
}


#' Title
#'
#' @param S0 
#' @param I0 
#' @param beta 
#' @param years 
#' @param births 
#' @param migration 
#' @param mortality_rate 
#'
#' @return
#' @export
#'
#' @examples SIR_run(S0 = 1000, I0 = 1, beta = 0.012, years = 1, births = 20, migration = 20, mortality_rate = 0.1)
SIR_run <- function(S0, I0, beta, years, births, migration, mortality_rate) {
  
  # beta is specified as a vector of values for each "biweek",
  beta <- rep(beta, years)
  Susc <- S0
  Infect <- I0
  timesteps <- 26 * years
  for (time in 1:timesteps) {
    out <- SIR_step(Susc[time], Infect[time], beta[time], births, migration, mortality_rate)
    Susc <- c(Susc, out$S)
    Infect <- c(Infect, out$I)
    # cat(out$I,".\n")
    if (!is.finite(out$I)) {
      break
    }
    # if(out$I==0){break}
  }
  # browser()
  return(list(S = Susc, I = Infect))
}

# beta<-source("/Users/mattferrari/Documents/Current Projects/MSF Measles/Measles_Dynamics_ms/Seasonality/MCMC(new)/niamey/niamey_beta.q")$value


SIRstep <- function(S, I, beta, births, migration, mortalityrate) {
  # browser()
  It <- rbinom(1, S, 1 - exp(-beta * I^.95)) + rpois(1, migration) + rpois(1, 1) # migration + extra random introduction external to metapop
  # It<-min(S,S*(1-exp(-beta*I^.95)))+migration
  St <- S - It - floor(mortalityrate * S) + floor(births)
  return(list(S = St, I = It))
}

SIRmeta <- function(S.init, I.init, beta, coupling, scaling, timesteps, births, mortalityrate) {
  Npatches <- length(S.init)
  Susc <- matrix(S.init, nrow = 1)
  Infect <- matrix(I.init, nrow = 1)
  beta.max <- max(beta)
  beta <- beta / beta.max
  beta <- beta^matrix(scaling, nrow = dim(beta)[1], ncol = Npatches, byrow = T)
  beta <- beta.max * beta
  for (i in 2:timesteps) {
    # browser()
    Susc.tmp <- numeric(0)
    Infect.tmp <- numeric(0)
    for (j in 1:Npatches) {
      tmp <- ceiling(24 + 24 * (i / 24 - ceiling(i / 24)))
      beta.i <- runif(1, beta[tmp, j] * .75, beta[tmp, j] * 1.25)
      # browser()
      I.ext <- sum(Infect[i - 1, -j])
      migr <- ifelse(I.ext > 0, min(10, rpois(1, coupling[j, -j] * I.ext)), 0)
      migr <- ifelse(runif(1) > .9,  migr,  0)
      # browser()
      out.tmp <- SIRstep(Susc[i - 1, j], Infect[i - 1, j], beta.i, births[j], mortalityrate[j], migration = migr)
      Susc.tmp <- c(Susc.tmp, out.tmp$S)
      Infect.tmp <- c(Infect.tmp, out.tmp$I)
      cat(i, "-", j, ".\n")
    }
    Susc <- rbind(Susc, Susc.tmp)
    Infect <- rbind(Infect, Infect.tmp)
    # browser()
    if (sum(Infect.tmp) == 0) {
      break
    }
  }
  return(list(S = Susc, I = Infect))
}


n.patches <- 10

N.init <- 1e7 + rpois(n.patches, 100)
ppn_atrisk <- .4
ppn_vacc <- .95
S.init <- floor(N.init * ppn_atrisk * (1 - ppn_vacc))
############################################
# Birth and death rate based on CIA factbook values
#
b_rate <- rep(0.3 * 0.88 * (50.73 / 24) * (N.init / 1000), n.patches) # vaccinated births
m_rate <- rep((20.91 / 24) / 1000, n.patches)

############################################
# beta <- rep(1e-5,24)
beta <- rep(c(.5e-5, 1e-5, 1.5e-5, 1.5e-5, 1e-5, .5e-5), each = 4) * .007 # seasonal transmission rate
beta.sim <- matrix(smooth.spline(beta)$y, nrow = 24, ncol = n.patches, byrow = F) # make transmission rate smooth, apply to all locations

I.init <- 100 + rpois(n.patches, 3)
years <- 200
Tmax <- 24 * years
coupling <- matrix(0.01, n.patches, n.patches)

out <- SIRmeta(S.init, I.init, beta = beta.sim, coupling = coupling, scaling = 1, timesteps = 24 * years, births = b_rate, mortalityrate = m_rate)
# plot(out$I[4201:4800],type="l")
matplot(out$I[4201:4800, ], type = "l")
