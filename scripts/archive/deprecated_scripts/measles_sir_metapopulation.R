


#' # init_pop: a function that initializes the target population
#' @param target_pop_size 
#' @param initPropImmune 
#' @param I0 
#'
#' @return 
#' @export
#'
#' @examples init_pop(10000, 0.3, 1)
init_pop <- function(target_pop_size, initPropImmune, I0) {
  immune <- floor(target_pop_size * initPropImmune)
  data.frame(
    S = target_pop_size - immune,
    I = I0,
    R = immune
  )
}

#' infect: a function that infects a proportion of the susceptible population
#'
#' @param births number of births per time period
#' @param migration positive integer number of migrations 
#' @param mortality_rate the number of natural deaths
#' @param run_type the dynamic way to run the function
#' @param browse if true, the browser will be opened for debugging purposes
#' @param beta 
#' @param target_pop 
#' @param rec_duration 
#'
#' @return a list with the current state of the system
#' @export 
#'
#' @examples infect(target_pop = init_pop(1000, 0.15, 1), beta = 0.012, births = 20, migration = 20, rec_duration = 7,mortality_rate = 0.2,run_type = 'det')
#' 
infect <- function(target_pop, 
                   beta, 
                   births, 
                   migration,
                   rec_duration,
                   mortality_rate, 
                   run_type = "det", 
                   browse = F
                   ) {
  
  if (browse) browser()
  
  pop_size <- sum(target_pop)
  
  switch(run_type,
    "det" = {
      It <- min(target_pop$S, beta * target_pop$S * target_pop$I) + migration
    },
    "stoc" = {
      infection_prob <- 1 - exp(-beta * target_pop$I)
      It <- rbinom(1, target_pop$S, infection_prob) + rpois(1, migration)
    }
  )
  pop_forward_step <- data.frame(
    S = target_pop$S + births - It - mortality_rate * target_pop$S,
    I = target_pop$I +  (1/rec_duration)*It,
    R = target_pop$R + (1/rec_duration)*It
  ) 
  return(pop_forward_step)
}


#' Title
#'
#' @param births 
#' @param migration 
#' @param mortality_rate 
#' @param vax_eff 
#' @param vax_rate 
#' @param n_team_type 
#' @param run_type 
#' @param browse 
#' @param target_pop 
#'
#' @return
#' @export
#'
#' @examples vaccinate(target_pop = init_pop(1000, 0.15, 1), births = 20, migration = 20, mortality_rate = 0.2, vax_eff = 0.95, vax_rate = 250, n_team_type = 1)
vaccinate <- function(target_pop, 
                      births, 
                      migration, 
                      mortality_rate, 
                      vax_eff, 
                      vax_rate,
                      n_team_type, 
                      run_type = "det", 
                      browse = F
                      ) {
  
  if (browse) browser()
  
  switch(run_type,
         "det" = {
           Rt <- min(target_pop$S, vax_rate*n_team_type*vax_eff) + migration
         },
         "stoc" = {
           Rt <- rbinom(1, vax_rate*n_team_type, vax_eff) + rpois(1, migration)
         }
  )
  pop_forward_step <- data.frame(
  S = target_pop$S + births - Rt - mortality_rate * target_pop$S,
  I = target_pop$I,
  R = target_pop$R + Rt
  )
  return(pop_forward_step)
}



#' Title
#'
#' @param beta 
#' @param births 
#' @param migration 
#' @param mortality_rate 
#' @param target_pop 
#' @param run_time 
#' @param rec_dur 
#' @param vax_day 
#' @param vax_efficacy 
#' @param orv_duration 
#' @param n_team_type 
#' @param team_performance 
#' @param infect_run_type 
#' @param vax_run_type 
#'
#' @return
#' @export
#'
#' @examples run_simulation(target_pop = init_pop(1000, 0.15, 1), beta = 0.012, run_time = 365, births = 20, migration = 20, mortality_rate = 0.1)
run_simulation <- function(target_pop,
                           beta,
                           run_time,
                           births,
                           migration,
                           mortality_rate,
                           rec_dur,
                           vax_day,
                           vax_efficacy,
                           orv_duration,
                           n_team_type,
                           team_performance,
                           infect_run_type = "det",
                           vax_run_type = "det",
                           browse = F) {


  #beta <- rep(beta, run_time)
  Susc <- target_pop$S
  Infected <- target_pop$I
  Rec <- target_pop$R

  # running parameters
  # timesteps <- 26 * run_time #26 biweeks in a year

  # day before campaign
  
  if(browse) browser()
  simResults <- data.frame(time = 0, S = Susc, I = Infected, R = Rec)

  for (time in 1:run_time) {
    if (!is.na(vax_day) & time < vax_day + 1 | time > vax_day + 1 + round(orv_duration / n_team_type)) {
      infected_pop <- infect(target_pop = simResults[time, -1], beta = beta, births = births, migration = migration, rec_duration = rec_dur, mortality_rate = mortality_rate, run_type = infect_run_type)
      simResults <- rbind(simResults, data.frame(time = time, infected_pop))
    } else if (!is.na(vaxDay) & time >= vaxDay + 1 & time <= vaxDay + 1 + round(orv_duration / n_team_type)) {
      vaccinated_pop <- vaccinate(target_pop = simResults[time, -1], births = births, migration = migration, mortality_rate = mortality_rate, vax_eff = vax_efficacy, vax_rate = team_performance, n_team_type = n_team_type, run_type = vax_run_type)
      simResults <- rbind(simResults, data.frame(time = time, vaccinated_pop))
    } else if (is.na(vaxDay)) {
      infected_pop <- infect(target_pop = simResults[time, -1], beta = beta, births = births, migration = migration, rec_duration = rec_dur, mortality_rate = mortality_rate, run_type = infect_run_type)
      simResults <- rbind(simResults, data.frame(time = time, infected_pop))
    }
  }
  return(simResults)
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
      ifelse(I.ext > 0, migr <- min(10, rpois(1, coupling[j, -j] * I.ext)), migr <- 0)
      ifelse(runif(1) > .9, migr <- migr, migr <- 0)
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