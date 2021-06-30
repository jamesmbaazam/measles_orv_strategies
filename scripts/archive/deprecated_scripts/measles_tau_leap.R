#############################################################################################
# Here is a short script for a stochastic SIR model
# this genrates cases per day and then aggregates into a series of observation intervals; here 13 biweeks, or half a year
#

gilstep <- function(S.t, I.t, I.p, beta, births) {
  # S.t = current susceptibles
  # I.t = current infecteds
  # I.p = infecteds one infectious period previous (note, this is analogous to your fixed infectious period)
  # beta = transmission rate
  # births = births per time step

  nSI <- ifelse(I.t > 0, rpois(1, beta * S.t * I.t), 0) # number of new cases in time step t
  nIR <- rpois(1, I.p) # number of cases recovering in time step t
  I.new <- max(0, I.t + nSI - nIR) # update I (I is populated by new infections and depopulated by recoveries)
  S.new <- max(0, S.t - nSI + births) # update S (S is populated by births and depopulated by new infections)
  return(list(S = S.new, I = I.new, new.case = nSI)) # store S, I, and new cases
}


gilrun <- function(sim_duration, S.init, I.init, beta, inf.per, births = 0) {
  # sim_duration = the total time of the simulation
  # S.init = the initial susceptible population
  # I.init = the initial infectious population
  # beta = the transmission rate
  # inf.per = the infectious period
  # births = the births per time step
  #

  S <- S.init
  I <- I.init
  new.case <- 0
  for (i in 2:sim_duration) {
    # browser()
    I.p <- ifelse(i > (inf.per + 1), new.case[i - inf.per], 0) # If time is earlier than the first infectious period, nobody can recover
    out <- gilstep(S[i - 1], I[i - 1], I.p, beta, births) # generate new infectious and recoveries
    I <- c(I, out$I) # Append new I
    new.case <- c(new.case, out$new.case) # Append new cases
    S <- c(S, out$S) # Append new S
  }
  return(list(I = I, S = S, new.case = new.case))
}
n.obs.int <- 13 # how many observation periods; here 13 2-week periods
sim_duration <- 14 * n.obs.int # total time is 2-weeks times the number of observation periods
p.obs <- 1 # probability of observing a case

out <- gilrun(sim_duration, S.init = 1e4, I.init = 1, beta = 2.5e-4 / 14, inf.per = 14, births = 0)
plot(out$I, col = "red", ylim = c(0, 1e4)) # plot total infections
points(out$S, col = "blue") # plot total susceptibles
points(out$new.case, col = grey(.5)) # plot new cases per day

obs.int <- rep(1:n.obs.int, each = 14) # break up in to observation periods

cases.int <- sapply(split(out$new.case, obs.int), sum) # count cases per observation period
lines(seq(14, sim_duration, by = 14), cases.int) # add lines for new cases per observation period

cases.obs <- rbinom(length(cases.int), cases.int, p.obs) # count reported cases per observation period

plot(out$new.case, type = "l") # just plot new cases per day