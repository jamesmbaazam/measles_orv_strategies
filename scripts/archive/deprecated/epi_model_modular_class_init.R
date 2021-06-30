###Vaccination event
#Q is number of susceptibles being injected in the arm
Q <- if (t < campaign_day | t > campaign_day + orv_dur){0} 
else if(S > team_performance){team_performance} 
else{S} 
#FIX
if (S==0){
<end campaign>
}
#model equations
S1[t+1] = S[t] - B*S[t]*I[t] - Q*vax_eff
S2[t+1] = Q*(1-vax_eff) #class of individuals who have failed immunisation but will not be revaccinated during the course of the ORV
E1[t+1] = B*S[t]*I[t] 



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

