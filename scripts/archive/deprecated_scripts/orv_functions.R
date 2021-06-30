
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
                           , strategy_name
                           , vax_eff
                           , team_performance
                          # , time_to_immunity
                           , browse = FALSE
) {
    
    time <- 0
    simResults <- data.frame(time = 0, pop) #at time 0 we have the initial population specified
    if (browse) browser()
    time <- time + 1
    while (time < run_time) {
        if (!is.na(vaxDay) & time < vaxDay + 1  | time > vaxDay + 1 + orv_duration) {
            simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], R0 = R0)))
        }else if (!is.na(vaxDay) & time >= vaxDay + 1  & time <= vaxDay + 1 + orv_duration){
            simResults <- rbind(simResults, data.frame(time, step(pop = vaccinate(simResults[time, -1], v = vax_eff, tp = team_performance), R0 = R0)))
        }else if (is.na(vaxDay)){
            simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], R0 = R0)))
        }
        epiDur <- time
        time <- time + 1
    }
    
    susTS <- rowSums(subset(simResults, select = grep("Sus", names(simResults))))
    exposedTS <- rowSums(subset(simResults, select = grep("Exp", names(simResults))))
    infectiousTS <- rowSums(subset(simResults, select = grep("Inf", names(simResults))))
    
    epi_out <- list(
        Detailed = data.frame(simResults, strategy = strategy_name)
        , Collapsed = data.frame(
            time = simResults$time
            , totalSus = susTS
            , totalExp = exposedTS
            , totalInf = infectiousTS
            , totalRec = simResults$Rec
            , strategy = strategy_name
        )
        , epiTotal = sum(simResults$Inf5)
        , strategy = strategy_name
    )
    return(epi_out)
}




####running a simulation

#what's the pop like before the outbreak
initial_pop <- initializePop(N = 1000, initPropImmune = 0.25, I0 = 1) #initializePop takes as input: N = target population size, initPropImmune = initial proportion of population that is immune, and I0=initial number of infected. InitializePop() returns a dataframe of the initialized population

#
sim_results <- runSimulations(
    R0 = 12 #specify an R0 value 
    , run_time = 365 #how many days of epidemic
    , pop = initial_pop
    , strategy_name = 'strategyX' #name of the strategy as a string; e.g 'dose10_fcc'.
    , vaxDay = 50 #when will the orv campaign start? i.e, what is the delay in whole number days?
    , orv_duration = 10 #fhow long should campaign last in whole number days?
    , vax_eff = 0.95 #vaccine efficacy (value range: 0 to 1)
    , team_performance = 250 #how many people are expected to be vaccinated per day? 
    , browse = F #if True, open Browser for debugging
) 

#Plots
#total susceptibles
plot(sim_results$Collapsed$time, sim_results$Collapsed$totalSus, type = 'b', xlab = 'Time', ylab = 'Susceptibles' )
#total infected
plot(sim_results$Collapsed$time, sim_results$Collapsed$totalInf, type = 'b', xlab = 'Time', ylab = 'Infectives')
plot(sim_results$Detailed$time, sim_results$Detailed$Inf5, type = 'b', xlab = 'Time', ylab = 'Infectives')     
