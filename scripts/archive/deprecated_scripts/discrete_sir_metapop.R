source('./scripts/measles_functions.R')


n_patches <- 2

patch_pop_sizes <- rep(100000, times = n_patches)
patch_I0 <- rep(1, times = n_patches)
init.prop.immune <- rep(0.3, times = n_patches)
patch_R0 <- rep(12, times = n_patches)
run_time <- 365
coupling <- matrix(0.01, n_patches, n_patches)
patch_vax_day <- seq(1: n_patches)
patch_campaign_dur <- rep(5, n_patches)

patch_pop_init <- NULL

for (patch in 1:n_patches) {
    patch_pop_init <- rbind(patch_pop_init, 
                            initializePop(patch_pop_sizes[patch],
                                                          init.prop.immune[patch],
                                                          I0 = patch_I0[patch])
                            ) 
}

patch_pop_init

meta_sim <- function(n_patches,
                     R0,
                     run_time, 
                     pop,
                     vaxDay = NA,
                     orv_duration,
                     strategy_name,
                     vax_eff,
                     n_team_type,
                     team_performance,
                     time_to_immunity,
                     browse = FALSE
) {
    patch_dynamics <- list()
    time <- 0
    simResults <- data.frame(time = 0, pop) #at time 0 we have the initial population specified
    if (browse) browser()

    for (time in 1:run_time) {
        for (patch in 1: n_patches) {
        if (!is.na(vaxDay) & time < vaxDay + 1  | time > vaxDay + 1 + (orv_duration/n_team_type)) {
            simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], R0 = R0)))
        }else if (!is.na(vaxDay) & time >= vaxDay + 1  & time <= vaxDay + 1 + (orv_duration/n_team_type)){
            simResults <- rbind(simResults, data.frame(time, step(pop = vaccinate(simResults[time, -1], v = vax_eff, n_team_type = n_team_type, tp = team_performance), R0 = R0)))
        }else if (is.na(vaxDay)){
            simResults <- rbind(simResults, data.frame(time, step(pop = simResults[time, -1], R0 = R0)))
        }
        #insert a breaking statement here if there are no prevailing infections in the previous timestep
        epiDur <- time
        }
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