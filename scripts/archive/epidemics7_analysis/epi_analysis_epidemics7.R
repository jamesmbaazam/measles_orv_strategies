
## Transmission dynamics ####

## Packages ####
library('conflicted')
library('dplyr')
library('reshape2')
library('tidyr')
library('purrr')


## Resolve package conflicts ####
conflict_prefer('filter', 'dplyr') #anytime I call the function 'filter', I mean dplyr::filter
conflict_prefer('select', 'dplyr')
conflict_prefer('merge', 'base')

#helper functions and parameters
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/epidemics7_analysis/scenarios.R') 
source('./scripts/epidemics7_analysis/simulation_params.R')
source('./scripts/measles_functions.R')


## Supply chain data ----
sc_results <- readRDS('./model_output/sc_analysis_full_10_teams.rds')

#' create a new column with the compounded delays

sc_results_full <- sc_results%>%
    group_split(strategy, mt_equip_type) %>%
    map_df(function(dat) {
        mutate(dat,
               mt_compounded_delay = calc_compounded_delays(
                   campaign_start,
                   mt_dur_constrained
               ),
               ft_compounded_delay = calc_compounded_delays(
                   campaign_start,
                   ft_dur_constrained
               )
        )
    })

View(sc_results_full)
###############################################################################
#Running the simulations for each strategy
###############################################################################

#far/remote location

orv_far_pop_dynamics <- vector('list', nrow(sc_results))

#strategy_name <- vector('list', nrow(sc_results))

for (sc_result_row in 1: nrow(sc_results_full)) {
    
    
    tp <- as.numeric(sc_model_params$vax_rate['mobile_team']) #if all mobile teams can exceed the doses needed to achieve the average daily performance, then it's safe to assume all teams can hit the average team performance
    orv_far_pop_dynamics[[sc_result_row]] <- runSimulations(
        R0 = orv_model_params$far_pop_R0 # transmission coeficient
        , run_time = orv_model_params$model_time # 1 yr!
        , pop = initializePop(N = as.numeric(sc_results_full[sc_result_row, 'far_pop']), initPropImmune = 0.25, I0 = 1)
        , strategy_name = sc_results_full[sc_result_row, 'strategy']
        , vaxDay = as.numeric(sc_results_full[sc_result_row, 'campaign_start_compounded'])
        , orv_duration = as.numeric(sc_results_full[sc_result_row ,'mt_team_days']) #for now we're only looking at the far campaigns 
        , n_team_type = 1
        , vax_eff = orv_model_params$vaccine_efficacy
        , team_performance = tp
        , time_to_immunity = orv_model_params$immune_response_timing
        , browse = F
    ) 
    
}

far_orv_dynamics_unlist <- unlist(orv_far_pop_dynamics, recursive = F)



sc_results %>% 
    group_split(strategy, mt_equip_type) %>% 
    rowwise() %>% 
    do({with(., 
             runSimulations(
                 R0 = orv_model_params$far_pop_R0 # transmission coeficient
                 , run_time = orv_model_params$model_time # 1 yr!
                 , pop = initializePop(N = far_pop, initPropImmune = 0.25, I0 = 1)
                 , strategy_name = strategy
                 , vaxDay = campaign_start_compounded
                 , orv_duration = mt_team_days
                 , n_team_type = 1
                 , vax_eff = orv_model_params$vaccine_efficacy
                 , team_performance = as.numeric(sc_model_params$vax_rate['mobile_team'])
                 , time_to_immunity = orv_model_params$immune_response_timing
                 , browse = F
             )  
             )
        })



