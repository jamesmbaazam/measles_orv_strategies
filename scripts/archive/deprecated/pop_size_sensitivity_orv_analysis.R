#global controls
options(nwarnings = 10000) #print this many messages if they exist or occur

#helper functions and parameters
source('./scripts/deterministic_framework_analysis/global_scripts/parameters.R') #global parameter list
source('./scripts/deterministic_framework_analysis/global_scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/deterministic_framework_analysis/global_scripts/scenarios.R') 
source('./scripts/deterministic_framework_analysis/sensitivity_analyses/pop_size/sim_params_pop_size_prop_teams_sensitivity.R')
source('./scripts/deterministic_framework_analysis/global_scripts/measles_deterministic_seir_model.r')

#packages
library('purrr')
library('dplyr')

## Supply chain data ----
pop_size_sensitivity_sc_results <- readRDS('./model_output/deterministic_framework_analysis_output/sensitivity_analysis/pop_sizes/sc_analysis_pop_size_prop_teams_sensitivity_full.rds')


#' create a new column with the compounded delays
pop_size_sensitivity_sc_results_full <- pop_size_sensitivity_sc_results %>%
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
    }) %>% mutate(strategy = gsub('_parallel', '', .$strategy))
