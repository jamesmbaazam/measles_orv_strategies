#global controls
options(nwarnings = 10000) #print this many messages if they exist or occur

#helper functions and parameters
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/deterministic_framework_analysis/scenarios.R') 
source('./scripts/deterministic_framework_analysis/sensitivity_analyses/pop_size/simulation_params_pop_size_sensitivity.R')
source('./scripts/measles_deterministic_seir_model.R')

## Supply chain data ----
pop_size_sensitivity_sc_results <- readRDS('./model_output/deterministic_framework_analysis_output/sensitivity_analysis/pop_sizes/sc_analysis_pop_size_sensitivity.rds')


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
