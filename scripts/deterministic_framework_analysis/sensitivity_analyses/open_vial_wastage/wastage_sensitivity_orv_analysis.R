#global controls
options(nwarnings = 10000) #print this many messages if they exist or occur

#helper functions and parameters
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/deterministic_framework_analysis/scenarios.R') 
source('./scripts/deterministic_framework_analysis/simulation_params.R')
source('./scripts/measles_deterministic_seir_model.R')

## Supply chain data ----
wastage_sensitivity_sc_results <- readRDS('./model_output/deterministic_framework_analysis_output/sensitivity_analysis/wastage/sc_analysis_ovw_sensitivity.rds')

#' create a new column with the compounded delays
wastage_sensitivity_sc_results_full <- wastage_sensitivity_sc_results %>%
    group_split(strategy, mt_equip_type, dose10_ovw_ft, dose10_ovw_mt, monodose_ovw_ft, monodose_ovw_mt) %>%
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


#View(wastage_sensitivity_sc_results_full)