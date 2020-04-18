#' packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(conflicted)

#' source helper scripts
source('./scripts/analyses_parameters.R')


#' Resolve package conflicts 
conflict_prefer('filter', 'dplyr') #anytime I call the function 'filter', I mean dplyr::filter
conflict_prefer('select', 'dplyr')
conflict_prefer('merge', 'base')
conflict_prefer('summarise', 'dplyr')


#'load data and rescale them to the actual numbers
orv_near_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/orv_near_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
           )

no_orv_near_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/no_orv_near_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
           )

orv_far_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/orv_far_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
           )

no_orv_far_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/no_orv_far_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
           )



#' total outbreak size across all near locations (orv) ----
#' near pop (orv) ====
orv_near_dynamics_outbreak_size_df <- orv_near_dynamics %>% 
    group_by(strategy, mt_equip_type) %>% 
    filter(time == orv_model_params$model_time) %>% 
    summarise(orv_near_total_cases = sum(K))

#' far pop (orv) ====
orv_far_dynamics_outbreak_size_df <- orv_far_dynamics %>% 
    group_by(strategy, mt_equip_type) %>% 
    filter(time == orv_model_params$model_time) %>% 
    summarise(orv_far_total_cases = sum(K))

#' total outbreak size across all near locations (baseline; no orv) ----
#' near pop (no orv) ====
no_orv_near_dynamics_outbreak_size <- no_orv_near_dynamics %>% 
    filter(time == orv_model_params$model_time) %>% 
    summarise(total_cases = sum(K)) %>% 
    as.numeric()

no_orv_near_dynamics_outbreak_size

#' far pop (no orv) ====
no_orv_far_dynamics_outbreak_size <- no_orv_far_dynamics %>% 
    filter(time == orv_model_params$model_time) %>% 
    summarise(total_cases = sum(K)) %>% 
    as.numeric()

no_orv_far_dynamics_outbreak_size


# calculate the total outbreak size across all locations, near and far
orv_outbreak_sizes_aggregated <- left_join(orv_near_dynamics_outbreak_size_df, 
                                           orv_far_dynamics_outbreak_size_df) %>% 
    mutate(orv_total_cases = orv_near_total_cases + orv_far_total_cases)

#' cases averted ====


#' we use the 10-dose fcc with vaccine carrier as the baseline because it is what is 
#' currently the practice
baseline_outbreak_size <- orv_outbreak_sizes_aggregated %>% filter(strategy == 'dose10_fcc', 
                                                                   mt_equip_type == 'vaxCarr') %>% 
    .$orv_total_cases %>% as.numeric()



cases_averted_df <- orv_outbreak_sizes_aggregated %>% 
    mutate(cases_averted = baseline_outbreak_size - orv_total_cases)

cases_averted_df

saveRDS(cases_averted_df, './model_output/deterministic_framework_analysis_output/cases_averted_deterministic_framework_analysis.rds')


#' Combine all simulations into a FINAL data.frame for post-processing

#'load supply chain summary
#'
sc_results_summary_10_teams <- readRDS('./model_output/deterministic_framework_analysis_output/sc_results_summary_10_teams.rds') %>% 
    as_tibble() %>% 
    rename(mt_equip_type = mt_equip) %>% 
    mutate(strategy = as_factor(gsub(pattern = '_parallel', replacement = '', .$strategy))) #shorten the strategy names

sc_epi_analysis_summary_10_teams <- left_join(sc_results_summary_10_teams, 
                                              cases_averted_df, 
                                              by = c('strategy', 'mt_equip_type'
                                              )
)

saveRDS(sc_epi_analysis_summary_10_teams, 
        file = './model_output/deterministic_framework_analysis_output/sc_epi_analysis_summary_10_teams.rds'
)
View(sc_epi_analysis_summary_10_teams)
