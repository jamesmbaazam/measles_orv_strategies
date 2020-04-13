#global controls
options(nwarnings = 10000) #print this many messages if they exist or occur

#packages
library('ggplot2')
library('ggthemes')
library('conflicted')
library('dplyr')
library('reshape2')
library('tidyr')
library('purrr')

## Resolve package conflicts ####
conflict_prefer('filter', 'dplyr') #anytime I call the function 'filter', I mean dplyr::filter
conflict_prefer('select', 'dplyr')
conflict_prefer('merge', 'base')
conflict_prefer('summarise', 'dplyr')

#helper functions and parameters
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/epidemics7_analysis/scenarios.R') 
source('./scripts/epidemics7_analysis/simulation_params.R')
source('./scripts/measles_deterministic_seir_model.R')

## Supply chain data ----
sc_results <- readRDS('./model_output/sc_analysis_full_10_teams.rds')

#' create a new column with the compounded delays
sc_results_full <- sc_results %>%
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

#View(sc_results_full)

#' orv_model_inputs; remove unwanted supply chain results ----
orv_model_inputs <- sc_results_full %>% 
    select(c(strategy, 
             location_id,
             mt_equip_type,
             near_pop,
             far_pop,
             ft_dur_constrained,
             mt_dur_constrained,
             ft_cov,
             mt_cov,
             mt_compounded_delay,
             ft_compounded_delay)
           )




#' near locations' dynamics following orv ----
orv_near_dynamics <- tibble()

for (sc_result_row in 1: nrow(orv_model_inputs)) {
    near_orv_sim <- run_orv_model(
        strategy = orv_model_inputs[[sc_result_row, 'strategy']],
        R0 = orv_model_params$near_pop_R0[21], # transmission coefficient
        max_time = orv_model_params$model_time, 
        target_pop_size = orv_model_inputs[[sc_result_row, 'near_pop']],
        mt_equip_type = orv_model_inputs[[sc_result_row, 'mt_equip_type']],
        vax_day = orv_model_inputs[[sc_result_row, 'ft_compounded_delay']],
        latent_period = orv_model_params$LP,
        infectious_period = orv_model_params$IP,
        scenario_campaign_duration = orv_model_inputs[[sc_result_row ,'ft_dur_constrained']], 
        location_id = orv_model_inputs[[sc_result_row, 'location_id']],
        vax_efficacy = orv_model_params$vaccine_efficacy,
        scenario_coverage = orv_model_inputs[[sc_result_row, 'ft_cov']],
        browse = F
    ) 
    orv_near_dynamics <- rbind(orv_near_dynamics, near_orv_sim)
}

saveRDS(orv_near_dynamics, './model_output/ottar_seir_model_output/orv_near_dynamics.rds')

#Plot of transmission dynamics for near pop
orv_near_dynamics_plot <- ggplot(data = orv_near_dynamics %>% filter(time <= 120)) + 
    geom_line(aes(x = time, 
                  y = S
    ), 
    color = 'blue', 
    size = 1
    ) + 
    geom_line(aes(x = time, 
                  y = I), 
              color = 'red', 
              size = 1) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals', 
         title = 'Transmission dynamics following orv in near locations'
    ) + 
    facet_grid(strategy ~ mt_equip_type + location_id)

plot(orv_near_dynamics_plot)



#' far locations' dynamics following orv ----
orv_far_dynamics <- tibble()
for (sc_result_row in 1: nrow(orv_model_inputs)) {
    far_orv_sim <- run_orv_model(
        strategy = orv_model_inputs[[sc_result_row, 'strategy']],
        R0 = orv_model_params$far_pop_R0[21], # transmission coefficient
        max_time = orv_model_params$model_time, 
        target_pop_size = orv_model_inputs[[sc_result_row, 'far_pop']],
        mt_equip_type = orv_model_inputs[[sc_result_row, 'mt_equip_type']],
        vax_day = orv_model_inputs[[sc_result_row, 'mt_compounded_delay']],
        latent_period = orv_model_params$LP,
        infectious_period = orv_model_params$IP,
        scenario_campaign_duration = orv_model_inputs[[sc_result_row ,'mt_dur_constrained']], 
        location_id = orv_model_inputs[[sc_result_row, 'location_id']],
        vax_efficacy = orv_model_params$vaccine_efficacy,
        scenario_coverage = orv_model_inputs[[sc_result_row, 'mt_cov']],
        browse = F
    ) 
    orv_far_dynamics <- rbind(orv_far_dynamics, far_orv_sim)
}

saveRDS(orv_far_dynamics, './model_output/ottar_seir_model_output/orv_far_dynamics.rds')

#Plot of transmission dynamics for far pops ---
orv_far_dynamics_plot <- ggplot(data = orv_far_dynamics) + 
    geom_line(aes(x = time, 
                  y = S
                  ), 
              color = 'blue', 
              size = 1
              ) + 
    geom_line(aes(x = time, 
                  y = I), 
              color = 'red', 
              size = 1) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals', 
         title = 'Transmission dynamics following orv in far locations'
         ) + 
    facet_grid(strategy ~ mt_equip_type + location_id)

plot(orv_far_dynamics_plot)



#' Baseline: no orv ---

#' near locations ====
no_orv_near_dynamics <- tibble()

for (location in 1: nrow(site_pops_df)) {
    no_orv_near_sim <- run_orv_model(
        strategy = 'no_orv_near_pops',
        R0 = orv_model_params$near_pop_R0[21], # transmission coefficient
        max_time = orv_model_params$model_time, 
        target_pop_size = site_pops_df[[location, 'near_pop']],
        mt_equip_type = 'none',
        vax_day = Inf,
        latent_period = orv_model_params$LP,
        infectious_period = orv_model_params$IP,
        scenario_campaign_duration = 0, 
        location_id = site_pops_df[[location, 'location_id']],
        vax_efficacy = 0,
        scenario_coverage = 0,
        browse = F
    ) 
    no_orv_near_dynamics <- rbind(no_orv_near_dynamics, no_orv_near_sim)
}


#Plot of transmission dynamics for far pops (no orv) ---
no_orv_near_dynamics_plot <- ggplot(data = no_orv_near_dynamics) + 
    geom_line(aes(x = time, 
                  y = S
    ), 
    color = 'blue', 
    size = 2
    ) + 
    geom_line(aes(x = time, 
                  y = I), 
              color = 'red', 
              size = 2) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals', 
         title = 'Transmission dynamics of near locations (No ORV)'
    ) + 
    facet_grid( ~ location_id)

plot(no_orv_near_dynamics_plot)


#' far locations ====
no_orv_far_dynamics <- tibble()

for (location in 1: nrow(site_pops_df)) {
    no_orv_far_sim <- run_orv_model(
        strategy = 'no_orv_far_pops',
        R0 = orv_model_params$far_pop_R0[21], # transmission coefficient
        max_time = orv_model_params$model_time, 
        target_pop_size = site_pops_df[[location, 'far_pop']],
        mt_equip_type = 'none',
        vax_day = Inf,
        latent_period = orv_model_params$LP,
        infectious_period = orv_model_params$IP,
        scenario_campaign_duration = 0, 
        location_id = site_pops_df[[location, 'location_id']],
        vax_efficacy = 0,
        scenario_coverage = 0,
        browse = F
    ) 
    no_orv_far_dynamics <- rbind(no_orv_far_dynamics, no_orv_far_sim)
}


#Plot of transmission dynamics for far pops (no orv) ---
no_orv_far_dynamics_plot <- ggplot(data = no_orv_far_dynamics) + 
    geom_line(aes(x = time, 
                  y = S
    ), 
    color = 'blue', 
    size = 2
    ) + 
    geom_line(aes(x = time, 
                  y = I), 
              color = 'red', 
              size = 2) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals', 
         title = 'Transmission dynamics of far locations  (No ORV)'
    ) + 
    facet_grid( ~ location_id)

plot(no_orv_far_dynamics_plot)


#' Plots (Monodose fcc for illustration) ----

#' 1a. Outbreak size: by near locations ====
monodose_fcc_near_outbreak_size_plot <- orv_near_dynamics %>% 
    group_by(strategy, mt_equip_type) %>% 
  #  filter(time == orv_model_params$model_time) %>% 
    filter(time == orv_model_params$model_time, strategy == 'monodose_fcc') %>% 
    ggplot(aes(x = time, y = K)) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = 'Location', y = 'Outbreak size', title = 'Outbreak sizes by location and equipment used (Near Locations)') +
    facet_grid(strategy ~ mt_equip_type + location_id)

plot(monodose_fcc_near_outbreak_size_plot)

#' 1b. Incidence: by near locations
monodose_fcc_near_incidence_plot <- orv_near_dynamics %>% 
  group_by(strategy, mt_equip_type) %>% 
  #  filter(time == orv_model_params$model_time) %>% 
  filter(strategy == 'monodose_fcc') %>% 
  ggplot(aes(x = time, y = I)) + 
  geom_line(size = 2, color = 'red') + 
  labs(x = 'Time (Days)', y = 'Incidence', title = 'Monodose FCC incidence by location and equipment used (Near Locations)') +
  facet_grid(strategy ~ mt_equip_type + location_id)

plot(monodose_fcc_near_incidence_plot)

#' 2a. Outbreak size: by far locations
monodose_fcc_far_outbreak_size_plot <- orv_far_dynamics %>% 
    group_by(strategy, mt_equip_type) %>% 
  #  filter(time == orv_model_params$model_time) %>% 
    filter(time == orv_model_params$model_time, strategy == 'monodose_fcc') %>% 
    ggplot(aes(x = time, y = K)) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = 'Location', y = 'Outbreak size', title = 'Monodose FCC outbreak sizes per location and equipment used (Far Locations)') +
    facet_grid(strategy ~ mt_equip_type + location_id)

plot(monodose_fcc_far_outbreak_size_plot)


#' 2b. Incidence: by far locations
monodose_fcc_far_incidence_plot <- orv_far_dynamics %>% 
  group_by(strategy, mt_equip_type) %>% 
  #  filter(time == orv_model_params$model_time) %>% 
  filter(strategy == 'monodose_fcc') %>% 
  ggplot(aes(x = time, y = I)) + 
  geom_line(size = 2, color = 'red') + 
  labs(x = 'Time (Days)', y = 'Incidence', title = 'Monodose FCC incidence by location and equipment used (Far Locations)') +
  facet_grid(strategy ~ mt_equip_type + location_id)

plot(monodose_fcc_far_incidence_plot)


#' print the monodose data to support the explanation of the plots:
# library(xtable)
# monodose_sc_results <- orv_model_inputs %>% 
#   filter(strategy == 'monodose_fcc') %>% 
#   select(-c(near_pop, far_pop, ft_dur_constrained, mt_dur_constrained)) 
# 
#   print(xtable(monodose_sc_results), 
#       floating=FALSE, 
#       latex.environments = NULL, 
#       booktabs = TRUE
#       )



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
                                orv_far_dynamics_outbreak_size_df
                                ) %>% 
    mutate(orv_total_cases = orv_near_total_cases + orv_far_total_cases)

#' cases averted ====
#' 
cases_averted_df <- orv_outbreak_sizes_aggregated %>% 
    mutate(cases_averted = (no_orv_near_dynamics_outbreak_size + no_orv_far_dynamics_outbreak_size) - orv_total_cases)

cases_averted_df

saveRDS(cases_averted_df, './model_output/cases_averted_deterministic_seir.rds')



#' Combine all simulations into a FINAL data.frame for post-processing

#'load supply chain summary
#'
sc_results_summary_10_teams <- readRDS('./model_output/sc_results_summary_10_teams.rds') %>% 
    as_tibble() %>% 
    rename(mt_equip_type = mt_equip) %>% 
    mutate(strategy = as_factor(gsub(pattern = '_parallel', replacement = '', .$strategy))) #shorten the strategy names

sc_epi_analysis_summary_10_teams <- left_join(sc_results_summary_10_teams , 
                                              cases_averted_df, 
                                              by = c('strategy', 'mt_equip_type'
                                              )
                                              )

saveRDS(sc_epi_analysis_summary_10_teams, file = './model_output/sc_epi_analysis_summary_10_teams.rds')
View(sc_epi_analysis_summary_10_teams)
