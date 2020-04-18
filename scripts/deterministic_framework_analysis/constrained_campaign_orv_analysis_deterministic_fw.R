#global controls
options(nwarnings = 10000) #print this many messages if they exist or occur

#helper functions and parameters
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/deterministic_framework_analysis/scenarios.R') 
source('./scripts/deterministic_framework_analysis/simulation_params.R')
source('./scripts/measles_deterministic_seir_model.R')

## Supply chain data ----
sc_results <- readRDS('./model_output/deterministic_framework_analysis_output/sc_analysis_full_10_teams.rds')

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


################################################################################
#' ORV Scenarios
################################################################################

#' near locations' dynamics following orv ====
orv_near_dynamics <- tibble()

for (sc_result_row in 1: nrow(orv_model_inputs)) {
    near_orv_sim <- run_orv_model(
        strategy = orv_model_inputs[[sc_result_row, 'strategy']],
        R0 = orv_model_params$near_pop_R0[21], # transmission coefficient
        I0 = 1,
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

#' save the results to file
saveRDS(orv_near_dynamics, 
        './model_output/deterministic_framework_analysis_output/orv_near_dynamics_proportions.rds'
        )



#' far locations' dynamics following orv ====
orv_far_dynamics <- tibble()

for (sc_result_row in 1: nrow(orv_model_inputs)) {
  far_orv_sim <- run_orv_model(
    strategy = orv_model_inputs[[sc_result_row, 'strategy']],
    R0 = orv_model_params$far_pop_R0[21], # transmission coefficient
    I0 = 1,
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

#' save the results to file
saveRDS(orv_far_dynamics, 
        './model_output/deterministic_framework_analysis_output/orv_far_dynamics_proportions.rds'
        )



################################################################################
#' BASELINE: no orv ---
################################################################################

#' near locations ====
no_orv_near_dynamics <- tibble()

for (location in 1: nrow(site_pops_df)) {
    no_orv_near_sim <- run_orv_model(
        strategy = 'no_orv_near_pops',
        R0 = orv_model_params$near_pop_R0[21], # transmission coefficient
        I0 = 1,
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

#' save the results to file
saveRDS(no_orv_near_dynamics, 
        './model_output/deterministic_framework_analysis_output/no_orv_near_dynamics_proportions.rds'
        )




#' far locations ====
no_orv_far_dynamics <- tibble()

for (location in 1: nrow(site_pops_df)) {
    no_orv_far_sim <- run_orv_model(
        strategy = 'no_orv_far_pops',
        R0 = orv_model_params$far_pop_R0[21], # transmission coefficient
        I0 = 1,
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

#' save the results to file
saveRDS(no_orv_far_dynamics, 
        './model_output/deterministic_framework_analysis_output/no_orv_far_dynamics_proportions.rds'
        )

