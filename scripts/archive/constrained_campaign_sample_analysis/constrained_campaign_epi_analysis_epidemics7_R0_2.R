
options(nwarnings = 10000) #print this many messages if they exist or occur

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
conflict_prefer('summarise', 'dplyr')

#helper functions and parameters
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/epidemics7_analysis/scenarios.R') 
source('./scripts/epidemics7_analysis/simulation_params.R')
source('./scripts/measles_functions.R')


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
    })

#View(sc_results_full)




###############################################################################
#Running the simulations for each strategy
###############################################################################

#' far/remote location ----

orv_far_pop_dynamics_detailed <- tibble()
orv_far_pop_dynamics_collapsed <- tibble()
orv_far_pop_dynamics_epi_total <- tibble()


for (sc_result_row in 1: nrow(sc_results_full)) {
    tp <- sc_model_params$vax_rate[['mobile_team']] #if all mobile teams can exceed the doses needed to achieve the average daily performance, then it's safe to assume all teams can hit the average team performance
    orv_far_pop_dynamics <- runSimulations(
        R0 = orv_model_params$far_pop_R0[1], # transmission coefficient
        run_time = orv_model_params$model_time, 
        pop = initializePop(N = sc_results_full[[sc_result_row, 'far_pop']], 
                            init_prop_immune = 0, 
                            I0 = 1),
        strategy_name = sc_results_full[[sc_result_row, 'strategy']],
        mt_equip = sc_results_full[[sc_result_row, 'mt_equip_type']],
        vaxDay = sc_results_full[[sc_result_row, 'mt_compounded_delay']],
        orv_duration = sc_results_full[[sc_result_row ,'mt_dur_constrained']], 
        n_team_type = teams$n_mt[1],
        site = sc_results_full[[sc_result_row, 'location_id']],
        vax_eff = orv_model_params$vaccine_efficacy,
        team_performance = tp,
        time_to_immunity = orv_model_params$immune_response_timing,
        browse = F
    ) 
    
    detailed_with_loc <- mutate(orv_far_pop_dynamics$Detailed,
                                location_id = as_factor(rep(orv_far_pop_dynamics$location_id, 
                                                            times = nrow(orv_far_pop_dynamics$Detailed))),
                                mt_equip_type = as_factor(rep(orv_far_pop_dynamics$mt_equip_type,
                                                              times = nrow(orv_far_pop_dynamics$Detailed)))
    )
    
    orv_far_pop_dynamics_detailed <- rbind(orv_far_pop_dynamics_detailed, 
                                           detailed_with_loc
    )
    
    collapsed_with_loc <- mutate(orv_far_pop_dynamics$Collapsed,
                                 location_id = as_factor(rep(orv_far_pop_dynamics$location_id, 
                                                             times = nrow(orv_far_pop_dynamics$Collapsed))), 
                                 mt_equip_type = as_factor(rep(orv_far_pop_dynamics$mt_equip_type, 
                                                               times = nrow(orv_far_pop_dynamics$Collapsed)))
    )
    
    orv_far_pop_dynamics_collapsed <- rbind(orv_far_pop_dynamics_collapsed, 
                                            collapsed_with_loc
    )
    
    orv_far_pop_dynamics_epi_total <- rbind(orv_far_pop_dynamics_epi_total, 
                                            data.frame(far_pop_epi_total = orv_far_pop_dynamics$epiTotal,
                                                       location_id = as_factor(orv_far_pop_dynamics$location_id),
                                                       mt_equip_type = as_factor(orv_far_pop_dynamics$mt_equip_type)
                                            )
    )
    
}

#save output
#save output
saveRDS(orv_far_pop_dynamics_detailed, file = './model_output/orv_far_pop_dynamics_detailed_R0_2.rds')
saveRDS(orv_far_pop_dynamics_collapsed, file = './model_output/orv_far_pop_dynamics_collapsed_R0_2.rds') 
saveRDS(orv_far_pop_dynamics_epi_total, file = './model_output/orv_far_pop_dynamics_epi_total_R0_2.rds') 



#' near/urban locations ----

orv_near_pop_dynamics_detailed <- tibble()
orv_near_pop_dynamics_collapsed <- tibble()
orv_near_pop_dynamics_epi_total <- tibble()

for (sc_result_row in 1: nrow(sc_results_full)) {
    tp <- sc_model_params$vax_rate[['fixed_team']] 
    orv_near_pop_dynamics <- runSimulations(
        R0 = orv_model_params$near_pop_R0[1], # transmission coeficient
        run_time = orv_model_params$model_time, # 1 yr!
        pop = initializePop(N = sc_results_full[[sc_result_row, 'near_pop']], 
                            init_prop_immune = 0, 
                            I0 = 1
        ),
        strategy_name = sc_results_full[[sc_result_row, 'strategy']],
        mt_equip = sc_results_full[[sc_result_row, 'mt_equip_type']],
        vaxDay = sc_results_full[[sc_result_row, 'ft_compounded_delay']],
        orv_duration = sc_results_full[[sc_result_row ,'ft_dur_constrained']],
        n_team_type = teams$n_ft[1],
        site = sc_results_full[[sc_result_row, 'location_id']],
        vax_eff = orv_model_params$vaccine_efficacy,
        team_performance = tp,
        time_to_immunity = orv_model_params$immune_response_timing,
        browse = F
    ) 
    
    detailed_with_loc <- mutate(orv_near_pop_dynamics$Detailed,
                                location_id = as_factor(rep(orv_near_pop_dynamics$location_id, 
                                                            times = nrow(orv_near_pop_dynamics$Detailed))),
                                mt_equip_type = as_factor(rep(orv_near_pop_dynamics$mt_equip_type,
                                                              times = nrow(orv_near_pop_dynamics$Detailed)))
    )
    
    orv_near_pop_dynamics_detailed <- rbind(orv_near_pop_dynamics_detailed, 
                                            detailed_with_loc
    )
    
    collapsed_with_loc <- mutate(orv_near_pop_dynamics$Collapsed,
                                 location_id = as_factor(rep(orv_near_pop_dynamics$location_id, 
                                                             times = nrow(orv_near_pop_dynamics$Collapsed))), 
                                 mt_equip_type = as_factor(rep(orv_near_pop_dynamics$mt_equip_type, 
                                                               times = nrow(orv_near_pop_dynamics$Collapsed)))
    )
    
    orv_near_pop_dynamics_collapsed <- rbind(orv_near_pop_dynamics_collapsed, 
                                             collapsed_with_loc
    )
    
    orv_near_pop_dynamics_epi_total <- rbind(orv_near_pop_dynamics_epi_total, 
                                             data.frame(near_pop_epi_total = orv_near_pop_dynamics$epiTotal,
                                                        location_id = orv_near_pop_dynamics$location_id,
                                                        mt_equip_type = orv_near_pop_dynamics$mt_equip_type
                                             )
    )
}

#save output
saveRDS(orv_near_pop_dynamics_detailed, file = './model_output/orv_near_pop_dynamics_detailed_R0_2.rds')
saveRDS(orv_near_pop_dynamics_collapsed, file = './model_output/orv_near_pop_dynamics_collapsed_R0_2.rds') 
saveRDS(orv_near_pop_dynamics_epi_total, file = './model_output/orv_near_pop_dynamics_epi_total_R0_2.rds') 





#' baseline near/urban location - No vaccination ----
#' 
#' sim <- runSimulations(
# R0 = orv_model_params$near_pop_R0, 
# run_time = orv_model_params$model_time, # 1 yr!
# pop = initializePop(N = as.numeric(site_pops_df[1, 'near_pop']), 
#                     init_prop_immune = 0.25, I0 = 1
# ),
# strategy_name = 'no_vax_baseline',
# vaxDay = 1,
# orv_duration = 0,
# n_team_type = 1,
# vax_eff = 0,
# team_performance = 0,
# time_to_immunity = 0,
# browse = F
# ) 
#' 
#' ggplot(data = sim$Collapsed %>% filter(time <= 100)) + 
#' geom_line(aes(x = time, y = totalSus), color = 'blue', size = 2) + 
#' geom_line(data = sim$Collapsed %>% filter(time <= 100), 
#' aes(x = time, y = totalInf), color = 'green', size = 2) + 
#' geom_line(data = sim$Collapsed %>% filter(time <= 100), 
#' aes(x = time, y = totalRec), color = 'red', size = 2)

no_vax_near_dynamics_detailed <- tibble()
no_vax_near_dynamics_collapsed <- tibble()
no_vax_near_dynamics_epi_total <- tibble()

for (site_row in 1: nrow(site_pops_df)) {
    no_vax_near_dynamics <- runSimulations(
        R0 = orv_model_params$near_pop_R0[1], 
        run_time = orv_model_params$model_time, # 1 yr!
        pop = initializePop(N = site_pops_df[[site_row, 'near_pop']], 
                            init_prop_immune = 0, 
                            I0 = 1
        ),
        strategy_name = 'no_vax_baseline',
        mt_equip = 'none',
        vaxDay = 1, #' NB: this is a trick to bypass some conditions but it does no harm to the output
        orv_duration = 1, #' NB: this is a trick to bypass some conditions but it does no harm to the output
        n_team_type = 1, #' NB: this is a trick to bypass some conditions but it does no harm to the output
        site = sc_results_full[[site_row, 'location_id']],
        vax_eff = 0,
        team_performance = 0,
        time_to_immunity = 0,
        browse = F
    ) 
    
    detailed_with_loc <- mutate(no_vax_near_dynamics$Detailed,
                                location_id = as_factor(rep(no_vax_near_dynamics$location_id, 
                                                            times = nrow(no_vax_near_dynamics$Detailed))),
                                mt_equip_type = as_factor(rep(no_vax_near_dynamics$mt_equip_type,
                                                              times = nrow(no_vax_near_dynamics$Detailed)))
    )
    
    no_vax_near_dynamics_detailed <- rbind(no_vax_near_dynamics_detailed, 
                                           detailed_with_loc
    )
    
    collapsed_with_loc <- mutate(no_vax_near_dynamics$Collapsed,
                                 location_id = as_factor(rep(no_vax_near_dynamics$location_id, 
                                                             times = nrow(no_vax_near_dynamics$Collapsed))), 
                                 mt_equip_type = as_factor(rep(no_vax_near_dynamics$mt_equip_type, 
                                                               times = nrow(no_vax_near_dynamics$Collapsed)))
    )
    
    no_vax_near_dynamics_collapsed <- rbind(no_vax_near_dynamics_collapsed, 
                                            collapsed_with_loc)
    
    no_vax_near_dynamics_epi_total <- rbind(no_vax_near_dynamics_epi_total, 
                                            data.frame(near_pop_epi_total = no_vax_near_dynamics$epiTotal,
                                                       location_id = as_factor(no_vax_near_dynamics$location_id),
                                                       mt_equip_type = as_factor(no_vax_near_dynamics$mt_equip_type)
                                            )
    )
}

#save output
saveRDS(no_vax_near_dynamics_detailed, file = './model_output/no_vax_near_dynamics_detailed_R0_2.rds')
saveRDS(no_vax_near_dynamics_collapsed, file = './model_output/no_vax_near_dynamics_collapsed_R0_2.rds')
saveRDS(no_vax_near_dynamics_epi_total, file = './model_output/no_vax_near_dynamics_epi_total')






#' baseline far/remote location - No vaccination ----

no_vax_far_dynamics_detailed <- tibble()
no_vax_far_dynamics_collapsed <- tibble()
no_vax_far_dynamics_epi_total <- tibble()

for (site_row in 1: nrow(site_pops_df)) {
    no_vax_far_dynamics <- runSimulations(
        R0 = orv_model_params$far_pop_R0[1], 
        run_time = orv_model_params$model_time, # 1 yr!
        pop = initializePop(N = site_pops_df[[site_row, 'far_pop']], 
                            init_prop_immune = 0, 
                            I0 = 1
        ),
        strategy_name = 'no_vax_baseline',
        mt_equip = 'none',
        vaxDay = 1, #' NB: this is a trick to bypass some conditions but it does no harm to the output
        orv_duration = 1, #' NB: this is a trick to bypass some conditions but it does no harm to the output
        n_team_type = 1, #' NB: this is a trick to bypass some conditions but it does no harm to the output
        site = sc_results_full[[site_row, 'location_id']],
        vax_eff = 0,
        team_performance = 0,
        time_to_immunity = 0,
        browse = F
    ) 
    
    detailed_with_loc <- mutate(no_vax_far_dynamics$Detailed,
                                location_id = as_factor(rep(no_vax_far_dynamics$location_id, 
                                                            times = nrow(no_vax_far_dynamics$Detailed))),
                                mt_equip_type = as_factor(rep(no_vax_far_dynamics$mt_equip_type,
                                                              times = nrow(no_vax_far_dynamics$Detailed)))
    )
    
    no_vax_far_dynamics_detailed <- rbind(no_vax_far_dynamics_detailed, 
                                          detailed_with_loc
    )
    
    collapsed_with_loc <- mutate(no_vax_far_dynamics$Collapsed,
                                 location_id = as_factor(rep(no_vax_far_dynamics$location_id, 
                                                             times = nrow(no_vax_far_dynamics$Collapsed))), 
                                 mt_equip_type = as_factor(rep(no_vax_far_dynamics$mt_equip_type, 
                                                               times = nrow(no_vax_far_dynamics$Collapsed)))
    )
    
    no_vax_far_dynamics_collapsed <- rbind(no_vax_far_dynamics_collapsed, 
                                           collapsed_with_loc
    )
    
    no_vax_far_dynamics_epi_total <- rbind(no_vax_far_dynamics_epi_total, 
                                           data.frame(far_pop_epi_total = no_vax_far_dynamics$epiTotal,
                                                      location_id = as_factor(no_vax_far_dynamics$location_id),
                                                      mt_equip_type = as_factor(no_vax_far_dynamics$mt_equip_type)
                                           )
    )
}

#save output
saveRDS(no_vax_far_dynamics_detailed, file = './model_output/no_vax_far_dynamics_detailed_R0_2.rds')
saveRDS(no_vax_far_dynamics_collapsed, file = './model_output/no_vax_far_dynamics_collapsed_R0_2.rds')
saveRDS(no_vax_far_dynamics_epi_total, file = './model_output/no_vax_far_dynamics_epi_total')





#' Combine the no vaccination counterfactual total cases from the near and far 
#' simulations from the 5 sites into 1 data frame
no_vax_site_epi_total <- left_join(no_vax_near_dynamics_epi_total, 
                                   no_vax_far_dynamics_epi_total) %>%
    mutate(strategy = as_factor(rep('no_vax_baseline', 
                                    times = nrow(no_vax_near_dynamics_epi_total)
    )),
    no_vax_site_total_cases = near_pop_epi_total + far_pop_epi_total) 


saveRDS(no_vax_site_epi_total, file = './model_output/no_vax_site_epi_total_R0_2.rds')

#' total cases across all sites - no vaccination counterfactual
no_vax_outbreak_size <- no_vax_site_epi_total %>% 
    summarise(no_vax_total_cases = sum(no_vax_site_total_cases)) %>% 
    as.numeric()

no_vax_outbreak_size

#' Combine all simulations into a FINAL data.frame for post-processing

full_analysis_10_teams <- bind_cols(sc_results_full,
                                    orv_far_pop_dynamics_epi_total %>% 
                                        select(-c(location_id, mt_equip_type)),
                                    orv_near_pop_dynamics_epi_total %>% 
                                        select(-c(location_id, mt_equip_type))
)


View(full_analysis_10_teams %>% 
         select(-c(ft_freezing_time, 
                   mt_freezing_time, 
                   near_pop, 
                   far_pop)
         )
     #      %>% filter(location_id == 1)
)


saveRDS(full_analysis_10_teams, file = './model_output/full_analysis_10_teams_R0_2.rds')

#' split the analysis by mobile team equipment scenario within and between each
#' strategy


epi_results_summary_10_teams <- full_analysis_10_teams %>% 
    group_by(strategy, mt_equip_type) %>% 
    summarise(near_pop_total_cases = sum(near_pop_epi_total),
              far_pop_total_cases = sum(far_pop_epi_total)
    ) %>% 
    ungroup() %>% 
    mutate(site_total_cases = near_pop_total_cases + far_pop_total_cases,
           cases_averted = no_vax_outbreak_size - site_total_cases)

#View(epi_results_summary_10_teams)


saveRDS(epi_results_summary_10_teams, file = './model_output/epi_results_summary_10_teams_R0_2.rds')


#' Summary of supply chain and epi analysis

#'load supply chain summary
#'
sc_results_summary_10_teams <- readRDS('./model_output/sc_results_summary_10_teams.rds')

sc_epi_analysis_summary_10_teams <- left_join(sc_results_summary_10_teams %>% rename(mt_equip_type = mt_equip), 
                                              epi_results_summary_10_teams, 
                                              by = c('strategy', 'mt_equip_type'
                                              )
)

saveRDS(sc_epi_analysis_summary_10_teams, file = './model_output/sc_epi_analysis_summary_10_teams_R0_2.rds')
View(sc_epi_analysis_summary_10_teams)

#' to do: 1. find the difference within each strategy between the epi totals and 
#' the baseline then sum it up at after the differencing, OR 2. sum up the epi 
#' totals across all locations within all strategies and find the difference between
#' the result and the total of the corresponding total cases across all locations
#' of the baseline
