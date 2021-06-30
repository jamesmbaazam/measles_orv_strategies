
#Supply chain analysis

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
source('./scripts/supply_chain_functions.R') #functions for performing individual supply chain calculations
source('./scripts/wrappers_supply_chain.R') #functions for running the supply chain model
source('./scripts/strategy_list_complete.R')
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/epidemics7_analysis/scenarios.R') 
source('./scripts/epidemics7_analysis/simulation_params.R')



# Campaign commencement delay ----

campaign_delay_results <- sim_params_table %>%
  rowwise() %>%
  do({
    with(
      .,
      analyse_prep_delay(
        strategy_name = strategy,
        fixed_team_with_dose10 = ft_with_dose10,
        fixed_team_with_ice = ft_with_ice,
        mobile_team_with_dose10 = mt_with_dose10,
        mobile_team_with_ice = mt_with_ice,
        team_dispatch = dispatch,
        site_details = data.frame(location_id = location_id, near_pop = near_pop, far_pop = far_pop),
        fixed_team_equip_type = "both",
        mobile_team_equip_type = equip_type,
        n_teams_fixed = 1,
        n_teams_mobile = 1,
        rcw25_ice_replacement_days = 2,
        mf314 = 1, 
        ambient_temperature = sc_model_params$ambient_temp[1], 
        dose10_vial_volume = sc_model_params$dose10_vial_vol[1], 
        monodose_vial_volume = sc_model_params$monodose_vial_vol[1], 
        res_type = 'detailed'
      )
    )
  })

## Remove some columns ==== 
campaign_delay_results_actual <- campaign_delay_results %>% 
    select(-c(near_pop, far_pop, ft_vial_type, 
              mt_vial_type, ft_doses_required, mt_doses_required, 
              ft_RCW25, mt_RCW25, ft_vaxCarr, 
              mt_vaxCarr, ft_icepacks_large, mt_icepacks_large, 
              ft_icepacks_small, mt_icepacks_small, team_leaving_first
              )
           )# %>% ungroup()



## Team days analysis ----


team_days_results <- sim_params_table %>%
    rowwise() %>%
    do({
        with(.,
             analyse_team_days(
                 strategy,
                 ft_with_dose10,
                 ft_with_ice,
                 mt_with_dose10,
                 mt_with_ice,
                 data.frame(location_id = location_id, near_pop = near_pop, far_pop = far_pop),
                 mobile_team_equip_type = equip_type
             )
        )
    })


team_days_results_actual <- team_days_results %>% 
    select(-c(ft_with_ice, mt_with_ice, ft_vial_type, mt_vial_type)) %>% 
  ungroup()


## Combine campaign delays and team days results ----
sc_analysis <- left_join(campaign_delay_results_actual, 
                          team_days_results_actual
                          ) %>% ungroup()


## Compounded delays ----

#' split-apply-combine
#' 
sc_results_full <- sc_analysis %>% 
    group_split(strategy, mt_equip_type) %>% 
    map_df(function(dat){mutate(dat, mt_compounded_delay = calc_compounded_delays(campaign_start, mt_team_days))}) 


#saveRDS(sc_results_full, file = './model_output/sc_results_full.rds')




